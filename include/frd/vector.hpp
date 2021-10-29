#pragma once

#include <initializer_list>

#include <frd/limits.hpp>
#include <frd/memory.hpp>
#include <frd/ranges.hpp>
#include <frd/algorithm.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /*
        TODO: Decide whether to do vector<bool> specialization.

        NOTE: We currently do not have strong exception guarantees.
    */

    template<object Element, allocator_for<Element> Allocator = allocator<Element>>
    requires (copy_assignable<Element> && copy_constructible<Element>)
    class vector {
        public:
            using _allocator_traits = allocator_traits<Allocator>;

            using allocator_type  = Allocator;
            using value_type      = Element;
            using reference       = Element &;
            using const_reference = const Element &;
            using pointer         = typename _allocator_traits::pointer;
            using const_pointer   = typename _allocator_traits::const_pointer;

            using iterator        = pointer;
            using const_iterator  = const_pointer;
            using difference_type = iter_difference<iterator>;
            using size_type       = make_unsigned<difference_type>;

            using reverse_iterator       = frd::reverse_iterator<iterator>;
            using const_reverse_iterator = frd::reverse_iterator<const_iterator>;

            static constexpr size_type IterDifferenceMax = numeric_limits<difference_type>::max;

            static constexpr size_type NewCapacityRatio = 2;

            [[no_unique_address]] Allocator _allocator = Allocator();

            pointer   _data     = nullptr;
            size_type _size     = 0;
            size_type _capacity = 0;

            constexpr vector() requires (default_initializable<Allocator>) = default;

            constexpr explicit vector(const Allocator &alloc) noexcept : _allocator(alloc) { }

            constexpr vector(const size_type size, const Element &value, const Allocator &alloc = Allocator())
            requires (
                allocator_value_constructible_from<Allocator, const Element &>
            ) :
                _allocator(alloc),
                _data(
                    [&]() -> pointer {
                        if (size > 0) {
                            return _allocator_traits::allocate(this->_allocator, size);
                        }

                        return nullptr;
                    }()
                ),
                _size(size),
                _capacity(size)
            {
                for (const auto i : interval(size)) {
                    _allocator_traits::construct(this->_allocator, this->_data + i, value);
                }
            }

            constexpr explicit vector(const size_type size, const Allocator &alloc = Allocator())
            requires (
                allocator_value_default_constructible<Allocator>
            ) :
                _allocator(alloc),
                _data(
                    [&]() -> pointer {
                        if (size > 0) {
                            return _allocator_traits::allocate(this->_allocator, size);
                        }

                        return nullptr;
                    }()
                ),
                _size(size),
                _capacity(size)
            {
                for (const auto i : interval(size)) {
                    _allocator_traits::construct(this->_allocator, this->_data + i);
                }
            }

            template<input_range R>
            requires (
                /* Stops ambiguity with copy/move constructors. */
                !same_as<remove_cvref<R>, vector> &&

                same_as<remove_cvref<range_reference<R>>, Element>                &&
                allocator_value_constructible_from<Allocator, range_reference<R>>
            )
            constexpr explicit vector(R &&r, const Allocator &alloc = Allocator()) : _allocator(alloc) {
                /* TODO: Potentially allocate data with member initializers? */
                this->insert(this->begin(), frd::forward<R>(r));
            }

            constexpr explicit vector(const std::initializer_list<Element> list, const Allocator &alloc = Allocator()) : _allocator(alloc) {
                this->insert(this->begin(), list);
            }

            constexpr ~vector() {
                this->_cleanup_data();
            }

            /* NOTE: This does not reset the capacity. */
            constexpr void _free_data() {
                if (this->_capacity > 0) {
                    _allocator_traits::deallocate(this->_allocator, this->_data, this->_capacity);
                }
            }

            /* NOTE: This does not reset the size or capacity. */
            constexpr void _cleanup_data() {
                for (const auto location : interval(this->_data, this->_data + this->_size)) {
                    _allocator_traits::destroy(this->_allocator, location);
                }

                this->_free_data();
            }

            constexpr void _reserve_and_destroy(const size_type new_capacity) {
                this->_cleanup_data();

                this->_data     = _allocator_traits::allocate(this->_allocator, new_capacity);
                this->_capacity = new_capacity;
            }

            constexpr void assign(const size_type size, const Element &value)
            requires (
                allocator_value_constructible_from<Allocator, const Element &>
            ) {
                /* If 'size' is more than our capacity, we need to allocate more memory. */
                if (size > this->_capacity) {
                    this->_reserve_and_destroy(size);

                    for (const auto location : interval(this->_data, this->_data + this->_size)) {
                        _allocator_traits::construct(this->_allocator, location, value);
                    }

                    this->_size = size;
                } else {
                    for (const auto i : interval(size)) {
                        const auto location = this->_data + i;

                        /* If we've reached uninitialized memory, construct objects there, else assign. */
                        if (i >= this->_size) {
                            _allocator_traits::construct(this->_allocator, location, value);
                        } else {
                            *location = value;
                        }
                    }

                    /* Destroy any dangling objects. */
                    if (size < this->_size) {
                        for (const auto location : interval(this->_data + size, this->_data + this->_size)) {
                            _allocator_traits::destroy(this->_allocator, location);
                        }
                    }

                    this->_size = size;
                }
            }

            constexpr iterator _to_mutable_iterator(const const_iterator it) {
                const auto begin = this->begin();

                return (begin + (it - begin));
            }

            constexpr void _reserve_and_move(const size_type new_capacity) {
                frd::precondition(new_capacity <= this->max_size(), "Cannot reserve enough data!");
                frd::precondition(new_capacity >= this->_size,      "Cannot reserve less than current size!");

                const auto new_data = _allocator_traits::allocate(this->_allocator, new_capacity);

                /* Move each element into its new location. */
                auto new_location = new_data;
                for (auto &elem : *this) {
                    _allocator_traits::construct(this->_allocator, new_location, frd::move(elem));
                    _allocator_traits::destroy  (this->_allocator, std::addressof(elem));

                    new_location++;
                }

                this->_free_data();

                this->_data     = new_data;
                this->_capacity = new_capacity;
            }

            constexpr void reserve(size_type new_capacity) {
                /* If we can already store more than requested, do nothing. */
                if (new_capacity <= this->_capacity) {
                    return;
                }

                this->_reserve_and_move(new_capacity);
            }

            template<typename... Args>
            requires (allocator_value_constructible_from<Allocator, Args...>)
            constexpr reference emplace_back(Args &&... args) {
                /*
                    We could call 'this->emplace(this->end(), ...)', but that would lead
                    to slightly suboptimal performance, and this code is easier to understand.
                */

                /* We call '_reserve_and_move' to avoid checks of current capacity. */
                if (this->_capacity == 0) {
                    this->_reserve_and_move(1);
                } else if (this->_size + 1 > this->_capacity) {
                    this->_reserve_and_move(NewCapacityRatio * this->_capacity);
                }

                const auto location = this->_data + this->_size;
                _allocator_traits::construct(this->_allocator, location, frd::forward<Args>(args)...);
                this->_size++;

                return *location;
            }

            template<forwarder_for<Element> ElementFwd>
            constexpr void push_back(ElementFwd &&obj) {
                this->emplace_back(frd::forward<ElementFwd>(obj));
            }

            constexpr void _reserve_for_insertion(const size_type new_capacity, const const_iterator pos_to_insert, const size_type insert_size) {
                /* Do our own reserve so we don't move our elements more than we need to. */

                const auto new_data = _allocator_traits::allocate(this->_allocator, new_capacity);

                auto new_location = new_data;
                for (const auto elem_it : interval(this->begin(), this->end())) {
                    /* Don't move elements into the place we're going to insert new ones. */
                    if (elem_it == pos_to_insert) {
                        new_location += insert_size;
                    }

                    _allocator_traits::construct(this->_allocator, new_location, frd::iter_move(elem_it));
                    _allocator_traits::destroy  (this->_allocator, pointer{elem_it});

                    new_location++;
                }

                this->_free_data();

                this->_data     = new_data;
                this->_capacity = new_capacity;
            }

            constexpr void _shift_elements_forward(const iterator begin_shift, const size_type shift_size) {
                /*
                    Loop starting from the end so we can just move each element over
                    without overwriting any other element.
                */
                for (const auto elem_it : frd::interval(begin_shift, this->end()) | views::reverse) {
                    const auto elem_addr = pointer{elem_it};

                    _allocator_traits::construct(this->_allocator, elem_addr + shift_size, frd::iter_move(elem_it));

                    /*
                        TODO: Do we need this 'destroy' call or should we just use
                        the assignment operator later instead of constructing?
                    */
                    _allocator_traits::destroy(this->_allocator, elem_addr);
                }
            }

            [[nodiscard]]
            constexpr iterator _make_space_for_insertion(const const_iterator pos, const size_type insert_size) {
                frd::precondition(pos >= this->begin() && pos <= this->end(), "Invalid iterator for insertion!");

                const auto insert_offset = pos - this->begin();

                if (this->_capacity == 0) {
                    /* Call guarded 'reserve' to ensure we don't allocate a zero-length array. */

                    this->reserve(insert_size);
                } else if (this->_size + insert_size > this->_capacity) {
                    /* The normal exponential reallocation could still be too small for the insertion. */
                    const auto new_capacity = frd::max(NewCapacityRatio * this->_capacity, this->_size + insert_size);

                    this->_reserve_for_insertion(new_capacity, pos, insert_size);
                } else if (!this->empty()) {
                    /*
                        We don't need to move anything if we're empty.

                        NOTE: We don't *need* to check if we're empty, as
                        '_shift_elements_forward' will do nothing if the
                        vector is empty, but I find explicitly checking it
                        to be more expressive, and saves us from the extra
                        logic that '_shift_elements_forward' would do.
                    */

                    this->_shift_elements_forward(this->_to_mutable_iterator(pos), insert_size);
                }

                /* Need to get new iterator as iterators may be invalidated. */
                return this->begin() + insert_offset;
            }

            template<typename... Args>
            requires (allocator_value_constructible_from<Allocator, Args...>)
            constexpr iterator emplace(const const_iterator pos, Args &&... args) {
                frd::precondition(pos >= this->begin() && pos <= this->end(), "Invalid iterator for insertion!");

                const auto insert_it = this->_make_space_for_insertion(pos, 1);

                /* Emplace the element into the empty location we have made for it. */
                _allocator_traits::construct(this->_allocator, pointer{insert_it}, frd::forward<Args>(args)...);
                this->_size++;

                return insert_it;
            }

            template<forwarder_for<Element> ElementFwd>
            constexpr iterator insert(const const_iterator pos, ElementFwd &&obj) {
                return this->emplace(pos, frd::forward<ElementFwd>(obj));
            }

            constexpr iterator insert(const const_iterator pos, const size_type count, const Element &value) {
                const auto insert_it = this->_make_space_for_insertion(pos, count);

                /* Copy the value into the spaces we have made for it. */
                for (const auto i : interval(count)) {
                    _allocator_traits::construct(this->_allocator, pointer{insert_it + i}, value);
                }

                this->_size += count;

                return insert_it;
            }

            /*
                We need this bit of indirection to allow brace-enclosed initializer
                lists to be deduced to 'std::initializer_list', as the compiler will
                not know it can use 'std::initializer_list' for 'R' and to avoid
                code duplication.
            */
            template<typename R>
            /* Requirements checked by callers. */
            constexpr iterator _insert_range(const const_iterator pos, R &&insert_rng) {
                /*
                    If 'R' is a sized range, then we can make sure we have enough space
                    before inserting. Otherwise insert each element individually.
                */
                if constexpr (sized_range<R>) {
                    const auto insert_size = frd::size(insert_rng);
                    const auto insert_it   = this->_make_space_for_insertion(pos, insert_size);

                    /* Insert the elements of the range into the empty locations we have made for it. */
                    auto insert_location = pointer{insert_it};
                    for (auto &&to_insert : insert_rng) {
                        _allocator_traits::construct(this->_allocator, insert_location, frd::forward<decltype(to_insert)>(to_insert));

                        insert_location++;
                    }

                    this->_size += insert_size;

                    return insert_it;
                } else {
                    size_type insert_offset = pos - this->begin();

                    for (auto &&to_insert : frd::forward<R>(insert_rng)) {
                        this->insert(this->begin() + insert_offset, frd::forward<decltype(to_insert)>(to_insert));

                        insert_offset++;
                    }

                    return this->begin() + insert_offset;
                }
            }

            constexpr iterator insert(const const_iterator pos, const std::initializer_list<Element> list)
            requires (
                allocator_value_constructible_from<Allocator, const Element &>
            ) {
                return this->_insert_range(pos, list);
            }

            template<input_range R>
            requires (
                /*
                    Stops ambiguity between single element overload and this one.

                    This is needed for the very slim edge case where our element
                    type is a range for itself, which is indeed possible.
                */
                !forwarder_for<R, Element> &&

                same_as<remove_cvref<range_reference<R>>, Element>                &&
                allocator_value_constructible_from<Allocator, range_reference<R>>
            )
            constexpr iterator insert(const const_iterator pos, R &&insert_rng) {
                return this->_insert_range(pos, frd::forward<R>(insert_rng));
            }

            constexpr void pop_back() {
                frd::precondition(this->_size > 0, "Cannot perform pop_back on a vector of 0 elements!");

                _allocator_traits::destroy(this->_allocator, this->_data + this->_size - 1);

                this->_size--;
            }

            constexpr void _shift_elements_backward(const iterator begin_shift, const difference_type shift_size) {
                for (const auto elem_it : interval(begin_shift, this->end())) {
                    const auto to_shift_into = elem_it - shift_size;

                    *to_shift_into = frd::iter_move(elem_it);
                }
            }

            constexpr iterator erase(const const_iterator pos) {
                frd::precondition(pos >= this->begin() && pos < this->end(), "Invalid iterator for erasing!");

                const auto mutable_pos = this->_to_mutable_iterator(pos);

                this->_shift_elements_backward(mutable_pos + 1, 1);
                _allocator_traits::destroy(this->_allocator, pointer{frd::prev(this->end())});

                this->_size--;

                return mutable_pos;
            }

            constexpr iterator erase(const const_iterator start, const const_iterator bound) {
                frd::precondition(start == bound || (start >= this->begin() && start < this->end()), "Invalid iterator for erasing!");
                frd::precondition(bound >= start && bound <= this->end(), "Invalid bound for erasing!");

                if (start == bound) {
                    return this->_to_mutable_iterator(bound);
                }

                const auto erase_size    = bound - start;
                const auto mutable_start = this->_to_mutable_iterator(start);
                const auto mutable_bound = this->_to_mutable_iterator(bound);

                this->_shift_elements_backward(mutable_bound, erase_size);

                for (const auto elem_it : interval(this->end() - erase_size, this->end())) {
                    _allocator_traits::destroy(this->_allocator, pointer{elem_it});
                }

                this->_size -= erase_size;

                return mutable_start;
            }

            constexpr void shrink_to_fit() {
                /* We don't need to reallocate if the size and capacity already match. */
                if (this->_size == this->_capacity) {
                    return;
                }

                this->_reserve_and_move(this->_size);
            }

            /*
                Allows shrinking size without passing an extra value when
                'Element' is not default constructible.
            */
            constexpr void shrink_size(const size_type new_size) {
                if (new_size >= this->_size) {
                    return;
                }

                for (const auto i : interval(new_size, this->_size)) {
                    _allocator_traits::destroy(this->_allocator, this->_data + i);
                }

                this->_size = new_size;
            }

            /*
                NOTE: The standard marks this as 'noexcept' despite that it calls
                functions which have no guarantees about whether they throw.
            */
            constexpr void clear() noexcept {
                this->shrink_size(0);
            }

            template<typename... Args>
            constexpr void _resize_with_constructor_args(const size_type new_size, Args &&... args) {
                if (new_size < this->_size) {
                    this->shrink_size(new_size);
                } else {
                    this->reserve(new_size);

                    for (const auto i : interval(this->_size, new_size)) {
                        _allocator_traits::construct(this->_allocator, this->_data + i, frd::forward<Args>(args)...);
                    }

                    this->_size = new_size;
                }
            }

            constexpr void resize(const size_type new_size)
            requires (
                allocator_value_default_constructible<Allocator>
            ) {
                this->_resize_with_constructor_args(new_size);
            }

            constexpr void resize(const size_type new_size, const Element &value)
            requires (
                allocator_value_constructible_from<Allocator, const Element &>
            ) {
                this->_resize_with_constructor_args(new_size, value);
            }

            [[nodiscard]]
            constexpr reference front() noexcept {
                return *this->_data;
            }

            [[nodiscard]]
            constexpr const_reference front() const noexcept {
                return *this->_data;
            }

            [[nodiscard]]
            constexpr reference back() noexcept {
                return *(this->_data + this->_size - 1);
            }

            [[nodiscard]]
            constexpr const_reference back() const noexcept {
                return *(this->_data + this->_size - 1);
            }

            [[nodiscard]]
            constexpr pointer data() noexcept {
                return this->_data;
            }

            [[nodiscard]]
            constexpr const_pointer data() const noexcept {
                return this->_data;
            }

            [[nodiscard]]
            constexpr size_type size() const noexcept {
                return this->_size;
            }

            [[nodiscard]]
            constexpr size_type max_size() const noexcept {
                const size_type alloc_max = _allocator_traits::max_size(this->_allocator);

                return frd::min(IterDifferenceMax, alloc_max);
            }

            [[nodiscard]]
            constexpr size_type capacity() const noexcept {
                return this->_capacity;
            }

            [[nodiscard]]
            constexpr iterator begin() noexcept {
                return this->_data;
            }

            [[nodiscard]]
            constexpr const_iterator begin() const noexcept {
                return this->_data;
            }

            [[nodiscard]]
            constexpr const_iterator cbegin() const noexcept {
                return this->begin();
            }

            [[nodiscard]]
            constexpr reverse_iterator rbegin() noexcept {
                return reverse_iterator(this->end());
            }

            [[nodiscard]]
            constexpr const_reverse_iterator rbegin() const noexcept {
                return const_reverse_iterator(this->end());
            }

            [[nodiscard]]
            constexpr const_reverse_iterator crbegin() const noexcept {
                return const_reverse_iterator(this->cend());
            }

            [[nodiscard]]
            constexpr iterator end() noexcept {
                return this->_data + this->_size;
            }

            [[nodiscard]]
            constexpr const_iterator end() const noexcept {
                return this->_data + this->_size;
            }

            [[nodiscard]]
            constexpr const_iterator cend() const noexcept {
                return this->end();
            }

            [[nodiscard]]
            constexpr reverse_iterator rend() noexcept {
                return reverse_iterator(this->begin());
            }

            [[nodiscard]]
            constexpr const_reverse_iterator rend() const noexcept {
                return const_reverse_iterator(this->begin());
            }

            [[nodiscard]]
            constexpr const_reverse_iterator crend() const noexcept {
                return const_reverse_iterator(this->cbegin());
            }

            [[nodiscard]]
            constexpr bool empty() const noexcept {
                return this->_size <= 0;
            }

            [[nodiscard]]
            constexpr Allocator get_allocator() const noexcept {
                return this->_allocator;
            }

            /* TODO: Do we want 'at'? I'm not really a fan of it. */
            [[nodiscard]]
            constexpr reference operator [](size_type index) noexcept {
                frd::precondition(index < this->_size, "Index is past bounds of vector!");

                return this->_data[index];
            }

            [[nodiscard]]
            constexpr const_reference operator [](size_type index) const noexcept {
                frd::precondition(index < this->_size, "Index is past bounds of vector!");

                return this->_data[index];
            }

            constexpr auto operator <=>(const vector &rhs) const
            noexcept(
                nothrow_synthetic_three_way_comparable<const Element &>
            )
            requires (
                synthetic_three_way_comparable<const Element &>
            ) {
                for (const auto &[lhs_elem, rhs_elem] : views::zip(*this, rhs)) {
                    const auto cmp = synthetic_three_way_compare(lhs_elem, rhs_elem);

                    if (cmp != 0) {
                        return cmp;
                    }
                }

                return this->_size <=> rhs._size;
            }

            constexpr bool operator ==(const vector &rhs) const
            requires (
                equality_comparable<const Element &>
            ) {
                if (this->_size != rhs._size) {
                    return false;
                }

                for (const auto &[lhs_elem, rhs_elem] : views::zip(*this, rhs)) {
                    if (lhs_elem != rhs_elem) {
                        return false;
                    }
                }

                return true;
            }
    };

    template<input_range R, typename Allocator = allocator<range_value<R>>>
    requires (
        !template_specialization_of<R, vector> &&

        allocator_for<Allocator, range_value<R>>                          &&
        allocator_value_constructible_from<Allocator, range_reference<R>>
    )
    vector(R &&, Allocator = Allocator()) -> vector<range_value<R>, Allocator>;

}
