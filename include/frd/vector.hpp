#pragma once

#include <initializer_list>

#include <frd/limits.hpp>
#include <frd/memory.hpp>
#include <frd/ranges.hpp>
#include <frd/algorithm.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /* TODO: Decide whether to do vector<bool> specialization. */

    template<typename Element, allocator_for<Element> Allocator = allocator<Element>>
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

            static constexpr size_type IterDifferenceMax = numeric_limits<difference_type>::max;

            static constexpr size_type NewCapacityRatio = 2;

            pointer   _data     = nullptr;
            size_type _size     = 0;
            size_type _capacity = 0;

            [[no_unique_address]] Allocator _allocator;

            constexpr vector() noexcept(noexcept(Allocator())) requires (default_constructible<Allocator>) = default;

            constexpr explicit vector(const Allocator &alloc) noexcept : _allocator(alloc) { }

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

            constexpr iterator _to_mutable_iterator(const const_iterator it) {
                const auto begin = this->begin();

                return (begin + (it - begin));
            }

            constexpr void _reserve_impl(const size_type new_capacity) {
                FRD_ASSERT(new_capacity <= this->max_size(), "Cannot reserve enough data!");
                FRD_ASSERT(new_capacity >= this->_size,      "Cannot reserve less than current size!");

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

                this->_reserve_impl(new_capacity);
            }

            template<typename... Args>
            requires (allocator_value_constructible_from<Allocator, Args...>)
            constexpr reference emplace_back(Args &&... args) {
                /*
                    We could call 'this->emplace(this->end(), ...)', but that would lead
                    to slightly suboptimal performance, and this code is easier to understand.
                */

                /* We call '_reserve_impl' to avoid checks of current capacity. */
                if (this->_capacity == 0) {
                    this->_reserve_impl(1);
                } else if (this->_size + 1 > this->_capacity) {
                    this->_reserve_impl(NewCapacityRatio * this->_capacity);
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

            constexpr void _reserve_insert_impl(const const_iterator pos_to_insert, const size_type new_capacity, const size_type insert_size) {
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

            constexpr void _shift_elements(const iterator begin_shift, const size_type shift_size) {
                /*
                    Loop starting from the end so we can just move each element over
                    without overwriting any other element.
                */
                for (const auto elem_it : frd::interval(begin_shift, this->end()) | views::reverse) {
                    const auto elem_addr = pointer{elem_it};

                    _allocator_traits::construct(this->_allocator, elem_addr + shift_size, frd::iter_move(elem_it));
                    _allocator_traits::destroy  (this->_allocator, elem_addr);
                }
            }

            template<typename... Args>
            requires (allocator_value_constructible_from<Allocator, Args...>)
            constexpr iterator emplace(const const_iterator pos, Args &&... args) {
                const auto emplace_offset = pos - this->begin();

                if (this->_capacity == 0) {
                    /* Use '_reserve_impl' to avoid capacity checks. */

                    this->_reserve_impl(1);
                } else if (this->_size + 1 > this->_capacity) {
                    this->_reserve_insert_impl(pos, NewCapacityRatio * this->_capacity, 1);
                } else if (!this->empty()) {
                    /* We do not need to move anything if we're empty. */

                    this->_shift_elements(this->_to_mutable_iterator(pos), 1);
                }

                _allocator_traits::construct(this->_allocator, this->_data + emplace_offset, frd::forward<Args>(args)...);
                this->_size++;

                return this->begin() + emplace_offset;
            }

            template<forwarder_for<Element> ElementFwd>
            constexpr iterator insert(const const_iterator pos, ElementFwd &&obj) {
                return this->emplace(pos, frd::forward<ElementFwd>(obj));
            }

            /*
                We need this bit of indirection to allow brace-enclosed initializer
                lists to be deduced to 'std::initializer_list', as the compiler will
                not know it can use 'std::initializer_list' for 'R'.
            */
            template<typename R>
            /* Requirements checked by callers. */
            constexpr iterator _insert_range(const const_iterator pos, R &&insert_rng) {
                const auto insert_size   = frd::size(insert_rng);
                const auto insert_offset = pos - this->begin();

                if (this->_capacity == 0) {
                    /* Use '_reserve_impl' to avoid capacity checks. */

                    this->_reserve_impl(insert_size);
                } else if (this->_size + insert_size > this->_capacity) {
                    this->_reserve_insert_impl(pos, NewCapacityRatio * this->_capacity, insert_size);
                } else if (!this->empty()) {
                    /* We do not need to move anything if we're empty. */

                    this->_shift_elements(this->_to_mutable_iterator(pos), insert_size);
                }

                /* Insert the elements of the range into the empty locations we have made for it. */
                auto insert_location = this->_data + insert_offset;
                for (auto &&to_insert : insert_rng) {
                    _allocator_traits::construct(this->_allocator, insert_location, frd::forward<decltype(to_insert)>(to_insert));

                    insert_location++;
                }

                this->_size += insert_size;

                return this->begin() + insert_offset;
            }

            constexpr iterator insert(const const_iterator pos, const std::initializer_list<Element> list) {
                return this->_insert_range(pos, list);
            }

            template<sized_range R>
            requires (
                input_range<R>                                                    &&
                same_as<remove_cvref<range_reference<R>>, Element>                &&
                allocator_value_constructible_from<Allocator, range_reference<R>>
            )
            constexpr iterator insert(const const_iterator pos, R &&insert_rng) {
                return this->_insert_range(pos, frd::forward<R>(insert_rng));
            }

            constexpr void pop_back() {
                _allocator_traits::destroy(this->_allocator, this->_data + this->_size - 1);

                this->_size--;
            }

            constexpr void shrink_to_fit() {
                /* We don't need to reallocate if the size and capacity already match. */
                if (this->_size == this->_capacity) {
                    return;
                }

                this->_reserve_impl(this->_size);
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

            constexpr void resize(const size_type new_size) requires (allocator_value_default_constructible<Allocator>) {
                if (new_size < this->_size) {
                    this->shrink_size(new_size);
                } else {
                    this->reserve(new_size);

                    for (const auto i : interval(this->_size, new_size)) {
                        _allocator_traits::construct(this->_allocator, this->_data + i);
                    }
                }
            }

            constexpr void resize(const size_type new_size, const Element &value) {
                if (new_size < this->_size) {
                    this->shrink_size(new_size);
                } else {
                    this->reserve(new_size);

                    for(const auto i : interval(this->_size, new_size)) {
                        _allocator_traits::construct(this->_allocator, this->_data + i, value);
                    }
                }
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
            constexpr reverse_iterator<iterator> rbegin() noexcept {
                return reverse_iterator(this->end());
            }

            [[nodiscard]]
            constexpr reverse_iterator<const_iterator> rbegin() const noexcept {
                return reverse_iterator(this->end());
            }

            [[nodiscard]]
            constexpr reverse_iterator<const_iterator> crbegin() const noexcept {
                return reverse_iterator(this->cend());
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
            constexpr reverse_iterator<iterator> rend() noexcept {
                return reverse_iterator(this->begin());
            }

            [[nodiscard]]
            constexpr reverse_iterator<const_iterator> rend() const noexcept {
                return reverse_iterator(this->begin());
            }

            [[nodiscard]]
            constexpr reverse_iterator<const_iterator> crend() const noexcept {
                return reverse_iterator(this->cbegin());
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
                FRD_ASSERT(index < this->_size, "Index is past bounds of vector!");

                return this->_data[index];
            }

            [[nodiscard]]
            constexpr const_reference operator [](size_type index) const noexcept {
                FRD_ASSERT(index < this->_size, "Index is past bounds of vector!");

                return this->_data[index];
            }
    };

}
