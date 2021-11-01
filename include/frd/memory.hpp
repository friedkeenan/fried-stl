#pragma once

#include <memory>

#include <frd/bits/memory_base.hpp>

#include <frd/defines.hpp>
#include <frd/arithmetic.hpp>
#include <frd/algorithm.hpp>
#include <frd/utility.hpp>
#include <frd/ranges.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename Element>
    class _unique_ptr_impl {
        /* A common base class for primary and array overloads of unique_ptr. */

        FRD_NON_COPYABLE(_unique_ptr_impl);

        public:
            using element_type    = Element;
            using pointer         = Element *;
            using reference       = Element &;
            using const_reference = const Element &;

            pointer _ptr = nullptr;

            /*
                I would prefer to have make_unique instead just be a constructor for
                unique_ptr, but that will result in a lot of potential ambiguity with
                other constructors, such as the move constructor.
            */
            constexpr _unique_ptr_impl() = default;

            constexpr explicit _unique_ptr_impl(const pointer ptr) noexcept : _ptr(ptr) { }

            [[nodiscard]]
            constexpr pointer get() const noexcept {
                return this->_ptr;
            }

            [[nodiscard]]
            constexpr bool empty() const noexcept {
                return this->_ptr == nullptr;
            }

            [[nodiscard]]
            constexpr pointer release() noexcept {
                return frd::exchange(this->_ptr, nullptr);
            }

            constexpr void swap(_unique_ptr_impl &other) noexcept {
                frd::swap(this->_ptr, other._ptr);
            }
    };

    template<typename T>
    requires (!bound_array<T>)
    class unique_ptr : public _unique_ptr_impl<T> {
        /* A minimal implementation of std::unique_ptr, with no custom deleters. */

        public:
            using pointer   = T *;
            using reference = T &;

            using Base = _unique_ptr_impl<T>;
            using Base::Base;

            template<typename U>
            requires (convertible_to<U *, pointer>)
            constexpr unique_ptr(unique_ptr<U> &&other) noexcept : Base(other.release()) { }

            template<typename U>
            requires (convertible_to<U *, pointer>)
            constexpr unique_ptr &operator =(unique_ptr<U> &&rhs) noexcept {
                /* Only check if assigning to self if 'rhs' is of the same type. */
                if constexpr (same_as<U, T>) {
                    FRD_CHECK_SELF(rhs);
                }

                if (!this->empty()) {
                    delete this->_ptr;
                }

                this->_ptr = rhs.release();

                return *this;
            }

            constexpr ~unique_ptr() noexcept {
                if (!this->empty()) {
                    delete this->_ptr;
                }
            }

            constexpr void reset(const pointer ptr = pointer()) noexcept {
                const auto old_ptr = frd::exchange(this->_ptr, ptr);

                if (old_ptr != nullptr) {
                    delete old_ptr;
                }
            }

            friend constexpr void swap(unique_ptr &lhs, unique_ptr &rhs) noexcept {
                lhs.swap(rhs);
            }

            [[nodiscard]]
            constexpr reference operator *() const noexcept(noexcept(*frd::declval<pointer>())) {
                return *this->_ptr;
            }

            constexpr pointer operator ->() const noexcept {
                return this->_ptr;
            }
    };

    template<typename T>
    class unique_ptr<T[]> : public _unique_ptr_impl<T> {
        /* A minimal implementation of std::unique_ptr, with no custom deleters. */

        public:
            using pointer   = T *;
            using reference = T &;

            using Base = _unique_ptr_impl<T>;
            using Base::Base;

            template<typename U>
            requires (convertible_to<U (*)[], T(*)[]>)
            constexpr unique_ptr(unique_ptr<U[]> &&other) noexcept : Base(other.release()) { }

            template<typename U>
            requires (convertible_to<U (*)[], T(*)[]>)
            constexpr unique_ptr &operator =(unique_ptr<U[]> &&rhs) noexcept {
                /* Only check if assigning to self if 'rhs' is of the same type. */
                if constexpr (same_as<U, T>) {
                    FRD_CHECK_SELF(rhs);
                }

                if (!this->empty()) {
                    delete[] this->_ptr;
                }

                this->_ptr = rhs.release();

                return *this;
            }

            constexpr ~unique_ptr() noexcept {
                if (!this->empty()) {
                    delete[] this->_ptr;
                }
            }

            template<typename U>
            requires (convertible_to<U (*)[], T (*)[]>)
            constexpr void reset(const U *ptr = nullptr) noexcept {
                const auto old_ptr = frd::exchange(this->_ptr, ptr);

                if (old_ptr != nullptr) {
                    delete[] old_ptr;
                }
            }

            constexpr void reset() noexcept {
                if (!this->empty()) {
                    delete[] this->_ptr;
                }

                this->_ptr = nullptr;
            }

            friend constexpr void swap(unique_ptr &lhs, unique_ptr &rhs) noexcept {
                lhs.swap(rhs);
            }

            constexpr reference operator [](frd::size_t index) const {
                return this->_ptr[index];
            }
    };

    template<typename T, typename... Args>
    requires (!array_type<T> && constructible_from<T, Args...>)
    constexpr unique_ptr<T> make_unique(Args &&... args) {
        return unique_ptr<T>(new T(frd::forward<Args>(args)...));
    }

    template<unbound_array T>
    requires (default_initializable<remove_extent<T>>)
    constexpr unique_ptr<T> make_unique(frd::size_t size) {
        return unique_ptr<T>(new remove_extent<T>[size]{});
    }

    template<typename T, allocator_for<T> Allocator = allocator<T>>
    requires (!array_type<T>)
    class scoped_ptr {
        /*
            scoped_ptr does not have an array overload, nor does it allow
            to convert a scoped_ptr<Derived> to a scoped_ptr<Base>, as the
            implementation needed for those things and allocator-awareness
            is not fun and I do not want to do it.
        */

        FRD_NON_COPYABLE(scoped_ptr);

        public:
            using _allocator_traits = allocator_traits<Allocator>;

            using allocator_type  = Allocator;
            using value_type      = T;
            using pointer         = typename _allocator_traits::pointer;
            using reference       = T &;
            using const_reference = const T &;

            [[nodiscard]]
            static constexpr pointer _move_element(Allocator &alloc, T &elem) {
                const auto ptr = _allocator_traits::allocate(alloc, 1);
                _allocator_traits::construct(alloc, ptr, frd::move(elem));

                return ptr;
            }

            pointer _ptr = nullptr;

            [[no_unique_address]] Allocator _allocator;

            /*
                I would prefer to have make_scoped instead just be a constructor for
                scoped_ptr, but that will result in a lot of potential ambiguity with
                other constructors, such as the move constructor.
            */
            constexpr scoped_ptr() noexcept(noexcept(Allocator())) requires (default_initializable<Allocator>) = default;

            constexpr explicit scoped_ptr(const Allocator &alloc) noexcept : _allocator(alloc) { }

            constexpr explicit scoped_ptr(const pointer ptr, const Allocator &alloc = Allocator()) noexcept : _ptr(ptr), _allocator(alloc) { }

            constexpr scoped_ptr(scoped_ptr &&other) noexcept : _ptr(other.release()), _allocator(frd::move(other._allocator)) { }

            constexpr scoped_ptr(scoped_ptr &&other, const Allocator &alloc) noexcept requires (_allocator_traits::always_equal)
                : _ptr(other.release()), _allocator(alloc) { }

            constexpr scoped_ptr(scoped_ptr &&other, const Allocator &alloc) : _allocator(alloc) {
                if (other.empty()) {
                    this->_ptr = nullptr;
                } else if (this->_allocator == other._allocator) {
                    this->_ptr = other.release();
                } else {
                    this->_ptr = _move_element(this->_allocator, *other._ptr);

                    other._destroy(other._ptr);
                    other._ptr = nullptr;
                }
            }

            constexpr scoped_ptr &operator =(scoped_ptr &&rhs) noexcept(_allocator_traits::propagate_on_container_move_assignment || _allocator_traits::always_equal) {
                FRD_CHECK_SELF(rhs);

                if (!this->empty()) {
                    this->_destroy(this->_ptr);
                }

                if constexpr (_allocator_traits::propagate_on_container_move_assignment) {
                    this->_allocator = frd::move(rhs._allocator);
                    this->_ptr       = rhs.release();
                } else if (rhs.empty()) {
                    this->_ptr = nullptr;
                } else if (_allocator_traits::are_equal(this->_allocator, rhs._allocator)) {
                    this->_ptr = rhs.release();
                } else {
                    this->_ptr = _move_element(this->_allocator, *rhs._ptr);

                    rhs._destroy(rhs._ptr);
                    rhs._ptr = nullptr;

                }

                return *this;
            }

            constexpr ~scoped_ptr() {
                if (!this->empty()) {
                    this->_destroy(this->_ptr);
                }
            }

            constexpr void _destroy(const pointer ptr) {
                _allocator_traits::destroy(this->_allocator, ptr);
                _allocator_traits::deallocate(this->_allocator, ptr, 1);
            }

            [[nodiscard]]
            constexpr pointer get() const noexcept {
                return this->_ptr;
            }

            [[nodiscard]]
            constexpr bool empty() const noexcept {
                return this->_ptr == nullptr;
            }

            [[nodiscard]]
            constexpr pointer release() noexcept {
                return frd::exchange(this->_ptr, nullptr);
            }

            constexpr void reset(pointer ptr = pointer()) noexcept {
                const auto old_ptr = frd::exchange(this->_ptr, ptr);

                if (old_ptr != nullptr) {
                    this->_destroy(old_ptr);
                }
            }

            constexpr void swap(scoped_ptr &other) noexcept(_allocator_traits::propagate_on_container_swap || _allocator_traits::always_equal) {
                if (frd::same_obj(*this, other)) {
                    return;
                }

                if constexpr (_allocator_traits::propagate_on_container_swap) {
                    frd::swap(this->_allocator, other._allocator);
                    frd::swap(this->_ptr,       other._ptr);
                } else if (this->empty() && other.empty()) {
                    return;
                } else if (_allocator_traits::are_equal(this->_allocator, other._allocator)) {
                    frd::swap(this->_ptr, other._ptr);
                } else {
                    if (other.empty()) {
                        other._ptr = _move_element(other._allocator, *this->_ptr);

                        this->_destroy(this->_ptr);
                        this->_ptr = nullptr;
                    } else if (this->empty()) {
                        this->_ptr = _move_element(this->_allocator, *other._ptr);

                        other._destroy(other._ptr);
                        other._ptr = nullptr;
                    } else {
                        frd::swap(*this->_ptr, *other._ptr);
                    }
                }
            }

            /* ADL-discovered swap. */
            friend constexpr void swap(scoped_ptr &lhs, scoped_ptr &rhs) noexcept(noexcept(lhs.swap(rhs))) {
                lhs.swap(rhs);
            }

            [[nodiscard]]
            constexpr Allocator get_allocator() const noexcept {
                return this->_allocator;
            }

            [[nodiscard]]
            constexpr reference operator *() const noexcept(noexcept(*frd::declval<pointer>())) {
                return *this->_ptr;
            }

            constexpr pointer operator ->() const noexcept {
                return this->_ptr;
            }
    };

    template<typename T, allocator_for<T> Allocator, typename... Args>
    requires (allocator_value_constructible_from<Allocator, Args...>)
    constexpr scoped_ptr<T, Allocator> make_scoped_with_allocator(Allocator alloc, Args &&... args) {
        const auto ptr = allocator_traits<Allocator>::allocate(alloc, 1);
        allocator_traits<Allocator>::construct(alloc, ptr, frd::forward<Args>(args)...);

        return scoped_ptr<T, Allocator>(ptr, frd::move(alloc));
    }

    template<typename T, default_initializable Allocator = allocator<T>, typename... Args>
    requires (allocator_for<Allocator, T> && allocator_value_constructible_from<Allocator, Args...>)
    constexpr scoped_ptr<T, Allocator> make_scoped(Args &&... args) {
        return make_scoped_with_allocator<T, Allocator>(Allocator(), frd::forward<Args>(args)...);
    }

    template<
        typename Element,
        allocator_for<Element> Allocator = allocator<Element>,

        /* Get the eventual 'size_type' of the container without having to specify more template arguments. */
        make_unsigned<iter_difference<typename allocator_traits<Allocator>::pointer>> Capacity = dynamic_extent,

        /* TODO: make this an enum argument so a name has to be at the call site? */
        bool SizeIsCapacity = false
    >
    class allocated_data {
        public:
            /* TODO: Strong exception guarantees? */

            using _allocator_traits = allocator_traits<Allocator>;

            using allocator_type  = Allocator;
            using value_type      = Element;
            using reference       = Element &;
            using const_reference = const Element &;
            using pointer         = typename _allocator_traits::pointer;
            using const_pointer   = typename _allocator_traits::const_pointer;

            /* TODO: Make an iterator that wraps a pointer to require more explicitness? */
            using iterator        = pointer;
            using const_iterator  = const_pointer;
            using difference_type = iter_difference<iterator>;
            using size_type       = make_unsigned<difference_type>;

            using reverse_iterator       = frd::reverse_iterator<iterator>;
            using const_reverse_iterator = frd::reverse_iterator<const_iterator>;

            static constexpr auto IterDifferenceMax = static_cast<size_type>(numeric_limits<difference_type>::max);

            static constexpr bool DynamicCapacity = (Capacity == dynamic_extent);

            [[no_unique_address]] Allocator _allocator = Allocator();

            pointer _data = nullptr;

            [[no_unique_address]] maybe_present<!SizeIsCapacity, size_type> _size     = 0;
            [[no_unique_address]] maybe_present<DynamicCapacity, size_type> _capacity = 0;

            constexpr allocated_data() requires (DynamicCapacity && default_initializable<Allocator>) = default;

            constexpr allocated_data(const Allocator &alloc) requires (DynamicCapacity) : _allocator(alloc) { }

            constexpr allocated_data() requires (!DynamicCapacity && default_initializable<Allocator>)
                : _data(this->_allocate(this->capacity())) { }

            constexpr allocated_data(const Allocator &alloc) requires (!DynamicCapacity)
                : _allocator(alloc), _data(this->_allocate(this->capacity())) { }

            constexpr allocated_data(const size_type capacity, const Allocator &alloc = Allocator())
            requires (
                DynamicCapacity &&
                SizeIsCapacity
            ) : _allocator(alloc), _data(this->_allocate(capacity)), _capacity(capacity) { }

            constexpr allocated_data(const size_type capacity, const size_type size = 0, const Allocator &alloc = Allocator())
            requires (
                DynamicCapacity &&
                !SizeIsCapacity
            ) :
                _allocator(alloc),
                _data(this->_allocate(capacity)),
                _size(size),
                _capacity(capacity)
            {
                frd::precondition(size <= capacity, "Size must not exceed capacity!");
            }

            constexpr allocated_data(const allocated_data &other)
            requires (
                allocator_value_constructible_from<Allocator, const Element &>
            ) :
                _allocator(
                    _allocator_traits::select_on_container_copy_construction(other._allocator)
                ),
                _data(this->_allocate(other.size())),
                _size(other.size()),
                _capacity(other.size())
            {
                for (const auto i : interval(this->size())) {
                    this->construct_at(i, other[i]);
                }
            }

            /*
                TODO: Figure out correct semantics for moving an 'allocated_data' object with static capacity.

                For objects with a static capacity, they *always* need data of that capacity, and furthermore
                if their size is also the same as their capacity, then at least for destruction they need valid
                objects populating all of that data, since the size can't be set to 0. The crux of the issue
                comes from what to do with the allocator. Do we move it? That doesn't seem right, since the
                moved-from container still needs it, at least for destruction. We could potentially move it and
                then destruct its elements for it, but then when it tries to destruct itself, it'll try to destruct
                its elements too, and we can't necessarily stop it by changing its size since its size may be
                static as well. We could maybe use 'select_on_container_copy_construction' but I'm not sure that
                would be correct since we're not copying the elements, we'd be moving them. Maybe we could have
                a move constructor if the allocators are always equal and default-constructible? Not sure that's
                a case worth giving special treatment. We could just delete the corresponding move constructors,
                so at least they don't fall back silently to the copy constructors, but then that would cause it
                to fail to satisfy 'copyable' and other copy-related concepts since the type would not be movable.
            */

            constexpr allocated_data(allocated_data &&other) noexcept
            requires (
                DynamicCapacity
            ) :
                _allocator(frd::move(other._allocator)),
                _data(frd::exchange(other._data, nullptr)),
                _size(frd::exchange(other._size, size_type{0})),
                _capacity(frd::exchange(other._capacity, size_type{0}))
            { }

            constexpr allocated_data(const allocated_data &other, const Allocator &alloc)
            requires (
                allocator_value_constructible_from<Allocator, const Element &>
            ) :
                _allocator(alloc),
                _data(this->_allocate(other.size())),
                _size(other.size()),
                _capacity(other,size())
            {
                for (const auto i : interval(this->size())) {
                    this->construct_at(i, other[i]);
                }
            }

            constexpr allocated_data(allocated_data &&other, const Allocator &alloc)
            noexcept(
                nothrow_constructible_from<Allocator, const Allocator &>
            )
            requires (
                DynamicCapacity &&

                _allocator_traits::always_equal
            ) :
                _allocator(alloc),
                _data(frd::exchange(other._data, nullptr)),
                _size(frd::exchange(other._size, size_type{0})),
                _capacity(frd::exchange(other._capacity, size_type{0}))
            { }

            constexpr allocated_data(allocated_data &&other, const Allocator &alloc)
            requires (
                DynamicCapacity &&

                allocator_value_constructible_from<Allocator, Element &&>
            ) :
                _allocator(alloc),
                _size(other.size())
            {
                if (other.empty()) {
                    return;
                }

                if (this->_allocator == other._allocator) {
                    this->_data     = frd::exchange(other._data,     nullptr);
                    this->_size     = frd::exchange(other._size,     size_type{0});
                    this->_capacity = frd::exchange(other._capacity, size_type{0});
                } else {
                    this->_data     = this->_allocate(other.size());
                    this->_size     = other.size();
                    this->_capacity = other.size();

                    for (const auto i : interval(this->size())) {
                        this->construct_at(i, frd::move(other[i]));
                    }
                }
            }

            /* TODO: Find a way to shorten this function? */
            constexpr allocated_data &operator =(const allocated_data &rhs) {
                FRD_CHECK_SELF(rhs);

                bool reallocated = false;

                if constexpr (_allocator_traits::propagate_on_container_copy_assignment) {
                    if (!_allocator_traits::are_equal(this->_allocator, rhs._allocator)) {
                        this->cleanup_data();

                        if constexpr (DynamicCapacity) {
                            this->_data     = rhs._allocate(rhs.size());
                            this->_capacity = rhs.size();
                        } else {
                            this->_data = rhs._allocate(this->capacity());
                        }

                        reallocated = true;
                    }

                    this->_allocator = rhs._allocator;
                }

                /*
                    We only need to do this check if we have a dynamic capacity since it
                    is a precondition that our static capacity can fit any size we reach.
                */
                if constexpr (DynamicCapacity) {
                    if (this->capacity() < rhs.size()) {
                        this->cleanup_data();

                        this->_data     = this->_allocate(rhs.size());
                        this->_capacity = rhs.size();

                        reallocated = true;
                    }
                }

                for (const auto i : interval(rhs.size())) {
                    /* If we didn't need to reallocate, we can just assign to our elements. */
                    if (reallocated || i >= this->size()) {
                        this->construct_at(i, rhs[i]);
                    } else {
                        *(this->begin() + i) = rhs[i];
                    }
                }

                if (this->size() > rhs.size()) {
                    for (const auto i : interval(rhs.size(), this->size())) {
                        this->destroy_at(i);
                    }
                }

                if constexpr (!SizeIsCapacity) {
                    this->change_size(rhs.size());
                }

                return *this;
            }

            constexpr ~allocated_data() {
                this->cleanup_data();
            }

            constexpr void free_data() {
                this->_deallocate(this->data(), this->capacity());
            }

            constexpr void destroy_elements_up_to(const size_type index) {
                for (const auto i : interval(index)) {
                    this->destroy_at(i);
                }
            }

            constexpr void destroy_elements() {
                this->destroy_elements_up_to(this->size());
            }

            constexpr void cleanup_data() {
                this->destroy_elements();
                this->free_data();
            }

            template<typename... Args>
            requires (allocator_value_constructible_from<Allocator, Args...>)
            constexpr void construct_at_ptr(const pointer location, Args &&... args) {
                _allocator_traits::construct(this->_allocator, location, frd::forward<Args>(args)...);
            }

            template<typename... Args>
            requires (allocator_value_constructible_from<Allocator, Args...>)
            constexpr void construct_at(const size_type index, Args &&... args) {
                frd::precondition(index < this->capacity(), "Invalid index for construction!");

                this->construct_at_ptr(this->data() + index, frd::forward<Args>(args)...);
            }

            constexpr void destroy_at_ptr(const pointer location) {
                _allocator_traits::destroy(this->_allocator, location);
            }

            constexpr void destroy_at(const size_type index) {
                frd::precondition(index < this->capacity(), "Invalid index for destruction!");

                this->destroy_at_ptr(this->data() + index);
            }

            constexpr pointer _allocate(const size_type data_size) {
                if (data_size > 0) {
                    return _allocator_traits::allocate(this->_allocator, data_size);
                }

                return nullptr;
            }

            constexpr void _deallocate(const pointer data, const size_type data_size) {
                if (data_size > 0) {
                    _allocator_traits::deallocate(this->_allocator, data, data_size);
                }
            }

            constexpr void _reserve_preconditions(const size_type new_capacity) const noexcept {
                frd::precondition(new_capacity <= this->max_size(), "Cannot reserve enough data!");
                frd::precondition(new_capacity >= this->size(),     "Cannot reserve less than current size!");
            }

            constexpr void reserve_and_destroy(const size_type new_capacity)
            requires (
                DynamicCapacity
            ) {
                this->_reserve_preconditions(new_capacity);

                this->cleanup_data();

                this->_data     = this->_allocate(new_capacity);
                this->_capacity = new_capacity;
            }

            constexpr void reserve_and_move(const size_type new_capacity)
            requires (
                DynamicCapacity &&

                allocator_value_constructible_from<Allocator, Element &&>
            ) {
                this->_reserve_preconditions(new_capacity);

                const auto new_data = this->_allocate(new_capacity);

                /* Move each element into its new location. */
                for (const auto i : interval(this->size())) {
                    this->construct_at_ptr(new_data + i, frd::iter_move(this->begin() + i));
                    this->destroy_at(i);
                }

                this->free_data();

                this->_data     = new_data;
                this->_capacity = new_capacity;
            }

            constexpr size_type capacity() const noexcept {
                if constexpr (DynamicCapacity) {
                    return this->_capacity;
                } else {
                    return Capacity;
                }
            }

            constexpr size_type size() const noexcept {
                if constexpr (SizeIsCapacity) {
                    return this->capacity();
                } else {
                    frd::precondition(this->_size <= this->capacity(), "Size must not exceed capacity!");

                    return this->_size;
                }
            }

            constexpr void change_size(const size_type new_size) noexcept
            requires (
                /* If size and capacity are the same, the reserve methods should be used. */
                !SizeIsCapacity
            ) {
                frd::precondition(new_size <= this->capacity(), "Size must not exceed capacity!");

                this->_size = new_size;
            }

            constexpr void increment_size(const size_type increment) noexcept
            requires (
                !SizeIsCapacity
            ) {
                frd::precondition(this->_size + increment <= this->capacity(), "Size must not exceed capacity!");

                this->_size += increment;
            }

            constexpr bool empty() const noexcept {
                return this->size() == 0;
            }

            constexpr size_type max_size() const noexcept {
                /* Our 'size_type' comes from our iterators, not our allocator. */
                const auto max_allocator_size = static_cast<size_type>(_allocator_traits::max_size(this->_allocator));

                return frd::min(IterDifferenceMax, max_allocator_size);
            }

            constexpr reference front() noexcept {
                return *this->begin();
            }

            constexpr const_reference front() const noexcept {
                return *this->begin();
            }

            constexpr reference back() noexcept {
                return *(this->begin() + this->size() - 1);
            }

            constexpr const_reference back() const noexcept {
                return *(this->begin() + this->size() - 1);
            }

            constexpr pointer data() noexcept {
                return this->_data;
            }

            constexpr const_pointer data() const noexcept {
                return this->_data;
            }

            constexpr iterator begin() noexcept {
                return this->_data;
            }

            constexpr const_iterator begin() const noexcept {
                return this->_data;
            }

            constexpr const_iterator cbegin() const noexcept {
                return this->begin();
            }

            constexpr reverse_iterator rbegin() noexcept {
                return reverse_iterator(this->end());
            }

            constexpr const_reverse_iterator crbegin() const noexcept {
                return const_reverse_iterator(this->cend());
            }

            constexpr iterator end() noexcept {
                return this->_data + this->size();
            }

            constexpr const_iterator end() const noexcept {
                return this->_data + this->size();
            }

            constexpr const_iterator cend() const noexcept {
                return this->end();
            }

            constexpr reverse_iterator rend() noexcept {
                return reverse_iterator(this->begin());
            }

            constexpr const_reverse_iterator crend() const noexcept {
                return const_reverse_iterator(this->cbegin());
            }

            constexpr Allocator get_allocator() const noexcept {
                return this->_allocator;
            }

            constexpr reference operator [](const size_type index) noexcept {
                frd::precondition(index < this->size(), "Index is past bounds of data!");

                return this->_data[index];
            }

            constexpr const_reference operator [](const size_type index) const noexcept {
                frd::precondition(index < this->size(), "Index is past bounds of data!");

                return this->_data[index];
            }

            constexpr auto operator <=>(const allocated_data &rhs) const
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

            constexpr bool operator ==(const allocated_data &rhs) const
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

}
