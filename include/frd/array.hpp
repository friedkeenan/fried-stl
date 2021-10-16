#pragma once

#include <tuple>

#include <frd/bits/ranges/interval.hpp>
#include <frd/bits/ranges/reverse_view.hpp>

#include <frd/arithmetic.hpp>
#include <frd/utility.hpp>
#include <frd/tuple.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename Element, frd::size_t Size>
    class array {
        public:
            using value_type      = Element;
            using pointer         = Element *;
            using const_pointer   = const Element *;
            using reference       = Element &;
            using const_reference = const Element &;

            using iterator        = pointer;
            using const_iterator  = const_pointer;
            using difference_type = frd::ptrdiff_t;
            using size_type       = frd::size_t;

            using reverse_iterator       = frd::reverse_iterator<iterator>;
            using const_reverse_iterator = frd::reverse_iterator<const_iterator>;

            Element _elems[Size];

            constexpr void fill(const Element &value) requires (copy_assignable<Element>) {
                for (auto &elem : *this) {
                    elem = value;
                }
            }

            constexpr void swap(array &other) noexcept(nothrow_swappable<Element>) requires (swappable<Element>) {
                for (const auto i : interval(Size)) {
                    frd::swap(this->_elems[i], other._elems[i]);
                }
            }

            /* ADL-discovered swap. */
            friend constexpr void swap(array &lhs, array &rhs) noexcept(nothrow_swappable<Element>) {
                lhs.swap(rhs);
            }

            constexpr reference front() noexcept {
                return this->_elems[0];
            }

            constexpr const_reference front() const noexcept {
                return this->_elems[0];
            }

            constexpr reference back() noexcept {
                return this->_elems[Size - 1];
            }

            constexpr const_reference back() const noexcept {
                return this->_elems[Size - 1];
            }

            constexpr pointer data() noexcept {
                return this->_elems + 0;
            }

            constexpr const_pointer data() const noexcept {
                return this->_elems + 0;
            }

            constexpr size_type size() const noexcept {
                return Size;
            }

            constexpr size_type max_size() const noexcept {
                return Size;
            }

            constexpr bool empty() const noexcept {
                return Size == 0;
            }

            constexpr iterator begin() noexcept {
                return this->data();
            }

            constexpr const_iterator begin() const noexcept {
                return this->data();
            }

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

            constexpr iterator end() noexcept {
                return this->begin() + Size;
            }

            constexpr const_iterator end() const noexcept {
                return this->begin() + Size;
            }

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


            constexpr reference operator [](const size_type index) noexcept {
                frd::precondition(index < Size, "Index is past bounds of array!");

                return this->_elems[index];
            }

            constexpr const_reference operator [](const size_type index) const noexcept {
                frd::precondition(index < Size, "Index is past bounds of array!");

                return this->_elems[index];
            }

            template<frd::size_t I, typename Self>
            static constexpr decltype(auto) _get(Self &&self) noexcept {
                return frd::forward<Self>(self)._elems[I];
            }

            template<frd::size_t I>
            constexpr decltype(auto) get() & noexcept {
                return _get<I>(*this);
            }

            template<frd::size_t I>
            constexpr decltype(auto) get() const & noexcept {
                return _get<I>(*this);
            }

            template<frd::size_t I>
            constexpr decltype(auto) get() && noexcept {
                return _get<I>(frd::move(*this));
            }

            template<frd::size_t I>
            constexpr decltype(auto) get() const && noexcept {
                return _get<I>(frd::move(*this));
            }

            constexpr auto operator <=>(const array &rhs) const
            noexcept(
                nothrow_synthetic_three_way_comparable<const Element &>
            )
            requires (
                synthetic_three_way_comparable<const Element &>
            ) {
                for (const auto i : interval(Size)) {
                    const auto cmp = frd::synthetic_three_way_compare(this->_elems[i], rhs._elems[i]);

                    if (cmp != 0) {
                        return cmp;
                    }
                }

                /* Should always be equal but it's helpful for returning a comparison type. */
                return (this->size() <=> rhs.size());
            }

            constexpr bool operator ==(const array &rhs) const noexcept {
                return (*this <=> rhs) == 0;
            }
    };

    /* Deduction guide for the implicitly declared constructor. */
    template<typename Head, typename... Rest>
    array(Head, Rest...) -> array<Head, sizeof...(Rest) + 1>;

    template<typename T>
    constexpr inline bool _array_specialization = false;

    template<typename Element, frd::size_t Size>
    constexpr inline bool _array_specialization<array<Element, Size>> = true;

    template<typename T>
    concept array_specialization = _array_specialization<remove_cvref<T>>;

    template<typename T, frd::size_t N, frd::size_t... I>
    constexpr array<remove_cv<T>, N> _to_array(const T (&arr)[N], index_sequence<I...>) {
        return {arr[I]...};
    }

    template<typename T, frd::size_t N, frd::size_t... I>
    constexpr array<remove_cv<T>, N> _to_array(T (&&arr)[N], index_sequence<I...>) {
        return {frd::move(arr[I])...};
    }

    template<typename T, frd::size_t N>
    constexpr array<remove_cv<T>, N> to_array(const T (&arr)[N]) {
        return _to_array(arr, make_index_sequence<N>{});
    }

    template<typename T, frd::size_t N>
    constexpr array<remove_cv<T>, N> to_array(T (&&arr)[N]) {
        return _to_array(frd::move(arr), make_index_sequence<N>{});
    }

}

namespace std {

    template<typename T, frd::size_t N>
    struct tuple_size<frd::array<T, N>> : frd::constant_holder<N> { };

    template<frd::size_t I, typename T, frd::size_t N>
    struct tuple_element<I, frd::array<T, N>> : frd::type_holder<T> { };

}
