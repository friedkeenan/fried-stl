#pragma once

#include <frd/arithmetic.hpp>
#include <frd/utility.hpp>
#include <frd/ranges.hpp>
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

            using size_type       = frd::size_t;
            using difference_type = frd::ptrdiff_t;
            using iterator        = pointer;
            using const_iterator  = const_pointer;

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

            constexpr iterator end() noexcept {
                return this->begin() + Size;
            }

            constexpr const_iterator end() const noexcept {
                return this->begin() + Size;
            }

            constexpr const_iterator cend() const noexcept {
                return this->end();
            }

            constexpr reference operator [](const size_type index) noexcept {
                return this->_elems[index];
            }

            constexpr const_reference operator [](const size_type index) const noexcept {
                return this->_elems[index];
            }

            template<size_type I>
            constexpr reference get() noexcept {
                return this->_elems[I];
            }

            template<size_type I>
            constexpr const_reference get() const noexcept {
                return this->_elems[I];
            }

            constexpr auto operator <=>(const array &rhs) const noexcept {
                for (const auto i : interval(Size)) {
                    const auto cmp = synthetic_three_way_compare(this->_elems[i], rhs._elems[i]);

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

    template<typename Head, typename... Rest>
    array(Head, Rest...) -> array<Head, sizeof...(Rest) + 1>;

    template<typename T, frd::size_t N>
    void swap(array<T, N> &lhs, array<T, N> &rhs) noexcept(noexcept(lhs.swap(rhs))) {
        lhs.swap(rhs);
    }

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

    template<std::size_t I, typename T, frd::size_t N>
    struct tuple_element<I, frd::array<T, N>> : frd::type_holder<T> { };

}
