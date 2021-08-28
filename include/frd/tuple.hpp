#pragma once

#include <memory>
#include <compare>
#include <tuple>

#include <frd/arithmetic.hpp>
#include <frd/utility.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename T, typename U>
    concept __tuple_comparable_impl = requires(const T &t, const U &u) {
        { t < u } -> boolean_testable;
        { u < t } -> boolean_testable;
    };

    template<typename... Ts>
    class tuple;

    template<typename Head, typename... Tail>
    class tuple<Head, Tail...> {
        public:
            using size_type = size_t;

            using iterator       = Head *;
            using const_iterator = const Head *;

            static constexpr bool SameTypes = (frd::same_as<Head, Tail> && ...);

            template<typename CmpHead, typename... CmpTail>
            static consteval bool IsComparable() {
                return (
                    std::three_way_comparable_with<Head, CmpHead> ||
                    __tuple_comparable_impl<Head, CmpHead>
                ) && ((
                    std::three_way_comparable_with<Tail, CmpTail> ||
                    __tuple_comparable_impl<Tail, CmpTail>
                ) && ...);
            }

            Head _head;
            [[no_unique_address]] tuple<Tail...> _tail;

            constexpr tuple() = default;
            constexpr tuple(const tuple &) = default;
            constexpr tuple(tuple &&) = default;

            constexpr tuple &operator =(const tuple &) = default;
            constexpr tuple &operator =(tuple &&) = default;

            template<typename HeadFwd, typename... TailFwd>
            requires (sizeof...(TailFwd) == sizeof...(Tail))
            constexpr explicit tuple(HeadFwd &&head, TailFwd &&... tail) : _head(frd::forward<HeadFwd>(head)), _tail(frd::forward<TailFwd>(tail)...) { }

            template<size_type I>
            constexpr decltype(auto) get() noexcept {
                if constexpr (I == 0) {
                    return this->_head;
                } else {
                    return this->_tail.template get<I - 1>();
                }
            }

            template<size_type I>
            constexpr decltype(auto) get() const noexcept {
                if constexpr (I == 0) {
                    return this->_head;
                } else {
                    return this->_tail.template get<I - 1>();
                }
            }

            constexpr Head *data() noexcept requires SameTypes {
                return std::addressof(this->_head);
            }

            constexpr const Head *data() const noexcept requires SameTypes {
                return std::addressof(this->_head);
            }

            constexpr size_type size() const noexcept requires SameTypes {
                return 1 + sizeof...(Tail);
            }

            constexpr iterator begin() noexcept requires SameTypes {
                return this->data();
            }

            constexpr const_iterator begin() const noexcept requires SameTypes {
                return this->data();
            }

            constexpr iterator end() noexcept requires SameTypes {
                return this->data() + this->size();
            }

            constexpr const_iterator end() const noexcept requires SameTypes {
                return this->data() + this->size();
            }

            template<typename RhsHead, typename... RhsTail>
            requires (IsComparable<RhsHead, RhsTail...>() && sizeof...(RhsTail) == sizeof...(Tail))
            constexpr auto operator <=>(const tuple<RhsHead, RhsTail...> &rhs) const {
                constexpr auto synth_three_way = []<typename T, typename U>(const T &lhs, const U &rhs) {
                    if constexpr (std::three_way_comparable_with<T, U>) {
                        return lhs <=> rhs;
                    } else {
                        if (lhs < rhs) {
                            return std::weak_ordering::less;
                        }

                        if (rhs < lhs) {
                            return std::weak_ordering::greater;
                        }

                        return std::weak_ordering::equivalent;
                    }
                };

                const auto cmp = synth_three_way(this->_head, rhs._head);

                if (cmp != 0) {
                    return cmp;
                }

                return this->_tail <=> rhs._tail;
            }

            template<typename RhsHead, typename... RhsTail>
            requires (IsComparable<RhsHead, RhsTail...>() && sizeof...(RhsTail) == sizeof...(Tail))
            constexpr bool operator ==(const tuple<RhsHead, RhsTail...> &rhs) const {
                /* Equality only checks members, so can only be as efficient as <=>. */

                return (*this <=> rhs) == 0;
            }

            constexpr Head &operator [](const size_type index) noexcept requires SameTypes {
                return *(this->begin() + index);
            }

            constexpr const Head &operator [](const size_type index) const noexcept requires SameTypes {
                return *(this->begin() + index);
            }
    };

    template<>
    class tuple<> {
        public:
            constexpr auto operator <=>(const tuple &rhs) const noexcept = default;
    };

    template<typename... Ts>
    tuple(Ts...) -> tuple<Ts...>;

}

template<typename... Ts>
struct std::tuple_size<frd::tuple<Ts...>> : frd::constant_holder<sizeof...(Ts)> { };

/* Recursive case. */
template<std::size_t I, typename Head, typename... Tail>
struct std::tuple_element<I, frd::tuple<Head, Tail...>> : std::tuple_element<I - 1, frd::tuple<Tail...>> { };

/* Base case. */
template<typename Head, typename... Tail>
struct std::tuple_element<0, frd::tuple<Head, Tail...>> {
    using type = Head;
};

template<typename T, typename Head, typename... Tail>
constexpr T &std::get(frd::tuple<Head, Tail...> &t) noexcept {
    if constexpr (frd::same_as<T, Head>) {
        return t._head;
    } else {
        return std::get<T>(t._tail);
    }
}

template<typename T, typename Head, typename... Tail>
constexpr const T &std::get(const frd::tuple<Head, Tail...> &t) noexcept {
    if constexpr (frd::same_as<T, Head>) {
        return t._head;
    } else {
        return std::get<T>(t._tail);
    }
}

template<typename T, typename Head, typename... Tail>
constexpr T &&std::get(frd::tuple<Head, Tail...> &&t) noexcept {
    if constexpr (frd::same_as<T, Head>) {
        return std::move(t._head);
    } else {
        return std::get<T>(std::move(t._tail));
    }
}

template<typename T, typename Head, typename... Tail>
constexpr const T &&std::get(const frd::tuple<Head, Tail...> &&t) noexcept {
    if constexpr (frd::same_as<T, Head>) {
        return std::move(t._head);
    } else {
        return std::get<T>(std::move(t._tail));
    }
}
