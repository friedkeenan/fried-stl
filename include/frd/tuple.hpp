#pragma once

#include <memory>
#include <tuple>

#include <frd/arithmetic.hpp>
#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename... Ts>
    class tuple;

    template<typename Head, typename... Tail>
    class tuple<Head, Tail...> {
        public:
            using size_type = size_t;

            using iterator       = Head *;
            using const_iterator = const Head *;

            static constexpr bool SameTypes = (same_as<Head, Tail> && ...);

            template<typename CmpHead, typename... CmpTail>
            static consteval bool _is_comparable() {
                return (
                    sizeof...(Tail) == sizeof...(CmpTail)
                ) && (
                    std::three_way_comparable_with<Head, CmpHead>   ||
                    weakly_less_than_comparable_with<Head, CmpHead>
                ) && ((
                    std::three_way_comparable_with<Tail, CmpTail>   ||
                    weakly_less_than_comparable_with<Tail, CmpTail>
                ) && ...);
            }


            /*
                NOTE: Both libstdc++ and libc++ store the tail elements
                before the head element, but in 'pair' they store the
                first element before the second. The layout doesn't seem
                to be mandated by the standard, but it makes more sense
                to me to store the head element before the tail elements.
            */
            [[no_unique_address]] Head _head;
            [[no_unique_address]] tuple<Tail...> _tail;

            constexpr tuple() = default;
            constexpr tuple(const tuple &) = default;
            constexpr tuple(tuple &&) = default;

            constexpr tuple &operator =(const tuple &) = default;
            constexpr tuple &operator =(tuple &&) = default;

            template<typename HeadFwd, typename... TailFwd>
            requires (sizeof...(TailFwd) == sizeof...(Tail))
            constexpr explicit tuple(HeadFwd &&head, TailFwd &&... tail) : _head(frd::forward<HeadFwd>(head)), _tail(frd::forward<TailFwd>(tail)...) { }

            /* TODO: Make sure we don't need lvalue/rvalue overloads. */
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
            requires (_is_comparable<RhsHead, RhsTail...>())
            constexpr auto operator <=>(const tuple<RhsHead, RhsTail...> &rhs) const {
                const auto cmp = synthetic_three_way_compare(this->_head, rhs._head);

                if (cmp != 0) {
                    return cmp;
                }

                return this->_tail <=> rhs._tail;
            }

            template<typename RhsHead, typename... RhsTail>
            requires (_is_comparable<RhsHead, RhsTail...>())
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

/* Allow for use with structured binding. */
namespace std {

    template<typename... Ts>
    struct tuple_size<frd::tuple<Ts...>> : frd::constant_holder<sizeof...(Ts)> { };

    /* Recursive case. */
    template<std::size_t I, typename Head, typename... Tail>
    struct tuple_element<I, frd::tuple<Head, Tail...>> : tuple_element<I - 1, frd::tuple<Tail...>> { };

    /* Base case. */
    template<typename Head, typename... Tail>
    struct tuple_element<0, frd::tuple<Head, Tail...>> : frd::type_holder<Head> { };

    template<typename T, typename Head, typename... Tail>
    constexpr T &get(frd::tuple<Head, Tail...> &t) noexcept {
        if constexpr (frd::same_as<T, Head>) {
            return t._head;
        } else {
            return std::get<T>(t._tail);
        }
    }

    template<typename T, typename Head, typename... Tail>
    constexpr const T &get(const frd::tuple<Head, Tail...> &t) noexcept {
        if constexpr (frd::same_as<T, Head>) {
            return t._head;
        } else {
            return std::get<T>(t._tail);
        }
    }

    template<typename T, typename Head, typename... Tail>
    constexpr T &&get(frd::tuple<Head, Tail...> &&t) noexcept {
        if constexpr (frd::same_as<T, Head>) {
            return frd::move(t._head);
        } else {
            return std::get<T>(std::move(t._tail));
        }
    }

    template<typename T, typename Head, typename... Tail>
    constexpr const T &&get(const frd::tuple<Head, Tail...> &&t) noexcept {
        if constexpr (frd::same_as<T, Head>) {
            return frd::move(t._head);
        } else {
            return std::get<T>(std::move(t._tail));
        }
    }

}
