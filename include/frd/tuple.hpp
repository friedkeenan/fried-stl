#pragma once

#include <memory>
#include <tuple>

#include <frd/bits/arithmetic_base.hpp>

#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /* Can be specialized, which will also reflect in the STL template. */
    template<typename T>
    constexpr inline frd::size_t tuple_size = std::tuple_size<remove_cvref<T>>::value;

    template<typename T>
    concept _should_use_frd_tuple_size = incomplete<std::tuple_size<T>>;

    /* Specializations will have their 'type' static member reflected in the STL template. */
    template<frd::size_t I, typename T>
    struct tuple_element_holder;

    template<frd::size_t I, typename T>
    concept _should_use_frd_tuple_element = !incomplete<::frd::tuple_element_holder<I, T>>;

    template<frd::size_t I, typename T>
    using tuple_element = typename std::tuple_element<I, remove_reference<T>>::type;

    template<frd::size_t I, typename T>
    concept _member_get = requires(T &&t) {
        frd::forward<T>(t).template get<I>();
    };

    namespace _adl {

        /* Lookup for '_adl_get'. */
        template<frd::size_t>
        void get() = delete;

        template<frd::size_t I, typename T>
        concept _adl_get = adl_discoverable<T> && requires(T &&t) {
            get<I>(frd::forward<T>(t));
        };

    }

    /*
        Needs to be a callable object for ADL lookup to be checked.

        'I' needs to be a specifiable template parameter, and so
        cannot just be for the call operator, but rather the whole type.
    */
    template<frd::size_t I>
    struct _get_fn {
        template<typename Getable>
        requires (
            (I < tuple_size<Getable>)                               &&
            (_member_get<I, Getable> || _adl::_adl_get<I, Getable>)
        )
        constexpr decltype(auto) operator ()(Getable &&getable) const
        noexcept(
            []() {
                if constexpr (_member_get<I, Getable>) {
                    return noexcept(frd::forward<Getable>(getable).template get<I>());
                } else {
                    return noexcept(get<I>(frd::forward<Getable>(getable)));
                }
            }()
        ) {
            if constexpr (_member_get<I, Getable>) {
                return frd::forward<Getable>(getable).template get<I>();
            } else {
                return get<I>(frd::forward<Getable>(getable));
            }
        }
    };

    /* Needs to be in own namespace to avoid ADL conflicts. */
    namespace {

        template<frd::size_t I>
        constexpr inline _get_fn<I> get;

    }

    template<typename T, frd::size_t I = 0>
    concept getable = requires(T &&t) {
        ::frd::get<I>(frd::forward<T>(t));
    };

    template<typename T, frd::size_t I = 0>
    concept nothrow_getable = getable<T, I> && noexcept(frd::get<I>(frd::declval<T>()));

    template<typename T>
    concept pair_like = !reference<T> && requires(T t) {
        tuple_size<T>;
        tuple_size<T> == 2;

        typename tuple_element<0, remove_const<T>>;
        typename tuple_element<1, remove_const<T>>;

        { frd::get<0>(t) } -> convertible_to<const tuple_element<0, T> &>;
        { frd::get<1>(t) } -> convertible_to<const tuple_element<1, T> &>;
    };

    template<typename... Ts>
    class tuple;

    template<typename Head, typename... Tail>
    class tuple<Head, Tail...> {
        public:
            template<typename CmpHead, typename... CmpTail>
            static consteval bool _is_comparable() {
                return (
                    sizeof...(Tail) == sizeof...(CmpTail)
                ) && (
                    synthetic_three_way_comparable_with<Head, CmpHead>
                ) && ((
                    synthetic_three_way_comparable_with<Tail, CmpTail>
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

            /* TODO: 'swap' method. */

            /* TODO: Make sure we don't need lvalue/rvalue overloads. */
            template<frd::size_t I>
            constexpr decltype(auto) get() noexcept {
                if constexpr (I == 0) {
                    return this->_head;
                } else {
                    return this->_tail.template get<I - 1>();
                }
            }

            template<frd::size_t I>
            constexpr decltype(auto) get() const noexcept {
                if constexpr (I == 0) {
                    return this->_head;
                } else {
                    return this->_tail.template get<I - 1>();
                }
            }

            template<typename RhsHead, typename... RhsTail>
            requires (_is_comparable<RhsHead, RhsTail...>())
            constexpr auto operator <=>(const tuple<RhsHead, RhsTail...> &rhs) const
            noexcept(
                nothrow_synthetic_three_way_comparable_with<const Head &, const RhsHead &> &&
                noexcept(this->_tail <=> rhs._tail)
            ) {
                const auto cmp = frd::synthetic_three_way_compare(this->_head, rhs._head);

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

            constexpr Head &operator [](const frd::size_t index) noexcept requires SameTypes {
                return *(this->begin() + index);
            }

            constexpr const Head &operator [](const frd::size_t index) const noexcept requires SameTypes {
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

    template<typename... Ts>
    constexpr inline frd::size_t tuple_size<tuple<Ts...>> = sizeof...(Ts);

    /* Recursive case. */
    template<frd::size_t I, typename Head, typename... Tail>
    struct tuple_element_holder<I, tuple<Head, Tail...>> : tuple_element_holder<I - 1, tuple<Tail...>> { };

    /* Base case. */
    template<typename Head, typename... Tail>
    struct tuple_element_holder<0, tuple<Head, Tail...>> : type_holder<Head> { };

}

namespace std {

    /* Shims for compatibility between fried-stl and STL templates. */

    template<frd::_should_use_frd_tuple_size T>
    struct tuple_size<T> : frd::constant_holder<frd::tuple_size<T>> { };

    template<frd::size_t I, typename T>
    requires (frd::_should_use_frd_tuple_element<I, T>)
    struct tuple_element<I, T> : frd::tuple_element_holder<I, T> { };

}
