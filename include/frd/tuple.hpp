#pragma once

#include <tuple>

#include <frd/bits/arithmetic_base.hpp>
#include <frd/bits/functional_base.hpp>

#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /* Should not be specialized. */
    template<typename T>
    constexpr inline frd::size_t tuple_size = std::tuple_size<remove_cvref<T>>::value;

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

        /*
            Needs to be a callable object for ADL lookup to be checked.

            'I' needs to be a specifiable template parameter, and so
            cannot just be for the call operator, but rather the whole type.

            TODO: This needs to be in the '_adl' namespace to avoid a GCC bug.
            We should move it out when that bug is fixed.
        */
        template<frd::size_t I>
        struct _get_fn {
            template<typename Getable>
            requires (
                (I < tuple_size<Getable>)                               &&
                (_member_get<I, Getable> || _adl_get<I, Getable>)
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

    }

    /* Needs to be in own namespace to avoid ADL conflicts. */
    namespace {

        template<frd::size_t I>
        constexpr inline _adl::_get_fn<I> get;

    }

    template<typename T, frd::size_t I = 0>
    concept getable = requires(T &&t) {
        ::frd::get<I>(frd::forward<T>(t));
    };

    template<typename T, frd::size_t I = 0>
    concept nothrow_getable = getable<T, I> && noexcept(frd::get<I>(frd::declval<T>()));

    template<typename TupleLike, typename Sequence>
    constexpr inline inert_type _has_tuple_elements;

    template<typename TupleLike, frd::size_t... Indices>
    constexpr inline bool _has_tuple_elements<TupleLike, frd::index_sequence<Indices...>> = (
        requires(TupleLike &&tuple_like) {
            typename std::tuple_element<Indices, remove_reference<TupleLike>>::type;

            frd::get<Indices>(frd::forward<TupleLike>(tuple_like));
        } && ...
    );

    template<typename T>
    concept tuple_like = requires {
        std::tuple_size<T>::value;

        requires _has_tuple_elements<remove_reference<T>, frd::make_index_sequence<tuple_size<T>>>;
    };

    template<typename T>
    concept pair_like = tuple_like<T> && requires(T &&t) {
        { frd::get<0>(frd::forward<T>(t)) } -> convertible_to<const tuple_element<0, remove_reference<T>> &>;
        { frd::get<1>(frd::forward<T>(t)) } -> convertible_to<const tuple_element<1, remove_reference<T>> &>;
    };

    template<frd::size_t I, tuple_like TupleLike>
    using forwarding_tuple_element = decltype(frd::get<I>(frd::declval<TupleLike>()));

    template<typename TupleLike, typename Sequence>
    struct _forwarding_tuple_elements;

    template<typename TupleLike, frd::size_t... Indices>
    struct _forwarding_tuple_elements<TupleLike, frd::index_sequence<Indices...>> {
        using type = type_list<forwarding_tuple_element<Indices, TupleLike>...>;
    };

    template<tuple_like TupleLike>
    using forwarding_tuple_elements = typename _forwarding_tuple_elements<TupleLike, frd::make_index_sequence<tuple_size<TupleLike>>>::type;

    template<typename Invocable, typename TypeList>
    inline bool _invocable_with_tuple_like;

    template<typename Invocable, typename... Elements>
    constexpr inline bool _invocable_with_tuple_like<Invocable, type_list<Elements...>> = invocable<Invocable, Elements...>;

    template<typename Invocable, typename TupleLike, frd::size_t... Indices>
    constexpr decltype(auto) _apply(Invocable &&invocable, TupleLike &&tuple_like, frd::index_sequence<Indices...>)
    noexcept(
        nothrow_invocable<Invocable, forwarding_tuple_element<Indices, TupleLike>...>
    ) {
        return frd::invoke(frd::forward<Invocable>(invocable), frd::get<Indices>(frd::forward<TupleLike>(tuple_like))...);
    }

    template<typename Invocable, tuple_like TupleLike>
    requires (_invocable_with_tuple_like<Invocable, forwarding_tuple_elements<TupleLike>>)
    constexpr decltype(auto) apply(Invocable &&invocable, TupleLike &&tuple_like)
    noexcept(
        noexcept(_apply(frd::forward<Invocable>(invocable), frd::forward<TupleLike>(tuple_like), frd::make_index_sequence<tuple_size<TupleLike>>{}))
    ) {
        return _apply(frd::forward<Invocable>(invocable), frd::forward<TupleLike>(tuple_like), frd::make_index_sequence<tuple_size<TupleLike>>{});
    }

    template<frd::size_t ElementIndex, typename Element>
    struct _tuple_element_holder {
        [[no_unique_address]] Element _elem;

        /*
            Require that 'I' equals our index, so that the correct 'get'
            method will be called just through overload resolution.
        */

        template<frd::size_t I>
        requires (I == ElementIndex)
        constexpr Element &get() & noexcept {
            return this->_elem;
        }

        template<frd::size_t I>
        requires (I == ElementIndex)
        constexpr const Element &get() const & noexcept {
            return this->_elem;
        }

        template<frd::size_t I>
        requires (I == ElementIndex)
        constexpr Element &&get() && noexcept {
            return frd::move(this->_elem);
        }

        template<frd::size_t I>
        requires (I == ElementIndex)
        constexpr const Element &&get() const && noexcept {
            return frd::move(this->_elem);
        }
    };

    template<typename... ElementHolders>
    struct _combine_tuple_element_holders : ElementHolders... {
        /*
            Combine all the element holders and pull in their
            'get' methods to participate in overload resolution.
        */

        using ElementHolders::get...;
    };

    template<typename Sequence, typename... Elements>
    struct _make_tuple_base;

    template<frd::size_t... Indices, typename... Elements>
    struct _make_tuple_base<frd::index_sequence<Indices...>, Elements...> {
        using type = _combine_tuple_element_holders<_tuple_element_holder<Indices, Elements>...>;
    };

    template<typename... Elements>
    struct _tuple_base : _make_tuple_base<frd::make_index_sequence<sizeof...(Elements)>, Elements...>::type { };

    /*
        NOTE: The way we inherit from each base causes us to store
        the first element before the second element before the third,
        and so on. Both libstdc++ and libc++ store their elements in
        the opposite order for 'std::tuple', but not for 'std::pair'.

        Storing earlier elements earlier in the underlying data makes
        more sense to me than the reverse, and as far as I can tell the
        layout is not mandated by the standard, so we will keep it the way
        it currently is.
    */
    template<typename... Elements>
    class tuple : public _tuple_base<Elements...> {
        public:
            /*
                NOTE: All our special member functions are implicitly
                defined, and therefore trivial if possible.
            */

            /* TODO: 'swap' method and assignment operator. */

            template<typename... CmpElements>
            static consteval bool _is_comparable() {
                return (
                    (sizeof...(Elements) == sizeof...(CmpElements))                     &&
                    (synthetic_three_way_comparable_with<Elements, CmpElements> && ...)
                );
            }

            /* All requirements and noexcept-ness handled by '<=>' operator. */
            template<frd::size_t I, typename... RhsElements>
            constexpr auto _compare_recurse(const tuple<RhsElements...> &rhs) const {
                const auto cmp = frd::synthetic_three_way_compare(this->template get<I>(), rhs.template get<I>());

                if constexpr (I < sizeof...(Elements) - 1) {
                    if (cmp != 0) {
                        return cmp;
                    }

                    return this->template _compare_recurse<I + 1>(rhs);
                } else {
                    return cmp;
                }
            }

            template<typename... RhsElements>
            requires (_is_comparable<RhsElements...>())
            constexpr auto operator <=>(const tuple<RhsElements...> &rhs) const
            noexcept(
                (nothrow_synthetic_three_way_comparable_with<const Elements &, const RhsElements &> && ...)
            ) {
                return this->template _compare_recurse<0>(rhs);
            }

            template<typename... RhsElements>
            requires (_is_comparable<RhsElements...>())
            constexpr bool operator ==(const tuple<RhsElements...> &rhs) const
            noexcept(
                noexcept((*this <=> rhs) == 0)
            ) {
                /* Equality only checks members, so can only be as efficient as '<=>'. */

                return (*this <=> rhs) == 0;
            }
    };

    /* Deduction guide for implicitly declared constructor. */
    template<typename... Ts>
    tuple(Ts...) -> tuple<Ts...>;

}

namespace std {

    template<typename... Ts>
    struct tuple_size<frd::tuple<Ts...>> : frd::constant_holder<sizeof...(Ts)> { };

    /* Recursive case. */
    template<frd::size_t I, typename Head, typename... Tail>
    struct tuple_element<I, frd::tuple<Head, Tail...>> : tuple_element<I - 1, frd::tuple<Tail...>> { };

    /* Base case. */
    template<typename Head, typename... Tail>
    struct tuple_element<0, frd::tuple<Head, Tail...>> : frd::type_holder<Head> { };

}
