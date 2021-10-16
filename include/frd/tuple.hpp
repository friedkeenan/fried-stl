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

    template<typename T>
    concept _has_tuple_size = requires {
        typename std::tuple_size<remove_cvref<T>>::type;
    };

    template<frd::size_t I, typename T>
    concept _has_tuple_element = requires {
        typename std::tuple_element<I, remove_reference<T>>::type;
    };

    template<typename T, frd::size_t I, typename TupleLike>
    concept _valid_tuple_element = convertible_to<T, const tuple_element<I, TupleLike> &>;

    template<frd::size_t I, typename T>
    concept _member_get = requires(T &&t) {
        { frd::forward<T>(t).template get<I>() } -> _valid_tuple_element<I, T>;
    };

    namespace _adl {

        /* Lookup for '_adl_get'. */
        template<frd::size_t>
        void get() = delete;

        template<frd::size_t I, typename T>
        concept _adl_get = adl_discoverable<T> && requires(T &&t) {
            { get<I>(frd::forward<T>(t)) } -> _valid_tuple_element<I, T>;
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
                _has_tuple_size<Getable>       &&
                _has_tuple_element<I, Getable>

                (I < tuple_size<Getable>)                         &&
                (_member_get<I, Getable> || _adl_get<I, Getable>)
            )
            [[nodiscard]]
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
            frd::get<Indices>(frd::forward<TupleLike>(tuple_like));
        } && ...
    );

    template<typename T>
    concept tuple_like = _has_tuple_size<T> && _has_tuple_elements<T, frd::make_index_sequence<tuple_size<T>>>;

    template<typename T, frd::size_t Size>
    concept tuple_like_with_size = tuple_like<T> && tuple_size<T> == Size;

    template<typename T>
    concept pair_like = tuple_like_with_size<T, 2>;

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
    constexpr inline inert_type _invocable_with_tuple_like;

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

    template<typename Invocable, typename TupleLike>
    concept applyable = requires(Invocable &&invocable, TupleLike &&tuple_like) {
        ::frd::apply(frd::forward<Invocable>(invocable), frd::forward<TupleLike>(tuple_like));
    };

    template<typename Invocable, typename TupleLike>
    concept nothrow_applyable = (
        applyable<Invocable, TupleLike> &&

        noexcept(frd::apply(frd::declval<Invocable>(), frd::declval<TupleLike>()))
    );

    template<typename Invocable, typename ElementsList>
    constexpr inline inert_type _invocable_for_each_tuple_element;

    template<typename Invocable, typename... Elements>
    constexpr inline bool _invocable_for_each_tuple_element<Invocable, type_list<Elements...>> =
        []() {
            if constexpr (sizeof...(Elements) <= 1) {
                return (invocable<Invocable, Elements> && ...);
            } else {
                return (invocable<Invocable &, Elements> && ...);
            }
        }();

    template<typename Invocable, typename TupleLike, frd::size_t... Indices>
    constexpr void _apply_for_each(Invocable &&invocable, TupleLike &&tuple_like, frd::index_sequence<Indices...>)
    noexcept(
        []() {
            if constexpr (sizeof...(Indices) <= 1) {
                return noexcept((frd::invoke(frd::forward<Invocable>(invocable), frd::get<Indices>(frd::forward<TupleLike>(tuple_like))), ...));
            } else {
                return noexcept((frd::invoke(invocable, frd::get<Indices>(frd::forward<TupleLike>(tuple_like))), ...));
            }
        }()
    ) {
        /*
            If there are 0 or 1 elements in the tuple-like object,
            we can safely forward 'invocable' on as we won't be using
            it multiple times.
        */
        if constexpr (sizeof...(Indices) <= 1) {
            (frd::invoke(frd::forward<Invocable>(invocable), frd::get<Indices>(frd::forward<TupleLike>(tuple_like))), ...);
        } else {
            (frd::invoke(invocable, frd::get<Indices>(frd::forward<TupleLike>(tuple_like))), ...);
        }
    }


    /*
        TODO: Is it possible to have this take multiple tuples
        and have the invocable take multiple arguments? I don't
        believe it is since it would require parameter packs of
        parameter packs and even if that were possible there would
        be ambiguity with unpacking them with 'Indices'.
    */
    template<typename Invocable, tuple_like TupleLike>
    requires (_invocable_for_each_tuple_element<Invocable, forwarding_tuple_elements<TupleLike>>)
    constexpr void apply_for_each(Invocable &&invocable, TupleLike &&tuple_like)
    noexcept(
        []() {
            return noexcept(
                _apply_for_each(
                    frd::forward<Invocable>(invocable),
                    frd::forward<TupleLike>(tuple_like),

                    frd::make_index_sequence<tuple_size<TupleLike>>{}
                )
            );
        }()
    ) {
        _apply_for_each(
            frd::forward<Invocable>(invocable),
            frd::forward<TupleLike>(tuple_like),
            frd::make_index_sequence<tuple_size<TupleLike>>{}
        );
}

    template<typename Invocable, typename TupleLike>
    concept applyable_for_each = requires(Invocable &&invocable, TupleLike &&tuple_like) {
        ::frd::apply_for_each(frd::forward<Invocable>(invocable), frd::forward<TupleLike>(tuple_like));
    };

    template<typename Invocable, typename TupleLike>
    concept nothrow_applyable_for_each = (
        applyable_for_each<Invocable, TupleLike> &&

        noexcept(frd::apply_for_each(frd::declval<Invocable>(), frd::declval<TupleLike>()))
    );

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

    template<typename ElementsList, typename CmpElementsList>
    constexpr inline inert_type _is_tuple_comparable;

    template<typename... Elements, typename... CmpElements>
    constexpr inline bool _is_tuple_comparable<type_list<Elements...>, type_list<CmpElements...>> = (
        (sizeof...(Elements) == sizeof...(CmpElements)) &&

        (synthetic_three_way_comparable_with<Elements, CmpElements> && ...)
    );

    template<typename ElementsList, typename CmpElementsList>
    constexpr inline inert_type _nothrow_tuple_comparable;

    template<typename... Elements, typename... CmpElements>
    constexpr inline bool _nothrow_tuple_comparable<type_list<Elements...>, type_list<CmpElements...>> = (
        nothrow_synthetic_three_way_comparable_with<Elements, CmpElements> && ...
    );

    /*
        NOTE: The way we inherit from each base causes us to store
        the first element before the second element before the third,
        and so on. Both libstdc++ and libc++ store their elements in
        the opposite order for 'std::tuple', but not for 'std::pair'.

        Storing earlier elements earlier in the underlying data makes
        more sense to me than the reverse, and as far as I can tell the
        layout is not mandated by the standard, so we will keep it the way
        it currently is.

        Note also that the order of bases classes in the underlying data
        is not well-defined *by the standard*, but is well-defined by vendors.
        The same does not apply to the order of construction.
    */
    template<typename... Elements>
    class tuple : public _tuple_base<Elements...> {
        public:
            /*
                NOTE: All our special member functions are implicitly
                defined, and therefore trivial if possible.
            */

            /* TODO: 'swap' method and assignment operator. */

            template<typename Invocable, typename... ElementsFwd>
            static consteval bool _can_transform() {
                return (
                    (sizeof...(Elements) <= 1 && (invocable<Invocable,   ElementsFwd> && ...)) ||
                    (sizeof...(Elements) >  1 && (invocable<Invocable &, ElementsFwd> && ...))
                );
            }

            template<typename Self, typename Invocable>
            static constexpr auto _transform(Self &&self, Invocable &&invocable)
            noexcept(
                (
                    sizeof...(Elements) <= 1                        &&
                    (nothrow_invocable<Invocable, Elements> && ...) &&

                    noexcept(frd::decay_copy(frd::declval<tuple<invoke_result<Invocable, Elements>...>>()))
                ) ||
                (
                    sizeof...(Elements) > 1                           &&
                    (nothrow_invocable<Invocable &, Elements> && ...) &&

                    noexcept(frd::decay_copy(frd::declval<tuple<invoke_result<Invocable &, Elements>...>>()))
                )
            ) {
                return frd::apply([&]<typename... ElementsFwd>(ElementsFwd &&... elements) {
                    /* Without specifying template arguments, the compiler assumes the current type we are in. */
                    if constexpr (sizeof...(Elements) <= 1) {
                        return tuple<invoke_result<Invocable, Elements>...>{frd::invoke(frd::forward<Invocable>(invocable), frd::forward<ElementsFwd>(elements))...};
                    } else {
                        return tuple<invoke_result<Invocable &, Elements>...>{frd::invoke(invocable, frd::forward<ElementsFwd>(elements))...};
                    }
                }, frd::forward<Self>(self));
            }

            template<typename Invocable>
            requires (_can_transform<Invocable, Elements &...>())
            constexpr auto transform(Invocable &&invocable) &
            noexcept(
                noexcept(_transform(*this, frd::forward<Invocable>(invocable)))
            ) {
                return _transform(*this, frd::forward<Invocable>(invocable));
            }

            template<typename Invocable>
            requires (_can_transform<Invocable, const Elements &...>())
            constexpr auto transform(Invocable &&invocable) const &
            noexcept(
                noexcept(_transform(*this, frd::forward<Invocable>(invocable)))
            ) {
                return _transform(*this, frd::forward<Invocable>(invocable));
            }

            template<typename Invocable>
            requires (_can_transform<Invocable, Elements &&...>())
            constexpr auto transform(Invocable &&invocable) &&
            noexcept(
                noexcept(_transform(frd::move(*this), frd::forward<Invocable>(invocable)))
            ) {
                return _transform(frd::move(*this), frd::forward<Invocable>(invocable));
            }

            template<typename Invocable>
            requires (_can_transform<Invocable, const Elements &&...>())
            constexpr auto transform(Invocable &&invocable) const &&
            noexcept(
                noexcept(_transform(frd::move(*this), frd::forward<Invocable>(invocable)))
            ) {
                return _transform(frd::move(*this), frd::forward<Invocable>(invocable));
            }

            /* All requirements and noexcept-ness handled by '<=>' operator. */
            template<frd::size_t I, typename RhsTupleLike>
            constexpr auto _compare_recurse(const RhsTupleLike &rhs) const {
                const auto cmp = frd::synthetic_three_way_compare(this->template get<I>(), frd::get<I>(rhs));

                if constexpr (I < sizeof...(Elements) - 1) {
                    if (cmp != 0) {
                        return cmp;
                    }

                    return this->template _compare_recurse<I + 1>(rhs);
                } else {
                    return cmp;
                }
            }

            template<tuple_like_with_size<sizeof...(Elements)> RhsTupleLike>
            requires (_is_tuple_comparable<type_list<const Elements &...>, forwarding_tuple_elements<const RhsTupleLike &>>)
            constexpr auto operator <=>(const RhsTupleLike &rhs) const
            noexcept(
                _nothrow_tuple_comparable<type_list<const Elements &...>, forwarding_tuple_elements<const RhsTupleLike &>>
            ) {
                return this->template _compare_recurse<0>(rhs);
            }

            template<tuple_like_with_size<sizeof...(Elements)> RhsTupleLike>
            requires (_is_tuple_comparable<type_list<const Elements &...>, forwarding_tuple_elements<const RhsTupleLike &>>)
            constexpr bool operator ==(const RhsTupleLike &rhs) const
            noexcept(
                _nothrow_tuple_comparable<type_list<const Elements &...>, forwarding_tuple_elements<const RhsTupleLike &>>
            ) {
                /* Equality only checks members, so can only be as efficient as '<=>'. */

                return (*this <=> rhs) == 0;
            }
    };

    /* Deduction guide for implicitly declared constructor. */
    template<typename... Ts>
    tuple(Ts...) -> tuple<Ts...>;

    /* TODO: Code duplication isn't great. SHould we just make 'pair<T, U>' an alias for 'tuple<T, U>'? */
    template<typename First, typename Second>
    class pair {
        public:
            [[no_unique_address]] First  first;
            [[no_unique_address]] Second second;

            /*
                NOTE: For some reason we need forwarding references for the return
                of 'get' here instead of 'decltype(auto)'. I think it may be because
                we're returning members.
            */
            template<frd::size_t I, typename Self>
            static constexpr auto &&_get(Self &&self) noexcept {
                if constexpr (I == 0) {
                    return frd::forward<Self>(self).first;
                } else {
                    return frd::forward<Self>(self).second;
                }
            }

            template<frd::size_t I>
            constexpr auto &&get() & noexcept {
                return _get<I>(*this);
            }

            template<frd::size_t I>
            constexpr auto &&get() const & noexcept {
                return _get<I>(*this);
            }

            template<frd::size_t I>
            constexpr auto &&get() && noexcept {
                return _get<I>(frd::move(*this));
            }

            template<frd::size_t I>
            constexpr auto &&get() const && noexcept {
                return _get<I>(frd::move(*this));
            }

            template<
                typename Self,
                typename Invocable,
                typename FirstFwd  = match_cvref<Self, First>,
                typename SecondFwd = match_cvref<Self, Second>
            >
            static constexpr auto _transform(Self &&self, Invocable &&invocable)
            noexcept(
                nothrow_invocable<Invocable &, FirstFwd>  &&
                nothrow_invocable<Invocable &, SecondFwd> &&

                noexcept(
                    frd::decay_copy(
                        frd::declval<
                            pair<
                                invoke_result<Invocable &, FirstFwd>,
                                invoke_result<Invocable &, SecondFwd>
                            >
                        >()
                    )
                )
            ) {
                return (
                    pair<
                        invoke_result<Invocable &, FirstFwd>,
                        invoke_result<Invocable &, SecondFwd>
                    >{
                        frd::invoke(invocable, frd::forward<Self>(self).first),
                        frd::invoke(invocable, frd::forward<Self>(self).second)
                    }
                );
            }

            template<typename Invocable>
            requires (
                invocable<Invocable &, First  &> &&
                invocable<Invocable &, Second &>
            )
            constexpr auto transform(Invocable &&invocable) &
            noexcept(
                noexcept(_transform(*this, frd::forward<Invocable>(invocable)))
            ) {
                return _transform(*this, frd::forward<Invocable>(invocable));
            }

            template<typename Invocable>
            requires (
                invocable<Invocable &, const First  &> &&
                invocable<Invocable &, const Second &>
            )
            constexpr auto transform(Invocable &&invocable) const &
            noexcept(
                noexcept(_transform(*this, frd::forward<Invocable>(invocable)))
            ) {
                return _transform(*this, frd::forward<Invocable>(invocable));
            }

            template<typename Invocable>
            requires (
                invocable<Invocable &, First  &&> &&
                invocable<Invocable &, Second &&>
            )
            constexpr auto transform(Invocable &&invocable) &&
            noexcept(
                noexcept(_transform(frd::move(*this), frd::forward<Invocable>(invocable)))
            ) {
                return _transform(frd::move(*this), frd::forward<Invocable>(invocable));
            }

            template<typename Invocable>
            requires (
                invocable<Invocable &, const First  &&> &&
                invocable<Invocable &, const Second &&>
            )
            constexpr auto transform(Invocable &&invocable) const &&
            noexcept(
                noexcept(_transform(frd::move(*this), frd::forward<Invocable>(invocable)))
            ) {
                return _transform(frd::move(*this), frd::forward<Invocable>(invocable));
            }

            template<pair_like RhsPairLike>
            requires (_is_tuple_comparable<type_list<const First &, const Second &>, forwarding_tuple_elements<const RhsPairLike &>>)
            constexpr auto operator <=>(const RhsPairLike &rhs) const
            noexcept(
                _nothrow_tuple_comparable<type_list<const First &, const Second &>, forwarding_tuple_elements<const RhsPairLike &>>
            ) {
                const auto cmp_first = frd::synthetic_three_way_compare(this->first, frd::get<0>(rhs));

                if (cmp_first != 0) {
                    return cmp_first;
                }

                return frd::synthetic_three_way_compare(this->second, frd::get<1>(rhs));
            }

            template<pair_like RhsPairLike>
            constexpr bool operator ==(const RhsPairLike &rhs) const {
                return (*this <=> rhs) == 0;
            }
    };

    template<typename First, typename Second>
    pair(First, Second) -> pair<First, Second>;

    /* TODO: 'noexcept' for 'tuple_convert'. */
    template<typename... NewElements, tuple_like_with_size<sizeof...(NewElements)> OldTupleLike>
    requires (sizeof...(NewElements) != 2)
    constexpr tuple<NewElements...> tuple_convert(OldTupleLike &&old_tuple_like) {
        return frd::apply([]<typename... Elements>(Elements &&... elements) {
            return tuple<NewElements...>{frd::forward<Elements>(elements)...};
        }, frd::forward<OldTupleLike>(old_tuple_like));
    }

    template<typename NewFirst, typename NewSecond, pair_like OldPairLike>
    constexpr pair<NewFirst, NewSecond> tuple_convert(OldPairLike &&old_pair_like) {
        return frd::apply([]<typename First, typename Second>(First &&first, Second &&second) {
            return pair<NewFirst, NewSecond>{frd::forward<First>(first), frd::forward<Second>(second)};
        }, frd::forward<OldPairLike>(old_pair_like));
    }

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

    template<typename First, typename Second>
    struct tuple_size<frd::pair<First, Second>> : frd::constant_holder<frd::size_t{2}> { };

    template<typename First, typename Second>
    struct tuple_element<0, frd::pair<First, Second>> : frd::type_holder<First> { };

    template<typename First, typename Second>
    struct tuple_element<1, frd::pair<First, Second>> : frd::type_holder<Second> { };

}
