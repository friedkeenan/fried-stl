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
    constexpr inline frd::size_t tuple_size = std::tuple_size<remove_reference<T>>::value;

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
    constexpr inline inert_t _has_tuple_elements;

    template<typename TupleLike, frd::size_t... Indices>
    constexpr inline bool _has_tuple_elements<TupleLike, frd::index_sequence<Indices...>> = (
        getable<TupleLike, Indices> && ...
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

    template<typename TupleLike, typename Sequence>
    struct _tuple_elements;

    template<typename TupleLike, frd::size_t... Indices>
    struct _tuple_elements<TupleLike, frd::index_sequence<Indices...>> {
        using type = type_list<tuple_element<Indices,TupleLike>...>;
    };

    template<tuple_like TupleLike>
    using tuple_elements = typename _tuple_elements<TupleLike, frd::make_index_sequence<tuple_size<TupleLike>>>::type;

    template<typename Invocable, typename TypeList>
    constexpr inline inert_t _invocable_with_tuple_like;

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

    template<frd::size_t I, frd::size_t N, typename Invocable, typename... TupleLikes>
    constexpr inline bool _invocable_for_each_tuple_element =
        []() {
            if constexpr (N == 0) {
                return true;
            } else if constexpr (N == 1) {
                return invocable<Invocable, forwarding_tuple_element<I, TupleLikes>...>;
            } else {
                constexpr bool can_invoke_for_index = invocable<Invocable &, forwarding_tuple_element<I, TupleLikes>...>;

                if constexpr (!can_invoke_for_index || I == N - 1) {
                    return can_invoke_for_index;
                } else {
                    return _invocable_for_each_tuple_element<I + 1, N, Invocable &, TupleLikes...>;
                }
            }
        }();

    /*
        We recursively increment the current index we're applying instead
        of using 'frd::index_sequence' as that would introduce ambiguity with
        expanding multiple parameter packs in one expression.
    */
    template<frd::size_t I, frd::size_t N, typename Invocable, typename... TupleLikes>
    constexpr void _apply_for_each(Invocable &&invocable, TupleLikes &&... tuple_likes)
    noexcept(
        []() {
            if constexpr (N == 1) {
                return noexcept(frd::invoke(frd::forward<Invocable>(invocable), frd::get<I>(frd::forward<TupleLikes>(tuple_likes))...));
            } else {
                constexpr auto noexcept_invoke_for_index = noexcept(
                    frd::invoke(invocable, frd::get<I>(frd::forward<TupleLikes>(tuple_likes))...)
                );

                if constexpr (!noexcept_invoke_for_index || I == N - 1) {
                    return noexcept_invoke_for_index;
                } else {
                    return noexcept(_apply_for_each<I + 1, N>(invocable, frd::forward<TupleLikes>(tuple_likes)...));
                }
            }
        }()
    ) {
        if constexpr (N == 1) {
            /*
                If the size of the tuple-likes is 1, we can safely forward on 'invocable'
                without worrying about use-after-forward issues since we're only using it once.
            */
            frd::invoke(frd::forward<Invocable>(invocable), frd::get<I>(frd::forward<TupleLikes>(tuple_likes))...);
        } else {
            frd::invoke(invocable, frd::get<I>(frd::forward<TupleLikes>(tuple_likes))...);

            if constexpr (I < N - 1) {
                _apply_for_each<I + 1, N>(invocable, frd::forward<TupleLikes>(tuple_likes)...);
            }
        }

    }

    template<
        typename Invocable,
        tuple_like HeadTupleLike,
        frd::size_t TupleSize = tuple_size<HeadTupleLike>,
        tuple_like_with_size<TupleSize>... TailTupleLikes
    >
    requires (_invocable_for_each_tuple_element<0, TupleSize, Invocable, HeadTupleLike, TailTupleLikes...>)
    constexpr void apply_for_each(Invocable &&invocable, HeadTupleLike &&head_tuple_like, TailTupleLikes &&... tail_tuple_likes)
    noexcept(
        []() {
            if constexpr (TupleSize == 0) {
                return true;
            } else {
                return noexcept(
                    _apply_for_each<0, TupleSize>(
                        frd::forward<Invocable>(invocable),
                        frd::forward<HeadTupleLike>(head_tuple_like),
                        frd::forward<TailTupleLikes>(tail_tuple_likes)...
                    )
                );
            }
        }()
    ) {
        if constexpr (TupleSize > 0) {
            _apply_for_each<0, tuple_size<HeadTupleLike>>(
                frd::forward<Invocable>(invocable),
                frd::forward<HeadTupleLike>(head_tuple_like),
                frd::forward<TailTupleLikes>(tail_tuple_likes)...
            );
        }
    }

    template<typename Invocable, typename... TupleLikes>
    concept applyable_for_each = requires(Invocable &&invocable, TupleLikes &&... tuple_likes) {
        ::frd::apply_for_each(frd::forward<Invocable>(invocable), frd::forward<TupleLikes>(tuple_likes)...);
    };

    template<typename Invocable, typename... TupleLikes>
    concept nothrow_applyable_for_each = (
        applyable_for_each<Invocable, TupleLikes...> &&

        noexcept(frd::apply_for_each(frd::declval<Invocable>(), frd::declval<TupleLikes>()...))
    );

    template<typename T, typename ElementsList>
    constexpr inline inert_t _constructible_from_tuple_elements;

    template<typename T, typename... Elements>
    constexpr inline bool _constructible_from_tuple_elements<T, type_list<Elements...>> = (
        constructible_from<T, Elements...>
    );

    template<typename T, typename TupleLike, frd::size_t... Indices>
    constexpr T _make_from_tuple(TupleLike &&tuple_like, frd::index_sequence<Indices...>) {
        return T(frd::get<Indices>(frd::forward<TupleLike>(tuple_like))...);
    }

    template<typename T, tuple_like TupleLike>
    requires (_constructible_from_tuple_elements<T, forwarding_tuple_elements<TupleLike>>)
    constexpr T make_from_tuple(TupleLike &&tuple_like) {
        return _make_from_tuple<T>(frd::forward<TupleLike>(tuple_like), frd::make_index_sequence<tuple_size<TupleLike>>{});
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
            /* 'static_cast' instead of moving for the case 'Element' is a lvalue reference. */

            return static_cast<Element &&>(this->_elem);
        }

        template<frd::size_t I>
        requires (I == ElementIndex)
        constexpr const Element &&get() const && noexcept {
            /* 'static_cast' instead of moving for the case 'Element' is a lvalue reference. */

            return static_cast<const Element &&>(this->_elem);
        }
    };

    /*
        We specialize the first two element holders of a tuple
        to have a 'first' and 'second' member so that we can have
        'pair<T, U>' just be an alias for 'tuple<T, U>' and
        reduce code duplication.
    */

    template<typename Element>
    struct _tuple_element_holder<0, Element> {
        [[no_unique_address]] Element first;

        /*
            Require that 'I' equals our index, so that the correct 'get'
            method will be called just through overload resolution.
        */

        template<frd::size_t I>
        requires (I == 0)
        constexpr Element &get() & noexcept {
            return this->first;
        }

        template<frd::size_t I>
        requires (I == 0)
        constexpr const Element &get() const & noexcept {
            return this->first;
        }

        template<frd::size_t I>
        requires (I == 0)
        constexpr Element &&get() && noexcept {
            /* 'static_cast' instead of moving for the case 'Element' is a lvalue reference. */

            return static_cast<Element &&>(this->first);
        }

        template<frd::size_t I>
        requires (I == 0)
        constexpr const Element &&get() const && noexcept {
            /* 'static_cast' instead of moving for the case 'Element' is a lvalue reference. */

            return static_cast<const Element &&>(this->first);
        }
    };

    template<typename Element>
    struct _tuple_element_holder<1, Element> {
        [[no_unique_address]] Element second;

        /*
            Require that 'I' equals our index, so that the correct 'get'
            method will be called just through overload resolution.
        */

        template<frd::size_t I>
        requires (I == 1)
        constexpr Element &get() & noexcept {
            return this->second;
        }

        template<frd::size_t I>
        requires (I == 1)
        constexpr const Element &get() const & noexcept {
            return this->second;
        }

        template<frd::size_t I>
        requires (I == 1)
        constexpr Element &&get() && noexcept {
            /* 'static_cast' instead of moving for the case 'Element' is a lvalue reference. */

            return static_cast<Element &&>(this->second);
        }

        template<frd::size_t I>
        requires (I == 1)
        constexpr const Element &&get() const && noexcept {
            /* 'static_cast' instead of moving for the case 'Element' is a lvalue reference. */

            return static_cast<const Element &&>(this->second);
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

    template<typename ElementsList, typename ElementsFwdList>
    constexpr inline inert_t _tuple_assignable_from;

    template<typename... Elements, typename... ElementsFwd>
    constexpr inline bool _tuple_assignable_from<type_list<Elements...>, type_list<ElementsFwd...>> = (
        assignable_from<Elements, ElementsFwd> && ...
    );

    template<typename ElementsList, typename ElementsFwdList>
    constexpr inline inert_t _nothrow_tuple_assignable_from;

    template<typename... Elements, typename... ElementsFwd>
    constexpr inline bool _nothrow_tuple_assignable_from<type_list<Elements...>, type_list<ElementsFwd...>> = (
        nothrow_assignable_from<Elements, ElementsFwd> && ...
    );

    template<typename ElementsList, typename ElementsFwdList>
    constexpr inline inert_t _tuple_swappable_with;

    template<typename... Elements, typename... ElementsFwd>
    constexpr inline bool _tuple_swappable_with<type_list<Elements...>, type_list<ElementsFwd...>> = (
        swappable_with<Elements, ElementsFwd> && ...
    );

    template<typename ElementsList, typename ElementsFwdList>
    constexpr inline inert_t _nothrow_tuple_swappable_with;

    template<typename... Elements, typename... ElementsFwd>
    constexpr inline bool _nothrow_tuple_swappable_with<type_list<Elements...>, type_list<ElementsFwd...>> = (
        nothrow_swappable_with<Elements, ElementsFwd> && ...
    );

    template<typename ElementsList, typename CmpElementsList>
    constexpr inline inert_t _tuple_comparable_with;

    template<typename... Elements, typename... CmpElements>
    constexpr inline bool _tuple_comparable_with<type_list<Elements...>, type_list<CmpElements...>> = (
        synthetic_three_way_comparable_with<Elements, CmpElements> && ...
    );

    template<typename ElementsList, typename CmpElementsList>
    constexpr inline inert_t _nothrow_tuple_comparable;

    template<typename... Elements, typename... CmpElements>
    constexpr inline bool _nothrow_tuple_comparable<type_list<Elements...>, type_list<CmpElements...>> = (
        nothrow_synthetic_three_way_comparable_with<Elements, CmpElements> && ...
    );

    template<typename Invocable, typename... ElementsFwd>
    concept _tuple_can_transform = (
        (sizeof...(ElementsFwd) <= 1 && (invocable<Invocable,   ElementsFwd> && ...)) ||
        (sizeof...(ElementsFwd) >  1 && (invocable<Invocable &, ElementsFwd> && ...))
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
        The same does not apply to the order of construction, which is
        well-defined by the standard.
    */
    template<typename... Elements>
    class tuple : public _tuple_base<Elements...> {
        public:
            static constexpr frd::size_t NumElements = sizeof...(Elements);

            /*
                NOTE: All our special member functions are implicitly
                defined, and therefore trivial if possible.

                TODO: Do we want to have an explicit assignment operator if we
                contain references, which implicitly deletes our assignment
                operator? It seems unsafe, but possibly desirable.
            */

            template<typename TupleLike, frd::size_t... Indices>
            constexpr void _assign(TupleLike &&tuple_like, frd::index_sequence<Indices...>) {
                ((this->template get<Indices>() = frd::get<Indices>(frd::forward<TupleLike>(tuple_like))), ...);
            }

            template<tuple_like_with_size<NumElements> TupleLike>
            requires (
                !same_as<remove_cvref<TupleLike>, tuple> &&

                _tuple_assignable_from<type_list<Elements &...>, forwarding_tuple_elements<TupleLike>>
            )
            constexpr tuple &operator =(TupleLike &&tuple_like)
            noexcept(
                _nothrow_tuple_assignable_from<type_list<Elements &...>, forwarding_tuple_elements<TupleLike>>
            ) {
                this->_assign(frd::forward<TupleLike>(tuple_like), frd::make_index_sequence<NumElements>{});

                return *this;
            }

            template<typename TupleLike, frd::size_t... Indices>
            constexpr void _swap(TupleLike &&tuple_like, frd::index_sequence<Indices...>) {
                (frd::swap(this->template get<Indices>(), frd::get<Indices>(frd::forward<TupleLike>(tuple_like))), ...);
            }

            template<tuple_like_with_size<NumElements> TupleLike>
            requires (_tuple_swappable_with<type_list<Elements &...>, forwarding_tuple_elements<TupleLike>>)
            constexpr void swap(TupleLike &&tuple_like)
            noexcept(
                _nothrow_tuple_swappable_with<type_list<Elements &...>, forwarding_tuple_elements<TupleLike>>
            ) {
                this->_swap(frd::forward<TupleLike>(tuple_like), frd::make_index_sequence<NumElements>{});
            }

            /* ADL-discovered swap. */
            template<tuple_like_with_size<NumElements> TupleLike>
            requires (_tuple_swappable_with<type_list<Elements &...>, forwarding_tuple_elements<TupleLike>>)
            friend constexpr void swap(tuple &lhs, TupleLike &&rhs)
            noexcept(
                noexcept(lhs.swap(frd::forward<TupleLike>(rhs)))
            ) {
                lhs.swap(frd::forward<TupleLike>(rhs));
            }

            /* Commutative ADL-discovered swap. */
            template<tuple_like_with_size<NumElements> TupleLike>
            requires (
                !template_specialization_of<remove_cvref<TupleLike>, tuple> &&

                _tuple_swappable_with<forwarding_tuple_elements<TupleLike>, type_list<Elements &...>>
            )
            friend constexpr void swap(TupleLike &&lhs, tuple &rhs)
            noexcept(
                noexcept(rhs.swap(frd::forward<TupleLike>(lhs)))
            ) {
                rhs.swap(frd::forward<TupleLike>(lhs));
            }

            template<typename Self, typename Invocable>
            static constexpr auto _transform(Self &&self, Invocable &&invocable)
            noexcept(
                []() {
                    using RealInvocable = conditional<NumElements <= 1, Invocable, Invocable &>;

                    return (
                        (nothrow_invocable<Invocable, match_cvref<Self, Elements>> && ...) &&

                        noexcept(
                            frd::decay_copy(
                                frd::declval<
                                    tuple<
                                        invoke_result<RealInvocable, match_cvref<Self, Elements>>...
                                    >
                                >()
                            )
                        )
                    );
                }()
            ) {
                using RealInvocable = conditional<NumElements <= 1, Invocable, Invocable &>;

                /*
                    Without specifying template arguments, the compiler assumes
                    we're constructing the current type we are in and ignores
                    template deduction.
                */
                using Return = tuple<invoke_result<RealInvocable, match_cvref<Self, Elements>>...>;

                return frd::apply([&]<typename... ElementsFwd>(ElementsFwd &&... elements) {
                    if constexpr (NumElements <= 1) {
                        return Return{frd::invoke(frd::forward<Invocable>(invocable), frd::forward<ElementsFwd>(elements))...};
                    } else {
                        return Return{frd::invoke(invocable, frd::forward<ElementsFwd>(elements))...};
                    }
                }, frd::forward<Self>(self));
            }

            template<typename Invocable>
            requires (_tuple_can_transform<Invocable, Elements &...>)
            constexpr auto transform(Invocable &&invocable) &
            noexcept(
                noexcept(_transform(*this, frd::forward<Invocable>(invocable)))
            ) {
                return _transform(*this, frd::forward<Invocable>(invocable));
            }

            template<typename Invocable>
            requires (_tuple_can_transform<Invocable, const Elements &...>)
            constexpr auto transform(Invocable &&invocable) const &
            noexcept(
                noexcept(_transform(*this, frd::forward<Invocable>(invocable)))
            ) {
                return _transform(*this, frd::forward<Invocable>(invocable));
            }

            template<typename Invocable>
            requires (_tuple_can_transform<Invocable, Elements &&...>)
            constexpr auto transform(Invocable &&invocable) &&
            noexcept(
                noexcept(_transform(frd::move(*this), frd::forward<Invocable>(invocable)))
            ) {
                return _transform(frd::move(*this), frd::forward<Invocable>(invocable));
            }

            template<typename Invocable>
            requires (_tuple_can_transform<Invocable, const Elements &&...>)
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

                if constexpr (I < NumElements - 1) {
                    if (cmp != 0) {
                        return cmp;
                    }

                    return this->template _compare_recurse<I + 1>(rhs);
                } else {
                    return cmp;
                }
            }

            template<tuple_like_with_size<NumElements> RhsTupleLike>
            requires (_tuple_comparable_with<type_list<const Elements &...>, forwarding_tuple_elements<const RhsTupleLike &>>)
            constexpr auto operator <=>(const RhsTupleLike &rhs) const
            noexcept(
                _nothrow_tuple_comparable<type_list<const Elements &...>, forwarding_tuple_elements<const RhsTupleLike &>>
            ) {
                return this->template _compare_recurse<0>(rhs);
            }

            template<tuple_like_with_size<NumElements> RhsTupleLike>
            requires (_tuple_comparable_with<type_list<const Elements &...>, forwarding_tuple_elements<const RhsTupleLike &>>)
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

    template<typename First, typename Second>
    using pair = tuple<First, Second>;

    template<typename... Args>
    constexpr tuple<Args &&...> forward_as_tuple(Args &&... args) noexcept {
        return {frd::forward<Args>(args)...};
    }

    template<typename... Args>
    constexpr tuple<Args &...> tie(Args &... args) noexcept {
        return {args...};
    }

    template<typename OldElementsList, typename NewElementsList>
    constexpr inline inert_t _tuple_convertible_to;

    template<typename... OldElements, typename... NewElements>
    constexpr inline bool _tuple_convertible_to<type_list<OldElements...>, type_list<NewElements...>> = (
        explicitly_convertible_to<OldElements, NewElements> && ...
    );

    template<typename OldElementsList, typename NewElementsList>
    constexpr inline inert_t _nothrow_tuple_convertible_to;

    template<typename... OldElements, typename... NewElements>
    constexpr inline bool _nothrow_tuple_convertible_to<type_list<OldElements...>, type_list<NewElements...>> = (
        nothrow_explicitly_convertible_to<OldElements, NewElements> && ...
    );

    template<typename... NewElements, tuple_like_with_size<sizeof...(NewElements)> OldTupleLike>
    requires (_tuple_convertible_to<forwarding_tuple_elements<OldTupleLike>, type_list<NewElements...>>)
    constexpr tuple<NewElements...> tuple_convert(OldTupleLike &&old_tuple_like)
    noexcept(
        _nothrow_tuple_convertible_to<forwarding_tuple_elements<OldTupleLike>, type_list<NewElements...>>
    ) {
        return frd::apply([]<typename... Elements>(Elements &&... elements) {
            return tuple{static_cast<NewElements>(frd::forward<Elements>(elements))...};
        }, frd::forward<OldTupleLike>(old_tuple_like));
    }

    template<typename... TupleLikes>
    struct _indices_for_head_tuple;

    template<typename HeadTupleLike, typename... TailTupleLikes>
    struct _indices_for_head_tuple<HeadTupleLike, TailTupleLikes...> : constant_holder<frd::make_index_sequence<tuple_size<HeadTupleLike>>{}> { };

    template<>
    struct _indices_for_head_tuple<> : constant_holder<frd::index_sequence<>{}> { };

    template<typename ReturnTuple, typename... TupleLikes>
    struct _tuple_cat;

    /*
        We separate the tuple-likes from the function call to
        allow multiple parameter packs in function parameters.
    */
    template<typename ReturnTuple, typename HeadTupleLike, typename... TailTupleLikes>
    struct _tuple_cat<ReturnTuple, HeadTupleLike, TailTupleLikes...> {
        template<frd::size_t... HeadIndices, typename... UnwrappedElements>
        static constexpr ReturnTuple concatenate(
            frd::index_sequence<HeadIndices...>,

            HeadTupleLike &&head_tuple_like,
            TailTupleLikes &&... tail_tuple_likes,

            UnwrappedElements &&... unwrapped_elements
        ) {
            return _tuple_cat<ReturnTuple, TailTupleLikes...>::concatenate(
                _indices_for_head_tuple<TailTupleLikes...>::value,

                frd::forward<TailTupleLikes>(tail_tuple_likes)...,

                frd::forward<UnwrappedElements>(unwrapped_elements)...,

                frd::get<HeadIndices>(frd::forward<HeadTupleLike>(head_tuple_like))...
            );
        }
    };

    template<typename ReturnTuple>
    struct _tuple_cat<ReturnTuple> {
        template<typename... UnwrappedElements>
        static constexpr ReturnTuple concatenate(frd::index_sequence<>, UnwrappedElements &&... unwrapped_elements) {
            return {frd::forward<UnwrappedElements>(unwrapped_elements)...};
        }
    };

    template<typename ElementsList, typename ElementsFwdList>
    constexpr inline inert_t _tuple_elements_forwardable;

    template<typename... Elements, typename... ElementsFwd>
    constexpr inline bool _tuple_elements_forwardable<type_list<Elements...>, type_list<ElementsFwd...>> = (
        convertible_to<ElementsFwd, Elements> && ...
    );

    template<typename ElementsList, typename ElementsFwdList>
    constexpr inline inert_t _nothrow_tuple_elements_forwardable;

    template<typename... Elements, typename... ElementsFwd>
    constexpr inline bool _nothrow_tuple_elements_forwardable<type_list<Elements...>, type_list<ElementsFwd...>> = (
        nothrow_convertible_to<ElementsFwd, Elements> && ...
    );

    template<
        tuple_like... TupleLikes,

        typename ElementsList    = type_list_concat<tuple_elements<TupleLikes>...>,
        typename ElementsFwdList = type_list_concat<forwarding_tuple_elements<TupleLikes>...>,

        typename ReturnTuple = template_from_type_list<tuple, ElementsList>
    >
    requires (_tuple_elements_forwardable<ElementsList, ElementsFwdList>)
    constexpr ReturnTuple tuple_cat(TupleLikes &&... tuple_likes)
    noexcept(
        (_nothrow_tuple_elements_forwardable<ElementsList, ElementsFwdList>)
    ) {
        return _tuple_cat<ReturnTuple, TupleLikes...>::concatenate(
            _indices_for_head_tuple<TupleLikes...>::value,

            frd::forward<TupleLikes>(tuple_likes)...
        );
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

}
