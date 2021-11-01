#pragma once

#include <tuple>

#include <frd/bits/arithmetic_base.hpp>
#include <frd/bits/functional_base.hpp>

#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename T>
    concept _bound_array_forwarder = bound_array<remove_reference<T>>;

    /* Should not be specialized. */
    template<typename T>
    constexpr inline frd::size_t tuple_size = []() {
        if constexpr (_bound_array_forwarder<T>) {
            return extent<remove_reference<T>>;
        } else {
            return std::tuple_size<remove_reference<T>>::value;
        }
    }();

    template<frd::size_t I, typename T>
    struct _tuple_element : std::tuple_element<I, T> { };

    template<frd::size_t I, typename T, frd::size_t N>
    requires (I < N)
    struct _tuple_element<I, T[N]> : type_holder<T> { };

    template<frd::size_t I, typename T>
    using tuple_element = typename _tuple_element<I, remove_reference<T>>::type;

    template<typename T>
    concept _has_tuple_size = _bound_array_forwarder<T> || requires {
        typename std::tuple_size<remove_cvref<T>>::type;
    };

    template<frd::size_t I, typename T>
    concept _has_tuple_element = (
        (_bound_array_forwarder<T> && I < extent<remove_reference<T>>) ||

        requires {
            typename std::tuple_element<I, remove_reference<T>>::type;
        }
    );

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
                (_bound_array_forwarder<Getable> || _member_get<I, Getable> || _adl_get<I, Getable>)
            )
            [[nodiscard]]
            constexpr decltype(auto) operator ()(Getable &&getable) const
            noexcept(
                []() {
                    if constexpr (_bound_array_forwarder<Getable>) {
                        return true;
                    } else if constexpr (_member_get<I, Getable>) {
                        return noexcept(frd::forward<Getable>(getable).template get<I>());
                    } else {
                        return noexcept(get<I>(frd::forward<Getable>(getable)));
                    }
                }()
            ) {
                if constexpr (_bound_array_forwarder<Getable>) {
                    return frd::forward<Getable>(getable)[I];
                } else if constexpr (_member_get<I, Getable>) {
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

    template<typename TupleLike>
    concept _has_tuple_elements = []<frd::size_t... Indices>(frd::index_sequence<Indices...>) {
        return (getable<TupleLike, Indices> && ...);
    }(frd::make_index_sequence<tuple_size<TupleLike>>{});

    template<typename T>
    concept tuple_like = _has_tuple_size<T> && _has_tuple_elements<T>;

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

    template<typename Invocable, typename TupleLike>
    concept _invocable_with_tuple_like = []<typename... Elements>(type_list<Elements...>) {
        return invocable<Invocable, Elements...>;
    }(forwarding_tuple_elements<TupleLike>{});

    template<typename Invocable, typename TupleLike, frd::size_t... Indices>
    constexpr decltype(auto) _apply(Invocable &&invocable, TupleLike &&tuple_like, frd::index_sequence<Indices...>)
    noexcept(
        nothrow_invocable<Invocable, forwarding_tuple_element<Indices, TupleLike>...>
    ) {
        return frd::invoke(frd::forward<Invocable>(invocable), frd::get<Indices>(frd::forward<TupleLike>(tuple_like))...);
    }

    template<typename Invocable, tuple_like TupleLike>
    requires (_invocable_with_tuple_like<Invocable, TupleLike>)
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

    /* TODO: When "Deducing this" is implemented, make this a concept with a recursive lambda. */
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

    template<typename T, typename TupleLike>
    concept _constructible_from_tuple_elements = []<typename... Elements>(type_list<Elements...>) {
        return constructible_from<T, Elements...>;
    }(forwarding_tuple_elements<TupleLike>{});

    template<typename T, typename TupleLike, frd::size_t... Indices>
    constexpr T _make_from_tuple(TupleLike &&tuple_like, frd::index_sequence<Indices...>) {
        return T(frd::get<Indices>(frd::forward<TupleLike>(tuple_like))...);
    }

    template<typename T, tuple_like TupleLike>
    requires (_constructible_from_tuple_elements<T, TupleLike>)
    constexpr T make_from_tuple(TupleLike &&tuple_like) {
        return _make_from_tuple<T>(frd::forward<TupleLike>(tuple_like), frd::make_index_sequence<tuple_size<TupleLike>>{});
    }

    template<frd::size_t ElementIndex, typename Element>
    struct _tuple_element_holder {
        using element = Element;

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
        using element = Element;

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
        using element = Element;

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

    /*
        In places we take a list of elements, that's because the method is implemented
        in '_tuple_common_impl', which is not a tuple-like class.
    */
    template<typename LhsElementsList, typename RhsTupleLike>
    concept _tuple_swappable_with = []<typename... LhsElements, typename... RhsElements>(type_list<LhsElements...>, type_list<RhsElements...>) {
        return (swappable_with<LhsElements, RhsElements> && ...);
    }(LhsElementsList{}, forwarding_tuple_elements<RhsTupleLike>{});

    template<typename LhsElementsList, typename RhsTupleLike>
    concept _nothrow_tuple_swappable_with = []<typename... LhsElements, typename... RhsElements>(type_list<LhsElements...>, type_list<RhsElements...>) {
        return (nothrow_swappable_with<LhsElements, RhsElements> && ...);
    }(LhsElementsList{}, forwarding_tuple_elements<RhsTupleLike>{});

    template<typename LhsElementsList, typename RhsTupleLike>
    concept _tuple_comparable_with = []<typename... LhsElements, typename... RhsElements>(type_list<LhsElements...>, type_list<RhsElements...>) {
        return (synthetic_three_way_comparable_with<LhsElements, RhsElements> && ...);
    }(LhsElementsList{}, forwarding_tuple_elements<RhsTupleLike>{});

    template<typename LhsElementsList, typename RhsTupleLike>
    concept _nothrow_tuple_comparable_with = []<typename... LhsElements, typename... RhsElements>(type_list<LhsElements...>, type_list<RhsElements...>) {
        return (nothrow_synthetic_three_way_comparable_with<LhsElements, RhsElements> && ...);
    }(LhsElementsList{}, forwarding_tuple_elements<RhsTupleLike>{});

    template<typename... ElementHolders>
    struct _combine_tuple_element_holders : ElementHolders... {
        static constexpr frd::size_t NumElements = sizeof...(ElementHolders);

        /*
            Combine all the element holders and pull in their
            'get' methods to participate in overload resolution.
        */

        using ElementHolders::get...;
    };

    template<typename... ElementHolders>
    struct _tuple_common_impl : _combine_tuple_element_holders<ElementHolders...> {
        using _combine_tuple_element_holders<ElementHolders...>::NumElements;

        template<typename TupleLike, frd::size_t... Indices>
        constexpr void _assign(TupleLike &&tuple_like, frd::index_sequence<Indices...>) {
            ((this->template get<Indices>() = frd::get<Indices>(frd::forward<TupleLike>(tuple_like))), ...);
        }

        template<typename TupleLike, frd::size_t... Indices>
        constexpr void _swap(TupleLike &&tuple_like, frd::index_sequence<Indices...>) {
            (frd::swap(this->template get<Indices>(), frd::get<Indices>(frd::forward<TupleLike>(tuple_like))), ...);
        }

        template<tuple_like_with_size<NumElements> TupleLike>
        requires (_tuple_swappable_with<type_list<typename ElementHolders::element &...>, TupleLike>)
        constexpr void swap(TupleLike &&tuple_like)
        noexcept(
            _nothrow_tuple_swappable_with<type_list<typename ElementHolders::element &...>, TupleLike>
        ) {
            this->_swap(frd::forward<TupleLike>(tuple_like), frd::make_index_sequence<NumElements>{});
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
        requires (_tuple_comparable_with<type_list<const typename ElementHolders::element &...>, const RhsTupleLike &>)
        constexpr auto operator <=>(const RhsTupleLike &rhs) const
        noexcept(
            _nothrow_tuple_comparable_with<type_list<const typename ElementHolders::element &...>, const RhsTupleLike &>
        ) {
            return this->template _compare_recurse<0>(rhs);
        }

        template<tuple_like_with_size<NumElements> RhsTupleLike>
        requires (_tuple_comparable_with<type_list<const typename ElementHolders::element &...>, const RhsTupleLike &>)
        constexpr bool operator ==(const RhsTupleLike &rhs) const
        noexcept(
            _nothrow_tuple_comparable_with<type_list<const typename ElementHolders::element &...>, const RhsTupleLike &>
        ) {
            /* Equality only checks members, so can only be as efficient as '<=>'. */

            return (*this <=> rhs) == 0;
        }
    };

    template<typename Sequence, typename... Elements>
    struct _make_tuple_base;

    template<frd::size_t... Indices, typename... Elements>
    struct _make_tuple_base<frd::index_sequence<Indices...>, Elements...> {
        using type = _tuple_common_impl<_tuple_element_holder<Indices, Elements>...>;
    };

    template<typename... Elements>
    struct _tuple_base : _make_tuple_base<frd::make_index_sequence<sizeof...(Elements)>, Elements...>::type { };

    template<typename LhsTupleLike, typename RhsTupleLike>
    concept _tuple_assignable_from = []<typename... LhsElements, typename... RhsElements>(type_list<LhsElements...>, type_list<RhsElements...>) {
        return (assignable_from<LhsElements, RhsElements> && ...);
    }(forwarding_tuple_elements<LhsTupleLike>{}, forwarding_tuple_elements<RhsTupleLike>{});

    template<typename LhsTupleLike, typename RhsTupleLike>
    concept _nothrow_tuple_assignable_from = []<typename... LhsElements, typename... RhsElements>(type_list<LhsElements...>, type_list<RhsElements...>) {
        return (nothrow_assignable_from<LhsElements, RhsElements> && ...);
    }(forwarding_tuple_elements<LhsTupleLike>{}, forwarding_tuple_elements<RhsTupleLike>{});

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
            using Base = _tuple_base<Elements...>;

            using Base::NumElements;

            /*
                NOTE: All our special member functions are implicitly
                defined, and therefore trivial if possible.
            */

            template<tuple_like_with_size<NumElements> TupleLike>
            requires (
                !same_as<remove_cvref<TupleLike>, tuple> &&

                _tuple_assignable_from<tuple &, TupleLike>
            )
            constexpr tuple &operator =(TupleLike &&tuple_like)
            noexcept(
                _nothrow_tuple_assignable_from<tuple &, TupleLike>
            ) {
                this->_assign(frd::forward<TupleLike>(tuple_like), frd::make_index_sequence<NumElements>{});

                return *this;
            }

            /* ADL-discovered swap. */
            template<tuple_like_with_size<NumElements> TupleLike>
            requires (_tuple_swappable_with<type_list<Elements &...>, TupleLike>)
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

                _tuple_swappable_with<type_list<Elements &...>, TupleLike>
            )
            friend constexpr void swap(TupleLike &&lhs, tuple &rhs)
            noexcept(
                noexcept(rhs.swap(frd::forward<TupleLike>(lhs)))
            ) {
                rhs.swap(frd::forward<TupleLike>(lhs));
            }
    };

    /*
        We have a separate specialization for when we contain a reference, since
        the assignment operator would be implciitly deleted in that case, but we
        need to propagate the assignment onto the references, which is needed for
        proper 'zip_view' iterator indirect assigment.
    */
    template<typename... Elements>
    requires (reference<Elements> || ...)
    class tuple<Elements...> : public _tuple_base<Elements...> {
        public:
            using Base = _tuple_base<Elements...>;

            using Base::NumElements;

            template<tuple_like_with_size<NumElements> TupleLike>
            requires (
                !same_as<remove_cvref<TupleLike>, tuple> &&

                _tuple_assignable_from<tuple &, TupleLike>
            )
            constexpr tuple &operator =(TupleLike &&tuple_like)
            noexcept(
                _nothrow_tuple_assignable_from<tuple &, TupleLike>
            ) {
                this->_assign(frd::forward<TupleLike>(tuple_like), frd::make_index_sequence<NumElements>{});

                return *this;
            }

            /*
                We need these separate assignment operators for the compiler not
                to complain that our base classes do not have assignment operators.
            */

            constexpr tuple &operator =(const tuple &rhs)
            noexcept(
                (nothrow_assignable_from<Elements &, const Elements &> && ...)
            )
            requires (
                assignable_from<Elements &, const Elements &> && ...
            ) {
                this->_assign(rhs, frd::make_index_sequence<NumElements>{});

                return *this;
            }

            constexpr tuple &operator =(tuple &&rhs)
            noexcept(
                (nothrow_assignable_from<Elements &, Elements &&> && ...)
            )
            requires (
                assignable_from<Elements &, Elements &&> && ...
            ) {
                this->_assign(frd::move(rhs), frd::make_index_sequence<NumElements>{});

                return *this;
            }

            /* ADL-discovered swap. */
            template<tuple_like_with_size<NumElements> TupleLike>
            requires (_tuple_swappable_with<type_list<Elements &...>, TupleLike>)
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

                _tuple_swappable_with<type_list<Elements &...>, TupleLike>
            )
            friend constexpr void swap(TupleLike &&lhs, tuple &rhs)
            noexcept(
                noexcept(rhs.swap(frd::forward<TupleLike>(lhs)))
            ) {
                rhs.swap(frd::forward<TupleLike>(lhs));
            }
    };

    /* Deduction guide for implicitly declared constructor. */
    template<typename... Ts>
    tuple(Ts...) -> tuple<Ts...>;

    template<typename First, typename Second>
    using pair = tuple<First, Second>;

    template<typename Invocable, typename TupleLike>
    concept _tuple_transformable_with = []<typename... Elements>(type_list<Elements...>) {
        using RealInvocable = conditional<sizeof...(Elements) <= 1, Invocable, Invocable &>;

        return (
            (invocable<RealInvocable, Elements> && ...) &&

            (possible_data_member<invoke_result<RealInvocable, Elements>> && ...)
        );
    }(forwarding_tuple_elements<TupleLike>{});

    template<typename Invocable, typename TupleLike>
    concept _nothrow_tuple_transformable_with = []<typename... Elements>(type_list<Elements...>) {
        using RealInvocable = conditional<sizeof...(Elements) <= 1, Invocable, Invocable &>;

        return (nothrow_invocable<RealInvocable, Elements> && ...);
    }(forwarding_tuple_elements<TupleLike>{});

    template<typename Invocable, typename ElementsList>
    struct _tuple_transform_result;

    template<typename Invocable, typename... Elements>
    struct _tuple_transform_result<Invocable, type_list<Elements...>> {
        using RealInvocable = conditional<sizeof...(Elements) <= 1, Invocable, Invocable &>;

        using type = tuple<invoke_result<RealInvocable, Elements>...>;
    };

    template<
        tuple_like TupleLike,
        typename Invocable,

        typename Result = typename _tuple_transform_result<Invocable, forwarding_tuple_elements<TupleLike>>::type
    >
    requires (_tuple_transformable_with<Invocable, TupleLike>)
    constexpr Result tuple_transform(TupleLike &&tuple_like, Invocable &&invocable)
    noexcept(
        _nothrow_tuple_transformable_with<Invocable, TupleLike>
    ) {
        return frd::apply([&]<typename... ElementsFwd>(ElementsFwd &&... elements) {
            if constexpr (tuple_size<TupleLike> <= 1) {
                return Result{frd::invoke(frd::forward<Invocable>(invocable), frd::forward<ElementsFwd>(elements))...};
            } else {
                return Result{frd::invoke(invocable, frd::forward<ElementsFwd>(elements))...};
            }
        }, frd::forward<TupleLike>(tuple_like));
    }

    template<typename... Args>
    constexpr tuple<Args &&...> forward_as_tuple(Args &&... args) noexcept {
        return {frd::forward<Args>(args)...};
    }

    template<typename... Args>
    constexpr tuple<Args &...> tie(Args &... args) noexcept {
        return {args...};
    }

    template<typename OldTupleLike, typename... NewElements>
    concept _tuple_convertible_to = []<typename... OldElements>(type_list<OldElements...>) {
        return (explicitly_convertible_to<OldElements, NewElements> && ...);
    }(forwarding_tuple_elements<OldTupleLike>{});

    template<typename OldTupleLike, typename... NewElements>
    concept _nothrow_tuple_convertible_to = []<typename... OldElements>(type_list<OldElements...>) {
        return (nothrow_explicitly_convertible_to<OldElements, NewElements> && ...);
    }(forwarding_tuple_elements<OldTupleLike>{});

    template<typename... NewElements, tuple_like_with_size<sizeof...(NewElements)> OldTupleLike>
    requires (_tuple_convertible_to<OldTupleLike, NewElements...>)
    constexpr tuple<NewElements...> tuple_convert(OldTupleLike &&old_tuple_like)
    noexcept(
        _nothrow_tuple_convertible_to<OldTupleLike, NewElements...>
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
        )
        noexcept(
            noexcept(
                _tuple_cat<ReturnTuple, TailTupleLikes...>::concatenate(
                    _indices_for_head_tuple<TailTupleLikes...>::value,

                    frd::forward<TailTupleLikes>(tail_tuple_likes)...,

                    frd::forward<UnwrappedElements>(unwrapped_elements)...,

                    frd::get<HeadIndices>(frd::forward<HeadTupleLike>(head_tuple_like))...
                )
            )
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
        static constexpr ReturnTuple concatenate(frd::index_sequence<>, UnwrappedElements &&... unwrapped_elements)
        noexcept(
            noexcept(ReturnTuple{frd::forward<UnwrappedElements>(unwrapped_elements)...})
        ) {
            return ReturnTuple{frd::forward<UnwrappedElements>(unwrapped_elements)...};
        }
    };

    template<typename ElementsList, typename ElementsFwdList>
    concept _tuple_elements_forwardable = []<typename... Elements, typename... ElementsFwd>(type_list<Elements...>, type_list<ElementsFwd...>) {
        return (convertible_to<ElementsFwd, Elements> && ...);
    }(ElementsList{}, ElementsFwdList{});

    template<
        tuple_like... TupleLikes,

        typename ElementsList    = type_list_concat<tuple_elements<TupleLikes>...>,
        typename ElementsFwdList = type_list_concat<forwarding_tuple_elements<TupleLikes>...>,

        typename ReturnTuple = template_from_type_list<tuple, ElementsList>
    >
    requires (_tuple_elements_forwardable<ElementsList, ElementsFwdList>)
    constexpr ReturnTuple tuple_cat(TupleLikes &&... tuple_likes)
    noexcept(
        noexcept(
            _tuple_cat<ReturnTuple, TupleLikes...>::concatenate(
                _indices_for_head_tuple<TupleLikes...>::value,

                frd::forward<TupleLikes>(tuple_likes)...
            )
        )
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
