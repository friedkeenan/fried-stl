#pragma once

#include <iterator>

#include <frd/bits/memory_base.hpp>

#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename It>
    concept _star_dereferenceable = requires(It &it) {
        { *it } -> referenceable;
    };

    template<_star_dereferenceable It>
    using iter_reference = decltype(*frd::declval<It &>());

    namespace unsafe {

        /*
            An implementation of primary 'std::iterator_traits'.

            As with other traits-types, it would be incorrect to use
            this as it is expected that you can specialize 'std::iterator_traits'.

            However, we use it to compare 'std::iterator_traits<It>' to our
            primary implementation to see if the former is *effectively*
            primary; that is to say whether it would have no effect were
            it specialized or not. We do this by comparing each field in the
            iterator traits.
        */

        template<typename It>
        concept _cpp17_iterator = copyable<It> && requires(It it) {
            { *it }   -> referenceable;
            { ++it }  -> same_as<It &>;
            { *it++ } -> referenceable;
        };

        template<typename It>
        concept _cpp17_input_iterator = (
            _cpp17_iterator<It>     &&
            equality_comparable<It> &&

            requires(It it) {
                typename std::incrementable_traits<It>::difference_type;
                typename std::indirectly_readable_traits<It>::value_type;

                typename std::common_reference_t<
                    iter_reference<It> &&,
                    typename std::indirectly_readable_traits<It>::value_type &
                >;

                typename std::common_reference_t<
                    decltype(*it++) &&,
                    typename std::indirectly_readable_traits<It>::value_type &
                >;

                requires signed_integral<typename std::incrementable_traits<It>::difference_type>;
            }
        );

        template<typename It>
        concept _cpp17_forward_iterator = (
            _cpp17_input_iterator<It> &&

            /* Slightly different from 'default_initializable'. */
            constructible_from<It> &&

            lvalue_reference<iter_reference<It>> &&

            same_as<
                remove_cvref<iter_reference<It>>,
                typename std::indirectly_readable_traits<It>::value_type
            > &&

            requires(It it) {
                { it++ }  -> convertible_to<const It &>;
                { *it++ } -> same_as<iter_reference<It>>;
            }
        );

        template<typename It>
        concept _cpp17_bidirectional_iterator = _cpp17_forward_iterator<It> && requires(It it) {
            { --it }  -> same_as<It &>;
            { it-- }  -> convertible_to<const It &>;
            { *it-- } -> same_as<iter_reference<It>>;
        };

        template<typename It>
        concept _cpp17_random_access_iterator = (
            _cpp17_bidirectional_iterator<It> &&
            totally_ordered<It>               &&

            requires(It it, typename std::incrementable_traits<It>::difference_type delta) {
                { it    += delta } -> same_as<It &>;
                { it    -= delta } -> same_as<It &>;
                { it    +  delta } -> same_as<It>;
                { delta +  it    } -> same_as<It>;
                { it    -  delta } -> same_as<It>;
                { it    -  it    } -> same_as<decltype(delta)>;

                { it[delta] } -> convertible_to<iter_reference<It>>;
            }
        );

        template<typename It>
        concept _has_category_value_difference_reference = requires {
            typename It::iterator_category;
            typename It::value_type;
            typename It::difference_type;
            typename It::reference;
        };

        template<typename T>
        using _direct_pointer = typename T::pointer;

        template<typename It>
        struct _iterator_traits { };

        template<_has_category_value_difference_reference It>
        struct _iterator_traits<It> {
            using iterator_category = typename It::iterator_category;
            using value_type        = typename It::value_type;
            using difference_type   = typename It::difference_type;
            using reference         = typename It::reference;

            using pointer = detected_else<void, _direct_pointer, It>;
        };

        template<_cpp17_input_iterator It>
        requires (!_has_category_value_difference_reference<It>)
        struct _iterator_traits<It> {
            using value_type      = typename std::indirectly_readable_traits<It>::value_type;
            using difference_type = typename std::incrementable_traits<It>::difference_type;

            template<typename T>
            using _arrow_deref_pointer = decltype(frd::declval<T &>().operator ->());

            using pointer = detected_else<
                detected_else<
                    void,

                    _arrow_deref_pointer, It
                >,

                _direct_pointer, It
            >;

            template<typename T>
            using _direct_reference = typename T::reference;

            using reference = detected_else<iter_reference<It>, _direct_reference, It>;

            template<typename T>
            using _direct_iter_category = typename T::iterator_category;

            using iterator_category = detected_else<
                conditional<
                    _cpp17_random_access_iterator<It>,

                    std::random_access_iterator_tag,
                    conditional<
                        _cpp17_bidirectional_iterator<It>,

                        std::bidirectional_iterator_tag,
                        conditional<
                            _cpp17_forward_iterator<It>,

                            std::forward_iterator_tag,
                            std::input_iterator_tag
                        >
                    >
                >,

                _direct_iter_category, It
            >;
        };

        template<_cpp17_iterator It>
        requires (!_cpp17_input_iterator<It> && !_has_category_value_difference_reference<It>)
        struct _iterator_traits<It> {
            using iterator_category = std::output_iterator_tag;
            using value_type        = void;
            using pointer           = void;
            using reference         = void;

            template<typename T>
            using _incrementable_difference = typename std::incrementable_traits<It>::difference_type;

            using difference_type = detected_else<void, _incrementable_difference, It>;
        };

        template<typename It>
        struct iterator_traits : _iterator_traits<It> {
            /* Something I wish the standard had. */
            using _primary_flag = void;
        };

    }

    template<typename Traits>
    concept _empty_iter_traits = (
        !requires { typename Traits::iterator_category; } &&
        !requires { typename Traits::value_type;        } &&
        !requires { typename Traits::difference_type;   } &&
        !requires { typename Traits::pointer;           } &&
        !requires { typename Traits::reference;         }
    );

    /*
        Check if every (used) field of 'std::iterator_traits<It>' is
        equivalent to our implementation of the primary template.
    */
    template<typename StdTraits, typename FrdTraits>
    concept _effectively_primary_iter_traits_impl = (
        (_empty_iter_traits<StdTraits> && _empty_iter_traits<FrdTraits>) ||

        (
            (
                same_as<typename StdTraits::iterator_category, typename FrdTraits::iterator_category> &&
                same_as<typename StdTraits::value_type,        typename FrdTraits::value_type>        &&
                same_as<typename StdTraits::difference_type,   typename FrdTraits::difference_type>   &&
                same_as<typename StdTraits::pointer,           typename FrdTraits::pointer>           &&
                same_as<typename StdTraits::reference,         typename FrdTraits::reference>
            ) &&

            /* 'std::iterator_traits<It>' can have an 'iterator_concept' member if specialized. */
            !requires {
                typename StdTraits::iterator_concept;
            }
        )
    );

    template<typename It>
    concept _effectively_primary_iter_traits = (
        _effectively_primary_iter_traits_impl<
            std::iterator_traits<It>,
            unsafe::iterator_traits<It>
        >
    );

    template<typename It>
    struct iterator_traits : public std::iterator_traits<It> { };

    template<_effectively_primary_iter_traits It>
    struct iterator_traits<It> : public std::iterator_traits<It> {
        /* Something I wish the standard had. */
        using _primary_flag = void;
    };

    /*
        The standard sometimes requires knowledge of whether 'std::iterator_traits<It>'
        is from the primary template.
    */
    template<typename It>
    concept primary_iterator_traits = requires {
        typename iterator_traits<remove_cvref<It>>::_primary_flag;
    };

    namespace _adl {

        /* Lookups for '_adl_iter_move'. */
        void iter_move() = delete;

        template<typename It>
        concept _adl_iter_move = adl_discoverable<It> && requires(It &&it) {
            iter_move(frd::forward<It>(it));
        };

    }

    template<typename It>
    concept _move_dereferenceable = requires(It &&it) {
        { *frd::forward<It>(it) } -> lvalue_reference;
        frd::move(*frd::forward<It>(it));
    };

    template<typename It>
    concept _rvalue_dereferenceable = requires(It &&it) {
        *frd::forward<It>(it);
        requires !reference<decltype(*frd::forward<It>(it))>;
    };

    /* Needs to be a callable object for ADL lookup to be checked. */
    struct _iter_move_fn {
        template<class It>
        requires (_adl::_adl_iter_move<It> || _move_dereferenceable<It> || _rvalue_dereferenceable<It>)
        constexpr decltype(auto) operator ()(It &&it) const
        noexcept(
            []() {
                if constexpr (_adl::_adl_iter_move<It>) {
                    return noexcept(iter_move(frd::forward<It>(it)));
                } else if constexpr (_move_dereferenceable<It>) {
                    return noexcept(frd::move(*frd::forward<It>(it)));
                } else {
                    return noexcept(*frd::forward<It>(it));
                }
            }()
        ) {
            if constexpr (_adl::_adl_iter_move<It>) {
                return iter_move(frd::forward<It>(it));
            } else if constexpr (_move_dereferenceable<It>) {
                return frd::move(*frd::forward<It>(it));
            } else {
                return *frd::forward<It>(it);
            }
        }
    };

    /* Needs to be in own namespace to avoid ADL conflicts. */
    namespace {

        constexpr inline _iter_move_fn iter_move;

    }

    /* Use these templates with 'detected_else' as we can't be sure these members exist. */
    template<typename T>
    using _iter_traits_value = typename iterator_traits<remove_cvref<T>>::value_type;

    template<typename T>
    using _iter_traits_difference = typename iterator_traits<remove_cvref<T>>::difference_type;

    template<typename It>
    using iter_value = conditional<
        !primary_iterator_traits<remove_cvref<It>>,

        detected_else<void, _iter_traits_value, It>,
        typename std::indirectly_readable_traits<remove_cvref<It>>::value_type
    >;

    template<typename It>
    using iter_difference = conditional<
        !primary_iterator_traits<remove_cvref<It>>,

        detected_else<void, _iter_traits_difference, It>,
        typename std::incrementable_traits<remove_cvref<It>>::difference_type
    >;

    template<typename It>
    concept _can_iter_rvalue_reference = _star_dereferenceable<It> && requires(It &it) {
        { ::frd::iter_move(it) } -> referenceable;
    };

    template<_can_iter_rvalue_reference It>
    using iter_rvalue_reference = decltype(frd::iter_move(frd::declval<It &>()));

    template<typename It>
    concept _has_sane_iter_difference = requires {
        typename iter_difference<It>;
        requires non_bool_signed_integral<iter_difference<It>>;
    };

    template<typename It>
    concept weakly_incrementable = (
        movable<It>                   &&
        _has_sane_iter_difference<It> &&

        requires(It it) {
            { ++it } -> same_as<It &>;
            it++;
        }
    );

    template<typename It>
    concept incrementable = regular<It> && weakly_incrementable<It> && requires(It it) {
        { it++ } -> same_as<It>;
    };

    template<typename It>
    concept weakly_decrementable = (
        movable<It>                   &&
        _has_sane_iter_difference<It> &&

        requires(It it) {
            { --it } -> same_as<It &>;
            it--;
        }
    );

    template<typename It>
    concept decrementable = regular<It> && weakly_decrementable<It> && requires(It it) {
        { it-- } -> same_as<It>;
    };

    template<typename In>
    concept _indirectly_readable = (
        requires(const In in) {
            typename iter_value<In>;
            typename iter_reference<In>;
            typename iter_rvalue_reference<In>;

            { *in } -> same_as<iter_reference<In>>;
            { iter_move(in) } -> same_as<iter_rvalue_reference<In>>;
        } &&
        common_reference_with<
            iter_reference<In> &&,
            iter_value<In> &
        > &&
        common_reference_with<
            iter_reference<In> &&,
            iter_rvalue_reference<In> &&
        > &&
        common_reference_with<
            iter_rvalue_reference<In> &&,
            const iter_value<In> &
        >
    );

    template<typename In>
    concept indirectly_readable = _indirectly_readable<remove_cvref<In>>;

    template<typename Out, typename Value>
    concept indirectly_writable = requires(Out &&out, Value &&value) {
                                                 *out  = frd::forward<Value>(value);
        const_cast<const iter_reference<Out> &&>(*out) = frd::forward<Value>(value);

                                                 *frd::forward<Out>(out)  = frd::forward<Value>(value);
        const_cast<const iter_reference<Out> &&>(*frd::forward<Out>(out)) = frd::forward<Value>(value);
    };

    template<typename In, typename Out>
    concept indirectly_copyable = indirectly_readable<In> && indirectly_writable<Out, iter_reference<In>>;

    template<typename In, typename Out>
    concept indirectly_movable = indirectly_readable<In> && indirectly_writable<Out, iter_rvalue_reference<In>>;

    template<typename In, typename Out>
    concept indirectly_movable_storable = (
        indirectly_movable<In, Out>                                   &&
        indirectly_writable<Out, iter_value<In>>                      &&
        movable<iter_value<In>>                                       &&
        constructible_from<iter_value<In>, iter_rvalue_reference<In>> &&
        assignable_from<iter_value<In>, iter_rvalue_reference<In>>
    );

    template<typename ItT, typename ItU = ItT>
    constexpr iter_value<ItT> iter_exchange_move(ItT &&it_t, ItU &&it_u)
    noexcept(
        nothrow_constructible_from<iter_value<ItT>, decltype(frd::iter_move(frd::forward<ItT>(it_t)))> &&
        noexcept(*it_t = frd::iter_move(frd::forward<ItU>(it_u)))
    ) {
        iter_value<ItT> old_value = frd::iter_move(frd::forward<ItT>(it_t));

        *it_t = frd::iter_move(frd::forward<ItU>(it_u));

        return old_value;
    }

    namespace _adl {

        /* Lookups for '_adl_iter_swap'. */
        void iter_swap(auto, auto) = delete;

        template<typename ItT, typename ItU>
        concept _adl_iter_swap = (
            (adl_discoverable<ItT> || adl_discoverable<ItU>) &&
            requires(ItT &&it_t, ItU &&it_u) {
                iter_swap(frd::forward<ItT>(it_t), frd::forward<ItU>(it_u));
            }
        );

    }

    template<typename ItT, typename ItU>
    concept _swap_deref_iter_swap = (
        indirectly_readable<ItT> &&
        indirectly_readable<ItU> &&
        swappable_with<
            iter_reference<ItT>,
            iter_reference<ItU>
        >
    );

    template<typename ItT, typename ItU>
    concept _exchange_iter_swap = indirectly_movable_storable<ItT, ItU> && indirectly_movable_storable<ItU, ItT>;

    /* Needs to be a callable object for ADL lookup to be checked. */
    struct _iter_swap_fn {
        template<typename ItT, typename ItU>
        requires (_adl::_adl_iter_swap<ItT, ItU> || _swap_deref_iter_swap<ItT, ItU> || _exchange_iter_swap<ItT, ItU>)
        constexpr void operator ()(ItT &&it_t, ItU &&it_u) const
        noexcept(
            []() {
                if constexpr (_adl::_adl_iter_swap<ItT, ItU>) {
                    return noexcept(iter_swap(frd::forward<ItT>(it_t), frd::forward<ItU>(it_u)));
                } else if constexpr (_swap_deref_iter_swap<ItT, ItU>) {
                    return noexcept(frd::swap(*frd::forward<ItT>(it_t), *frd::forward<ItU>(it_u)));
                } else {
                    return noexcept(frd::iter_exchange_move(frd::forward<ItT>(it_t), frd::forward<ItU>(it_u)));
                }
            }()
        ) {
            if constexpr (_adl::_adl_iter_swap<ItT, ItU>) {
                iter_swap(frd::forward<ItT>(it_t), frd::forward<ItU>(it_u));
            } else if constexpr (_swap_deref_iter_swap<ItT, ItU>) {
                frd::swap(*frd::forward<ItT>(it_t), *frd::forward<ItU>(it_u));
            } else {
                *frd::forward<ItT>(it_t) = frd::iter_exchange_move(frd::forward<ItU>(it_u), frd::forward<ItT>(it_t));
            }
        }
    };

    /* Needs to be in own namespace to avoid ADL conflicts. */
    namespace {

        constexpr inline _iter_swap_fn iter_swap;

    }

    template<typename ItT, typename ItU = ItT>
    concept indirectly_swappable = (
        indirectly_readable<ItT> &&
        indirectly_readable<ItU> &&

        requires(const ItT it_t, const ItU it_u) {
            ::frd::iter_swap(it_t, it_t);
            ::frd::iter_swap(it_u, it_u);
            ::frd::iter_swap(it_t, it_u);
            ::frd::iter_swap(it_u, it_t);
        }
    );

    /*
        Equivalent to 'std::input_or_output_iterator'.

        NOTE: Input and output iterators do not have any constraint
        on the return type of their post-increment, as they do
        not need to be copyable.
    */
    template<typename It>
    concept iterator = requires(It it) {
        { *it } -> referenceable;
    } && weakly_incrementable<It>;

    template<typename S, typename It>
    concept sentinel_for = semiregular<S> && iterator<It> && weakly_equality_comparable_with<S, It>;

    /*
        NOTE: It would be nice to shim `std::disable_sized_sentinel_for` and other
        range-related helper templates, but since they are variable templates and
        not types like 'std::tuple_size', I do not know of a way to do so while still
        also having our helper templates work with stuff which uses the STL templates.
    */

    template<typename S, typename It>
    concept sized_sentinel_for = (
        sentinel_for<S, It>                                           &&
        !std::disable_sized_sentinel_for<remove_cv<S>, remove_cv<It>> &&
        requires(const It &it, const S &s) {
            { s  - it } -> same_as<iter_difference<It>>;
            { it - s  } -> same_as<iter_difference<It>>;
        }
    );

    /*
        We get the associated "iterator concept" of an iterator by seeing if
        'iterator_traits<It>' is from the primary template, and if so we just
        use 'It' as an effective 'iterator_traits', otherwise we use the template.
        We then try to get 'traits::iterator_concept', or, failing that, we try to
        get 'traits::iterator_category'. If we can't do that either, then if
        'iterator_traits<It>' is generated from the primary template, we default to
        'std::random_access_iterator_tag', else 'void'.

        This is necessary for the iterator concepts so we can make 'contiguous_iterator'
        opt-in; If we only check iterator operations, many random access iterators, e.g.
        'reverse_iterator', will also satisy 'contiguous_iterator' despite that being
        incorrect. 'std::random_access_iterator_tag' is used as the default case since
        that is the highest order of input iterator besides 'contiguous_iterator', and
        since we still check iterator operations, iterators that land on that default
        case will still only satisfy 'random_access_iterator' if they also satisfy the
        needed operations.

        Additonally this allows iterators to opt out of being counted as a type
        of iterator. I personally don't find that super compelling, but I'm not
        on the standards committee.

        If I were to design this API myself I'd probably just make some
        'enable_contiguous_iterator' template and call it a day.

        NOTE: Our implementation is *slightly* different from the standard's as we only
        check if each value in 'iterator_traits<It>' is the same as the one generated
        from the primary template. This can lead to situations where 'iterator_traits<It>'
        has all the same values as the primary template, but 'It' doesn't have 'iterator_concept'
        or 'iterator_category' members, leading to 'iter_concept<It>' spitting out
        'std::random_access_iterator_tag', when the standard would use
        'iterator_traits<It>::iterator_category'. However, this deviance from the standard should
        only impact iterators opting out of being counted as e.g. a random access iterator,
        which as stated previously I do not find that use case compelling, and it's a small
        portion of said iterators as well. This does not lead to iterators being incorrectly
        marked as contiguous iterators, which is the main important thing for me.

        This issue also should not functionally affect 'iter_value' or 'iter_difference'.

        I *really* wish the standard just included some flag to say whether 'std::iterator_traits<It>'
        is from the primary template, or gave some API for us to test that ourselves.

        Alas, woe is me, woe is me.
    */

    template<typename It>
    using _iter_traits = conditional<primary_iterator_traits<It>, It, iterator_traits<It>>;

    template<typename It>
    struct _iter_concept {
        using traits = _iter_traits<It>;

        template<typename T>
        using _direct_concept = typename T::iterator_concept;

        /* Use this since we don't know 'traits' will have an 'iterator_category' member. */
        template<typename T>
        using _direct_category = typename T::iterator_category;

        /*
            NOTE: The standard here actually says that in the case that
            'traits::iterator_concept' is invalid, 'traits::iterator_category'
            is invalid, and 'std::iterator_traits<It>' is not from the primary
            template, then 'iter_concept' should not name a type. We just name
            'void', which should result in the same end behavior anyways.
        */
        using type = detected_else<
            detected_else<
                conditional<primary_iterator_traits<It>, std::random_access_iterator_tag, void>,

                _direct_category, traits
            >,

            _direct_concept, traits
        >;
    };

    template<typename It>
    using iter_concept = typename _iter_concept<It>::type;

    template<typename It>
    concept input_iterator = (
        iterator<It>            &&
        indirectly_readable<It> &&

        derived_from<iter_concept<It>, std::input_iterator_tag>
    );

    /* NOTE: Neither we nor the standard check iterator tags here. */
    template<typename It, typename Value>
    concept output_iterator = iterator<It> && indirectly_writable<It, Value> && requires(It it, Value &&value) {
        *it++ = frd::forward<Value>(value);
    };

    template<typename It>
    concept forward_iterator = (
        input_iterator<It>   &&
        incrementable<It>    &&
        sentinel_for<It, It> &&

        derived_from<iter_concept<It>, std::forward_iterator_tag>
    );

    template<typename It>
    concept bidirectional_iterator = (
        forward_iterator<It> &&
        decrementable<It>    &&

        derived_from<iter_concept<It>, std::bidirectional_iterator_tag>
    );

    template<typename It>
    concept random_access_iterator = (
        bidirectional_iterator<It> &&
        totally_ordered<It>        &&

        derived_from<iter_concept<It>, std::random_access_iterator_tag> &&

        requires(It it, const It const_it, const iter_difference<It> delta) {
            {       it += delta    } -> same_as<It &>;
            { const_it +  delta    } -> same_as<It>;
            { delta    +  const_it } -> same_as<It>;
            {       it -= delta    } -> same_as<It &>;
            { const_it -  delta    } -> same_as<It>;

            { const_it[delta] } -> same_as<iter_reference<It>>;
        }
    );

    template<typename It>
    concept contiguous_iterator = (
        random_access_iterator<It>                                &&
        lvalue_reference<iter_reference<It>>                      &&
        same_as<iter_value<It>, remove_cvref<iter_reference<It>>> &&

        derived_from<iter_concept<It>, std::contiguous_iterator_tag> &&

        requires(const It &it) {
            { frd::to_address(it) } -> same_as<add_pointer<iter_reference<It>>>;
        }
    );

    /*
        A dummy type that iterators can define equality with,
        used particularly with iterators that have all the information
        needed to know when they're at the end of their range.
    */
    struct default_sentinel_t { };

    constexpr inline default_sentinel_t default_sentinel{};

    struct unreachable_sentinel_t {
        /* TODO: Make this an object parameter method. */
        template<weakly_incrementable It>
        friend constexpr bool operator ==(unreachable_sentinel_t s, const It &it) noexcept {
            frd::discard(s, it);

            return false;
        }
    };

    constexpr inline unreachable_sentinel_t unreachable_sentinel{};

}
