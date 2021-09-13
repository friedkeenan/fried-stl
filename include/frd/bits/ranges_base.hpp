#pragma once

#include <iterator>

#include <frd/memory.hpp>
#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename It>
    concept _star_dereferenceable = requires(It &it) {
        { *it } -> referenceable;
    };

    template<typename It>
    concept _adl_iter_move = adl_discoverable<It> && requires(It &&it) {
        iter_move(frd::forward<It>(it));
    };

    template<typename It>
    concept _move_dereferenceable = requires(It &&it) {
        { *frd::forward<It>(it) } -> lvalue_reference;
        frd::move(*frd::forward<It>(it));
    };

    template<typename It>
    concept _rvalue_dereferenceable = requires(It &&it) {
        { *frd::forward<It>(it) } -> rvalue_reference;
    };

    template<typename It>
    concept _nothrow_iter_move = (
        (_adl_iter_move<It>          && noexcept(iter_move(frd::declval<It>())))  ||
        (_move_dereferenceable<It>   && noexcept(frd::move(*frd::declval<It>()))) ||
        (_rvalue_dereferenceable<It> && noexcept(*frd::declval<It>()))
    );

    template<class It>
    requires (_adl_iter_move<It> || _move_dereferenceable<It> || _rvalue_dereferenceable<It>)
    constexpr decltype(auto) iter_move(It &&it) noexcept(_nothrow_iter_move<It>) {
        if constexpr (_adl_iter_move<It>) {
            return iter_move(frd::forward<It>(it));
        } else if constexpr (_move_dereferenceable<It>) {
            return frd::move(*frd::forward<It>(it));
        } else {
            return *frd::forward<It>(it);
        }
    }

    /*
        NOTE: for 'iter_value' and 'iter_difference', the standard gets
        these types in a slightly different way, only using 'std::iterator_traits<It>'
        if it's specialized. We use it simply if we can.
    */

    template<typename It>
    using _iter_value = typename std::iterator_traits<remove_cvref<It>>::value_type;

    template<typename It>
    using iter_value = detected_else<typename std::indirectly_readable_traits<remove_cvref<It>>::value_type, _iter_value, It>;

    template<_star_dereferenceable It>
    using iter_reference = decltype(*frd::declval<It &>());

    template<typename It>
    concept _can_iter_rvalue_reference = _star_dereferenceable<It> && requires(It &it) {
        { ::frd::iter_move(it) } -> referenceable;
    };

    template<_can_iter_rvalue_reference It>
    using iter_rvalue_reference = decltype(iter_move(frd::declval<It &>()));

    template<typename It>
    using _iter_difference = typename std::iterator_traits<remove_cvref<It>>::difference_type;

    template<typename It>
    using iter_difference = detected_else<typename std::incrementable_traits<remove_cvref<It>>::difference_type, _iter_difference, It>;

    template<typename It>
    concept weakly_incrementable = movable<It> && requires(It it) {
        typename iter_difference<It>;
        requires signed_integral<iter_difference<It>>;

        { ++it } -> same_as<It &>;
        it++;
    };

    template<typename It>
    concept incrementable = regular<It> && weakly_incrementable<It> && requires(It it) {
        { it++ } -> same_as<It>;
    };

    template<typename It>
    concept weakly_decrementable = movable<It> && requires(It it) {
        typename iter_difference<It>;
        requires signed_integral<iter_difference<It>>;

        { --it } -> same_as<It &>;
        it--;
    };

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

                                                 *frd::forward<Out>(out)    = frd::forward<Value>(value);
        const_cast<const iter_reference<Out> &&>(*frd::forward<Out>(out)) = frd::forward<Value>(value);
    };

    /* Equivalent to 'std::input_or_output_iterator'. */
    template<typename It>
    concept iterator = requires(It it) {
        { *it } -> referenceable;
    } && weakly_incrementable<It>;

    template<typename S, typename It>
    concept sentinel_for = semiregular<S> && iterator<It> && weakly_equality_comparable_with<S, It>;

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
        NOTE: The standard uses an invisible 'ITER_CONCEPT(It)' type alias
        that retrieves an iterator tag ('std::input_iterator_tag',
        'std::forward_iterator_tag', etc.) from an iterator using a DIFFERENT
        means from 'std::iterator_traits<It>::iterator_category' and checks in
        'std::input_iterator', 'std::forward_iterator', etc. that 'ITER_CONCEPT(It)'
        derives from the appropriate iterator tag while ALSO checking that all the
        required operations are valid, which can lead to INTENTIONAL cases where
        an iterator which does NOT satisfy 'std::random_access_iterator' can have
        'ITER_CONCEPT(It)' give 'std::random_access_iterator_tag' and I have ZERO
        clue why that is wanted or useful behavior when you can just not check the tags.
        Note also that 'std::output_iterator' does NOT use 'ITER_CONCEPT' at all and
        does not check iterator tags AT ALL. I have spent too much time trying to
        understand why the standard does this and so I am just not going to follow it
        here, and just check the operations.

        I'm not sure I could even make an equivalent to 'ITER_CONCEPT' without relying
        on implementation details of the STL since it requires knowing if
        'std::iterator_traits<It>' uses the primary template or not.
    */

    template<typename It>
    concept input_iterator = iterator<It> && indirectly_readable<It>;

    template<typename It, typename Value>
    concept output_iterator = iterator<It> && indirectly_writable<It, Value> && requires(It it, Value &&value) {
        *it++ = frd::forward<Value>(value);
    };

    template<typename It>
    concept forward_iterator = input_iterator<It> && incrementable<It> && sentinel_for<It, It>;

    template<typename It>
    concept bidirectional_iterator = forward_iterator<It> && decrementable<It>;

    template<typename It>
    concept random_access_iterator = bidirectional_iterator<It> && totally_ordered<It> &&
        requires(It it, const It const_it, const iter_difference<It> delta) {
            {       it += delta }    -> same_as<It &>;
            { const_it +  delta }    -> same_as<It>;
            { delta    +  const_it } -> same_as<It>;
            {       it -= delta }    -> same_as<It &>;
            { const_it -  delta }    -> same_as<It>;

            { const_it[delta] } -> same_as<iter_reference<It>>;
        };

    template<typename It>
    concept contiguous_iterator = (
        random_access_iterator<It>                                &&
        lvalue_reference<iter_reference<It>>                      &&
        same_as<iter_value<It>, remove_cvref<iter_reference<It>>> &&
        requires(const It &it) {
            { frd::to_address(it) } -> same_as<add_pointer<iter_reference<It>>>;
        }
    );

    /* Remove this in favor of STL when Clang can compile libstdc++ ranges header. */
    template<typename T>
    constexpr inline bool enable_borrowed_range = false;

    template<typename R>
    concept _maybe_borrowed_range = lvalue_reference<R> || enable_borrowed_range<remove_cvref<R>>;

    template<typename R>
    concept _range_array = bound_array<remove_reference<R>>;

    template<typename R>
    concept _member_begin = requires(R &&r) {
        frd::forward<R>(r).begin();
    };

    template<typename R>
    concept _adl_begin = adl_discoverable<R> && requires(R &r) {
        { frd::decay_copy(begin(r)) } -> iterator;
    };

    template<typename R>
    concept _nothrow_begin = (
        _range_array<R>                                           ||
        (_member_begin<R> && noexcept(frd::declval<R>().begin())) ||
        (_adl_begin<R>    && noexcept(begin(frd::declval<R>())))
    );

    template<_maybe_borrowed_range R>
    requires (_range_array<R> || _member_begin<R> || _adl_begin<R>)
    constexpr auto begin(R &&r) noexcept(_nothrow_begin<R>) {
        if constexpr (_range_array<R>) {
            return r + 0;
        } else if constexpr (_member_begin<R>) {
            return frd::forward<R>(r).begin();
        } else {
            return begin(frd::forward<R>(r));
        }
    }

    template<typename R>
    using range_iterator = decltype(begin(frd::declval<R &>()));

    template<typename R>
    concept _member_end = requires(R &&r) {
        frd::forward<R>(r).end();
    };

    template<typename R>
    concept _adl_end = adl_discoverable<R> && requires(R &r) {
        { frd::decay_copy(end(r)) } -> sentinel_for<range_iterator<R>>;
    };

    template<typename R>
    concept _nothrow_end = (
        _range_array<R>                                       ||
        (_member_end<R> && noexcept(frd::declval<R>().end())) ||
        (_adl_end<R>    && noexcept(end(frd::declval<R>())))
    );

    template<_maybe_borrowed_range R>
    requires (_range_array<R> || _member_end<R> || _adl_end<R>)
    constexpr auto end(R &&r) noexcept(_nothrow_end<R>) {
        if constexpr (_range_array<R>) {
            return r + extent<remove_reference<R>>;
        } else if constexpr (_member_end<R>) {
            return frd::forward<R>(r).end();
        } else {
            return end(frd::forward<R>(r));
        }
    }

    template<typename R>
    concept range = requires(R &r) {
        ::frd::begin(r);
        ::frd::end  (r);
    };

    template<range R>
    using range_sentinel = decltype(frd::end(frd::declval<R &>()));

    /* TODO: 'range_size'. */

    template<range R>
    using range_value = iter_value<range_iterator<R>>;

    template<range R>
    using range_reference = iter_reference<range_iterator<R>>;

    template<range R>
    using range_rvalue_reference = iter_rvalue_reference<range_iterator<R>>;

    template<range R>
    using range_difference = iter_difference<range_iterator<R>>;

    template<typename R>
    concept borrowed_range = range<R> && (
        lvalue_reference<R>                    ||
        enable_borrowed_range<remove_cvref<R>>
    );

    template<typename R>
    concept _member_data = requires(R &&r) {
        frd::forward<R>(r).data();
    };

    template<typename R>
    concept _contiguous_begin = contiguous_iterator<range_iterator<R>>;

    template<typename R>
    concept _nothrow_data = (
        _range_array<R>                                                                    ||
        (_member_data<R>      && noexcept(frd::declval<R>().data()))                       ||
        (_contiguous_begin<R> && noexcept(frd::to_address(frd::begin(frd::declval<R>()))))
    );

    template<borrowed_range R>
    requires (_range_array<R> || _member_data<R> || _contiguous_begin<R>)
    constexpr add_pointer<range_reference<R>> data(R &&r) noexcept(_nothrow_data<R>) {
        if constexpr (_range_array<R>) {
            return r + 0;
        } else if constexpr (_member_data<R>) {
            return frd::forward<R>(r).data();
        } else {
            static_assert(!incomplete<remove_all_extents<R>>);

            return frd::to_address(frd::begin(frd::forward<R>(r)));
        }
    }

    /* Equivalent to 'std::view_base'. */
    struct view_tag { };

    /* Remove 'enable_view' in favor of specializing STL when clang can compile libstdc++ ranges header. */
    template<typename R>
    constexpr inline bool enable_view = derived_from<R, view_tag>;

    template<typename R>
    concept view = range<R> && movable<R> && enable_view<R>;

    template<typename R>
    concept input_range = range<R> && input_iterator<range_iterator<R>>;

    template<typename R, typename Value>
    concept output_range = range<R> && output_iterator<range_iterator<R>, Value>;

    template<typename R>
    concept forward_range = input_range<R> && forward_iterator<range_iterator<R>>;

    template<typename R>
    concept bidirectional_range = forward_range<R> && bidirectional_iterator<range_iterator<R>>;

    template<typename R>
    concept random_access_range = bidirectional_range<R> && random_access_iterator<range_iterator<R>>;

    template<typename R>
    concept contiguous_range = (
        random_access_range<R> &&
        contiguous_iterator<range_iterator<R>> &&
        requires(R &r) {
            { ::frd::data(r) } -> same_as<add_pointer<range_reference<R>>>;
        }
    );

    template<typename R>
    concept _member_size = !std::ranges::disable_sized_range<remove_cv<R>> && requires(R &&r) {
        frd::forward<R>(r).size();
    };

    template<typename R>
    concept _adl_size = !std::ranges::disable_sized_range<remove_cv<R>> && adl_discoverable<R> &&
        requires(R &&r) {
            size(frd::forward<R>(r));
        };

    template<typename R>
    concept _iterator_size = forward_range<R> && sized_sentinel_for<range_sentinel<R>, range_iterator<R>>;

    template<typename R>
    concept _nothrow_size = (
        _range_array<R>                                                                                                ||
        (_member_size<R>   && noexcept(frd::declval<R>().size()))                                                      ||
        (_adl_size<R>      && noexcept(size(frd::declval<R>())))                                                       ||
        (_iterator_size<R> && noexcept(frd::to_unsigned(frd::end(frd::declval<R>()) - frd::begin(frd::declval<R>()))))
    );

    template<range R>
    requires (
        _range_array<R>   ||
        _member_size<R>   ||
        _adl_size<R>      ||
        _iterator_size<R>
    )
    constexpr auto size(R &&r) noexcept(_nothrow_size<R>) {
        if constexpr (_range_array<R>) {
            return extent<remove_reference<R>>;
        } else if constexpr (_member_size<R>) {
            return frd::forward<R>(r).size();
        } else if constexpr (_adl_size<R>) {
            return size(frd::forward<R>(r));
        } else {
            return frd::to_unsigned(frd::end(r) - frd::begin(r));
        }
    }

    template<typename R>
    concept sized_range = range<R> && requires(R &r) {
        ::frd::size(r);
    };

    template<sized_range R>
    using range_size = decltype(frd::size(frd::declval<R &>()));

    template<typename R>
    concept viewable_range = range<R> && (
        ( view<remove_cvref<R>> && constructible_from<remove_cvref<R>, R>) ||
        (!view<remove_cvref<R>> && borrowed_range<R>)
    );

    /*
        A dummy type that iterators can define equality with,
        used particularly with iterators that have all the information
        needed to know when they're at the end of their range.
    */
    struct default_sentinel_t { };
    constexpr inline default_sentinel_t default_sentinel;

}
