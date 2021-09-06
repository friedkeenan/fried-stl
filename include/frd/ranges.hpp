#pragma once

#include <iterator>

#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {


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
    concept weakly_decrementable = movable<It> && requires(It it) {
        typename iter_difference<It>;
        requires signed_integral<iter_difference<It>>;

        { --it } -> same_as<It &>;
        it--;
    };

    template<typename It>
    concept iterator = requires(It it) {
        { *it } -> referenceable;
    } && weakly_incrementable<It>;

    template<typename S, typename It>
    concept sentinel_for = semiregular<S> && iterator<It> && weakly_equality_comparable_with<S, It>;

    /* Map this back to STL when Clang can compile libstdc++ ranges header. */
    template<typename T>
    constexpr inline bool enable_borrowed_range = false; //std::ranges::enable_borrowed_range<T>;

    template<typename R>
    concept _maybe_borrowed_range = lvalue_reference<R> || enable_borrowed_range<remove_cvref<R>>;

    template<typename R>
    concept _member_begin = requires(R &&r) {
        frd::forward<R>(r).begin();
    };

    template<typename R>
    concept _adl_begin = (class_type<remove_reference<R>> || enum_type<remove_reference<R>>) &&
        requires(R &r) {
            { frd::decay_copy(begin(r)) } -> iterator;
        };

    template<_maybe_borrowed_range R>
    requires (bound_array<remove_reference<R>> || _member_begin<R> || _adl_begin<R>)
    constexpr auto begin(R &&r) {
        if constexpr (bound_array<remove_reference<R>>) {
            static_assert(lvalue_reference<R>, "Cannot return dangling iterator from array type!");

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
    concept _adl_end = (class_type<remove_reference<R>> || enum_type<remove_reference<R>>) &&
        requires(R &r) {
            { frd::decay_copy(end(r)) } -> sentinel_for<range_iterator<R>>;
        };

    template<_maybe_borrowed_range R>
    requires (bound_array<remove_reference<R>> || _member_end<R> || _adl_end<R>)
    constexpr auto end(R &&r) {
        if constexpr (bound_array<remove_reference<R>>) {
            static_assert(lvalue_reference<R>, "Cannot return dangling iterator from array type!");

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

}
