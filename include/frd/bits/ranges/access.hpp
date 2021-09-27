#pragma once

#include <iterator>
#include <ranges>

#include <frd/bits/ranges/iterators.hpp>

#include <frd/memory.hpp>
#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename R>
    concept _maybe_borrowed_range = lvalue_reference<R> || std::ranges::enable_borrowed_range<remove_cvref<R>>;

    template<typename R>
    concept _range_array = bound_array<remove_reference<R>>;

    template<typename R>
    concept _member_begin = requires(R &&r) {
        frd::forward<R>(r).begin();
    };

    namespace _adl {

        /* Lookups for '_adl_begin'. */
        void begin(auto &) = delete;
        void begin(const auto &) = delete;

        template<typename R>
        concept _adl_begin = adl_discoverable<R> && requires(R &r) {
            { frd::decay_copy(begin(r)) } -> iterator;
        };

    }

    /* Needs to be a callable object for ADL lookup to be checked. */
    struct _begin_fn {
        template<_maybe_borrowed_range R>
        requires (_range_array<R> || _member_begin<R> || _adl::_adl_begin<R>)
        constexpr auto operator ()(R &&r) const
        noexcept(
            []() {
                if constexpr (_range_array<R>) {
                    return true;
                } else if constexpr (_member_begin<R>) {
                    return noexcept(frd::decay_copy(frd::forward<R>(r).begin()));
                } else {
                    return noexcept(frd::decay_copy(begin(frd::forward<R>(r))));
                }
            }()
        ) {
            if constexpr (_range_array<R>) {
                return r + 0;
            } else if constexpr (_member_begin<R>) {
                return frd::forward<R>(r).begin();
            } else {
                return begin(frd::forward<R>(r));
            }
        }
    };

    /* Needs to be in own namespace to avoid ADL conflicts. */
    namespace {

        constexpr inline _begin_fn begin;

    }

    template<typename R>
    using range_iterator = decltype(frd::begin(frd::declval<R &>()));

    template<typename R>
    concept _member_end = requires(R &&r) {
        { frd::forward<R>(r).end() } -> sentinel_for<range_iterator<R>>;
    };

    namespace _adl {

        /* Lookups for '_adl_end'. */
        void end(auto &) = delete;
        void end(const auto &) = delete;

        template<typename R>
        concept _adl_end = adl_discoverable<R> && requires(R &r) {
            { frd::decay_copy(end(r)) } -> sentinel_for<range_iterator<R>>;
        };

    }

    /* Needs to be a callable object for ADL lookup to be checked. */
    struct _end_fn {
        template<_maybe_borrowed_range R>
        requires (_range_array<R> || _member_end<R> || _adl::_adl_end<R>)
        constexpr auto operator ()(R &&r) const
        noexcept(
            []() {
                if constexpr (_range_array<R>) {
                    return true;
                } else if constexpr(_member_end<R>) {
                    return noexcept(frd::decay_copy(frd::forward<R>(r).end()));
                } else {
                    return noexcept(frd::decay_copy(end(frd::forward<R>(r))));
                }
            }()
        ) {
            if constexpr (_range_array<R>) {
                return r + extent<remove_reference<R>>;
            } else if constexpr (_member_end<R>) {
                return frd::forward<R>(r).end();
            } else {
                return end(frd::forward<R>(r));
            }
        }
    };

    /* Needs to be in own namespace to avoid ADL conflicts. */
    namespace {

        constexpr inline _end_fn end;

    }

    template<typename R>
    concept range = requires(R &r) {
        ::frd::begin(r);
        ::frd::end  (r);
    };

    template<range R>
    using range_sentinel = decltype(frd::end(frd::declval<R &>()));

    template<range R>
    using range_value = iter_value<range_iterator<R>>;

    template<range R>
    using range_reference = iter_reference<range_iterator<R>>;

    template<range R>
    using range_rvalue_reference = iter_rvalue_reference<range_iterator<R>>;

    template<range R>
    using range_difference = iter_difference<range_iterator<R>>;

    template<typename R>
    concept common_range = range<R> && same_as<range_iterator<R>, range_sentinel<R>>;

    template<typename R>
    concept borrowed_range = range<R> && (
        lvalue_reference<R>                                 ||
        std::ranges::enable_borrowed_range<remove_cvref<R>>
    );

    template<typename R>
    concept _member_data = requires(R &&r) {
        { frd::forward<R>(r).data() } -> same_as<add_pointer<range_value<R>>>;
    };

    template<typename R>
    concept _contiguous_begin = contiguous_iterator<range_iterator<R>>;

    template<borrowed_range R>
    requires (_range_array<R> || _member_data<R> || _contiguous_begin<R>)
    constexpr auto data(R &&r)
    noexcept(
        []() {
            if constexpr (_range_array<R>) {
                return true;
            } else if constexpr (_member_data<R>) {
                return noexcept(frd::decay_copy(frd::forward<R>(r).data()));
            } else {
                return noexcept(frd::to_address(frd::begin(frd::forward<R>(r))));
            }
        }()
    ) {
        if constexpr (_range_array<R>) {
            return r + 0;
        } else if constexpr (_member_data<R>) {
            return frd::forward<R>(r).data();
        } else {
            static_assert(!incomplete<remove_all_extents<R>>);

            return frd::to_address(frd::begin(frd::forward<R>(r)));
        }
    }

    /*
        Equivalent to 'std::view_base'.

        'std::ranges::enable_view' is specialized further down to be true
        for types that derive from `view_tag`.
    */
    struct view_tag { };

    template<typename R>
    concept view = range<R> && movable<R> && std::ranges::enable_view<R>;

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
        { frd::forward<R>(r).size() } -> integral;
    };

    namespace _adl {

        /* Lookups for '_adl_size'. */
        void size(auto &) = delete;
        void size(const auto &) = delete;

        template<typename R>
        concept _adl_size = !std::ranges::disable_sized_range<remove_cv<R>> && adl_discoverable<R> &&
            requires(R &&r) {
                { frd::decay_copy(size(frd::forward<R>(r))) } -> integral;
            };

    }

    template<typename R>
    concept _iterator_size = forward_range<R> && sized_sentinel_for<range_sentinel<R>, range_iterator<R>>;

    /* Needs to be a callable object for ADL lookup to be checked. */
    struct _size_fn {
        template<range R>
        requires (
            _range_array<R>    ||
            _member_size<R>    ||
            _adl::_adl_size<R> ||
            _iterator_size<R>
        )
        constexpr auto operator ()(R &&r) const
        noexcept(
            []() {
                if constexpr (_range_array<R>) {
                    return true;
                } else if constexpr(_member_size<R>) {
                    return noexcept(frd::decay_copy(frd::forward<R>(r).size()));
                } else if constexpr (_adl::_adl_size<R>) {
                    return noexcept(frd::decay_copy(size(frd::forward<R>(r))));
                } else {
                    return noexcept(frd::decay_copy(frd::end(r) - frd::begin(r)));
                }
            }()
        ) {
            if constexpr (_range_array<R>) {
                return extent<remove_reference<R>>;
            } else if constexpr (_member_size<R>) {
                return frd::forward<R>(r).size();
            } else if constexpr (_adl::_adl_size<R>) {
                return size(frd::forward<R>(r));
            } else {
                return frd::to_unsigned(frd::end(r) - frd::begin(r));
            }
        }
    };

    /* Needs to be in own namespace to avoid ADL conflicts. */
    namespace {

        constexpr inline _size_fn size;

    }

    template<typename R>
    concept sized_range = range<R> && requires(R &r) {
        ::frd::size(r);
    };

    template<sized_range R>
    using range_size = decltype(frd::size(frd::declval<R &>()));

    template<typename R>
    concept _member_empty = requires(R &&r) {
        static_cast<bool>(frd::forward<R>(r).empty());
    };

    template<typename R>
    concept _size_empty = requires(R &&r) {
        frd::size(frd::forward<R>(r)) == 0;
    };

    template<typename R>
    concept _iterator_empty = requires(R &&r) {
        requires forward_iterator<decltype(frd::begin(r))>;

        static_cast<bool>(frd::begin(r) == frd::end(r));
    };

    template<range R>
    requires (_member_empty<R> || _size_empty<R> || _iterator_empty<R>)
    [[nodiscard]]
    constexpr bool empty(R &&r)
    noexcept(
        []() {
            if constexpr (_member_empty<R>) {
                return noexcept(static_cast<bool>(frd::forward<R>(r).empty()));
            } else if constexpr (_size_empty<R>) {
                return noexcept(frd::size(frd::forward<R>(r)) == 0);
            } else {
                return noexcept(static_cast<bool>(frd::begin(r) == frd::end(r)));
            }
        }()
    ) {
        if constexpr (_member_empty<R>) {
            return static_cast<bool>(frd::forward<R>(r).empty());
        } else if constexpr (_size_empty<R>) {
            return frd::size(frd::forward<R>(r)) == 0;
        } else {
            return static_cast<bool>(frd::begin(r) == frd::end(r));
        }
    }

    template<typename R>
    concept possibly_empty_range = range<R> && requires(R &&r) {
        frd::empty(frd::forward<R>(r));
    };

    template<typename R>
    concept viewable_range = range<R> && (
        ( view<remove_cvref<R>> && constructible_from<remove_cvref<R>, R>) ||
        (!view<remove_cvref<R>> && borrowed_range<R>)
    );

    template<range R>
    using borrowed_iterator = conditional<borrowed_range<R>, range_iterator<R>, inert_type>;

}

namespace std::ranges {

    template<frd::derived_from<frd::view_tag> R>
    constexpr inline bool enable_view<R> = true;

}
