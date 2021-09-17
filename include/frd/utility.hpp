#pragma once

#include <compare>
#include <utility>
#include <concepts>

#include <frd/bits/utility_base.hpp>
#include <frd/bits/concepts_base.hpp>

#include <frd/arithmetic.hpp>
#include <frd/type_traits.hpp>

namespace frd {

    template<typename T>
    requires (implicitly_convertible_to<T, decay<T>>)
    constexpr decay<T> decay_copy(T &&t) noexcept(nothrow_implicitly_convertible_to<T, decay<T>>) {
        return frd::forward<T>(t);
    }

    template<typename T, typename U>
    concept _std_swappable = requires(T &&t, U &&u) {
        std::swap(frd::forward<T>(t), frd::forward<U>(u));
    };

    namespace _adl {

        /* Lookup for '_adl_swap'. */
        template<typename T>
        void swap(T &, T &) = delete;

        template<typename T, typename U>
        concept _adl_swap = adl_discoverable<T> && requires(T &&t, U &&u) {
            swap(frd::forward<T>, frd::forward<U>);
        };

    }

    template<typename T, typename U>
    concept _normal_swappable = move_constructible<T> && assignable_from<T &, U &&> && assignable_from<U &, T &&>;

    /*
        A utility function that will use ADL-discovered swap, std::swap,
        or swap them manually, using no copies.

        TODO: Make implementation of 'std::ranges::swap'?
    */

    /* Needs to be a callable object for ADL lookup to be checked. */
    struct _swap_fn {
        template<typename LHS, typename RHS>
        requires (_std_swappable<LHS, RHS> || _adl::_adl_swap<LHS, RHS> || _normal_swappable<LHS, RHS>)
        constexpr void operator ()(LHS &&lhs, RHS &&rhs) const
        noexcept(
            []() {
                if constexpr (_std_swappable<LHS, RHS>) {
                    return noexcept(std::swap(frd::forward<LHS>(lhs), frd::forward<RHS>(rhs)));
                } else if constexpr (_adl::_adl_swap<LHS, RHS>) {
                    return noexcept(swap(frd::forward<LHS>(lhs), frd::forward<RHS>(rhs)));
                } else {
                    return (
                        noexcept(remove_cvref<LHS>(frd::move(lhs)))                    &&
                        noexcept(lhs = frd::move(rhs))                                 &&
                        noexcept(rhs = frd::move(frd::declval<remove_cvref<LHS> &>()))
                    );
                }
            }()
        ) {
            if constexpr (_std_swappable<LHS, RHS>) {
                std::swap(frd::forward<LHS>(lhs), frd::forward<RHS>(rhs));
            } else if constexpr (_adl::_adl_swap<LHS, RHS>) {
                swap(frd::forward<LHS>(lhs), frd::forward<RHS>(rhs));
            } else {
                remove_cvref<LHS> tmp_lhs = frd::move(lhs);
                lhs                       = frd::move(rhs);
                rhs                       = frd::move(tmp_lhs);
            }
        }
    };

    /* Needs to be in own namespace to avoid ADL conflicts. */
    namespace {

        constexpr inline _swap_fn swap;

    }

    template<typename T, typename U>
    concept swappable_with = requires(T &t, U &u) {
        swap(t, u);
    };

    template<typename T>
    concept swappable = swappable_with<T, T>;

    template<typename T, typename U>
    concept nothrow_swappable_with = swappable_with<T, U> && requires(T &t, U &u) {
        requires noexcept(swap(t, u));
    };

    template<typename T>
    concept nothrow_swappable = nothrow_swappable_with<T, T>;

    template<typename T>
    concept _member_get = requires(T &&t) {
        frd::forward<T>(t).template get<frd::size_t{0}>();
    };

    namespace _adl {

        /* Lookup for '_adl_get'. */
        template<frd::size_t>
        void get() = delete;

        template<typename T>
        concept _adl_get = adl_discoverable<T> && requires(T &&t) {
            get<frd::size_t{0}>(frd::forward<T>(t));
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
        requires (_member_get<Getable> || _adl::_adl_get<Getable>)
        constexpr decltype(auto) get(Getable &&getable) const
        noexcept(
            []() {
                if constexpr (_member_get<Getable>) {
                    return noexcept(frd::forward<Getable>(getable).template get<I>());
                } else {
                    return noexcept(get<I>(frd::forward<Getable>(getable)));
                }
            }()
        ) {
            if constexpr (_member_get<Getable>) {
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

    template<typename T>
    concept getable = requires(T &&t) {
        ::frd::get<frd::size_t{0}>(frd::forward<T>(t));
    };

    template<typename T>
    concept nothrow_getable = getable<T> && noexcept(frd::get<frd::size_t{0}>(frd::declval<T>()));

    template<integral T>
    constexpr make_signed<T> to_signed(const T value) noexcept {
        return static_cast<make_signed<T>>(value);
    }

    template<integral T>
    constexpr make_unsigned<T> to_unsigned(const T value) noexcept {
        return static_cast<make_unsigned<T>>(value);
    }

    template<typename LHS, typename RHS>
    requires (std::three_way_comparable_with<const LHS &, const RHS &> || weakly_less_than_comparable_with<const LHS &, const RHS &>)
    constexpr auto synthetic_three_way_compare(const LHS &lhs, const RHS &rhs) noexcept {
        if constexpr (std::three_way_comparable_with<const LHS &, const RHS &>) {
            return lhs <=> rhs;
        } else {
            if (lhs < rhs) {
                return std::weak_ordering::less;
            }

            if (rhs < lhs) {
                return std::weak_ordering::greater;
            }

            return std::weak_ordering::equivalent;
        }
    }

    template<frd::size_t... I>
    using index_sequence = std::integer_sequence<frd::size_t, I...>;

    /* Unfortunately requires compiler magic. */
    template<frd::size_t N>
    using make_index_sequence = std::make_integer_sequence<frd::size_t, N>;

}
