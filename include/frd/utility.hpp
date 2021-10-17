#pragma once

#include <compare>
#include <utility>
#include <concepts>

#include <frd/bits/utility_discard.hpp>
#include <frd/bits/utility_base.hpp>
#include <frd/bits/arithmetic_base.hpp>
#include <frd/bits/concepts_base.hpp>

#include <frd/type_traits.hpp>

namespace frd {

    template<typename T>
    requires (convertible_to<T, decay<T>>)
    constexpr decay<T> decay_copy(T &&t) noexcept(nothrow_convertible_to<T, decay<T>>) {
        return frd::forward<T>(t);
    }

    template<typename T, typename U = T>
    [[nodiscard]]
    constexpr T exchange(T &obj, U &&new_value)
    noexcept(
        nothrow_constructible_from<T, T &&> &&
        nothrow_assignable_from<T &, U>
    ) {
        T old_value = frd::move(obj);

        obj = frd::forward<U>(new_value);

        return old_value;
    }

    namespace _adl {

        /* Lookups for '_adl_swap'. */
        template<typename T>
        void swap(T &, T &) = delete;

        template<typename T, typename U>
        concept _adl_swap = requires(T &&t, U &&u) {
            swap(frd::forward<T>(t), frd::forward<U>(u));
        };

    }

    template<typename T, typename U>
    concept _normal_swappable = (
        move_constructible<remove_reference<T>>      &&
        assignable_from<T &, remove_reference<U> &&> &&
        assignable_from<U &, remove_reference<T>>
    );

    /*
        A utility function that will use ADL-discovered swap, std::swap,
        or swap them manually, using no copies.

        TODO: Make implementation of 'std::ranges::swap'?
    */

    /* Needs to be a callable object for ADL lookup to be checked. */
    struct _swap_fn {
        template<typename LHS, typename RHS>
        requires (_adl::_adl_swap<LHS, RHS> || _normal_swappable<LHS, RHS>)
        constexpr void operator ()(LHS &&lhs, RHS &&rhs) const
        noexcept(
            []() {
                if constexpr (_adl::_adl_swap<LHS, RHS>) {
                    return noexcept(swap(frd::forward<LHS>(lhs), frd::forward<RHS>(rhs)));
                } else {
                    return noexcept(rhs = frd::exchange(lhs, frd::move(rhs)));
                }
            }()
        ) {
            if constexpr (_adl::_adl_swap<LHS, RHS>) {
                swap(frd::forward<LHS>(lhs), frd::forward<RHS>(rhs));
            } else {
                rhs = frd::exchange(lhs, frd::move(rhs));
            }
        }
    };

    /* Needs to be in own namespace to avoid ADL conflicts. */
    namespace {

        constexpr inline _swap_fn swap;

    }

    template<typename T, typename U>
    concept swappable_with = requires(T &t, U &u) {
        ::frd::swap(t, u);
    };

    template<typename T>
    concept swappable = swappable_with<T, T>;

    template<typename T, typename U>
    concept nothrow_swappable_with = swappable_with<T, U> && requires(T &t, U &u) {
        requires noexcept(frd::swap(t, u));
    };

    template<typename T>
    concept nothrow_swappable = nothrow_swappable_with<T, T>;

    template<integral T>
    constexpr make_signed<T> to_signed(const T value) noexcept {
        return static_cast<make_signed<T>>(value);
    }

    template<integral T>
    constexpr make_unsigned<T> to_unsigned(const T value) noexcept {
        return static_cast<make_unsigned<T>>(value);
    }

    template<typename LHS, typename RHS>
    requires (std::three_way_comparable_with<LHS, RHS> || weakly_less_than_comparable_with<LHS, RHS>)
    constexpr auto synthetic_three_way_compare(LHS &&lhs, RHS &&rhs)
    noexcept(
        []() {
            if constexpr (std::three_way_comparable_with<LHS, RHS>) {
                return noexcept(frd::decay_copy(frd::forward<LHS>(lhs) <=> frd::forward<RHS>(rhs)));
            } else {
                return noexcept(lhs < rhs) && noexcept(rhs < lhs);
            }
        }()
    ) {
        if constexpr (std::three_way_comparable_with<LHS, RHS>) {
            return frd::forward<LHS>(lhs) <=> frd::forward<RHS>(rhs);
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

    template<typename LHS, typename RHS>
    concept synthetic_three_way_comparable_with = requires(LHS &&lhs, RHS &&rhs) {
        ::frd::synthetic_three_way_compare(frd::forward<LHS>(lhs), frd::forward<RHS>(rhs));
    };

    template<typename T>
    concept synthetic_three_way_comparable = synthetic_three_way_comparable_with<T, T>;

    template<typename LHS, typename RHS>
    concept nothrow_synthetic_three_way_comparable_with = synthetic_three_way_comparable_with<LHS, RHS> && (
        noexcept(frd::synthetic_three_way_compare(frd::declval<LHS>(), frd::declval<RHS>()))
    );

    template<typename T>
    concept nothrow_synthetic_three_way_comparable = nothrow_synthetic_three_way_comparable_with<T, T>;

    template<auto... Constants>
    struct constant_sequence { };

    /*
        NOTE: The standard requires specifying the type; we do not.

        TODO: Require these all to be the same type?
    */
    template<integral auto... Ints>
    using integer_sequence = constant_sequence<Ints...>;

    template<frd::size_t... Indices>
    using index_sequence = constant_sequence<Indices...>;

    template<auto N, auto... Ints>
    struct _make_integer_sequence : _make_integer_sequence<N - 1, N - 1, Ints...> { };

    template<auto N, auto... Ints>
    /* Cannot just put '0' in as 'N' as that will only specialize for 'int{0}'. */
    requires (N == 0)
    struct _make_integer_sequence<N, Ints...> {
        using type = constant_sequence<Ints...>;
    };

    template<integral auto N>
    using make_integer_sequence = typename _make_integer_sequence<N>::type;

    template<frd::size_t N>
    using make_index_sequence = make_integer_sequence<N>;

    /* An argument passed to remove ambiguity with copy/move constructors. */
    struct not_copy_move_tag_t { };

    constexpr inline not_copy_move_tag_t not_copy_move_tag{};

    template<typename Object>
    constexpr const Object &as_const(Object &obj) noexcept {
        return obj;
    }

    template<typename Object>
    void as_const(const Object &&) = delete;

}
