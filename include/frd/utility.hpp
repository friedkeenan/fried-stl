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

    template<typename T, typename U>
    concept _adl_swappable = adl_discoverable<T> && requires(T &&t, U &&u) {
        swap(frd::forward<T>, frd::forward<U>);
    };

    template<typename T, typename U>
    concept _normal_swappable = move_constructible<T> && assignable_from<T &, U &&> && assignable_from<U &, T &&>;

    template<typename T, typename U>
    concept _nothrow_swap = (
        (_std_swappable<T, U> && noexcept(std::swap(frd::declval<T>(), frd::declval<U>()))) ||
        (_adl_swappable<T, U> && noexcept(     swap(frd::declval<T>(), frd::declval<U>()))) ||
        (_normal_swappable<T, U> && (
            noexcept(T(frd::move(frd::declval<T>())))                                  &&
            noexcept(frd::declval<T &>() = frd::move(frd::declval<U>()))               &&
            noexcept(frd::declval<U &>() = frd::move(frd::declval<remove_cvref<T>>()))
        ))
    );

    /*
        A utility function that will use ADL-discovered swap, std::swap,
        or swap them manually, using no copies.

        TODO: Make implementation of 'std::ranges::swap'?
    */
    template<typename LHS, typename RHS>
    requires (_std_swappable<LHS, RHS> || _adl_swappable<LHS, RHS> || _normal_swappable<LHS, RHS>)
    constexpr void swap(LHS &&lhs, RHS &&rhs) noexcept(_nothrow_swap<LHS, RHS>) {
        if constexpr (_std_swappable<LHS, RHS>) {
            std::swap(frd::forward<LHS>(lhs), frd::forward<RHS>(rhs));
        } else if constexpr (_adl_swappable<LHS, RHS>) {
            swap(frd::forward<LHS>(lhs), frd::forward<RHS>(rhs));
        } else {
            remove_cvref<LHS> tmp_lhs = frd::move(lhs);
            lhs                       = frd::move(rhs);
            rhs                       = frd::move(tmp_lhs);
        }
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
    concept _member_getable = requires(T &&t) {
        frd::forward<T>(t).template get<frd::size_t{0}>();
    };

    template<typename T>
    concept _adl_getable = adl_discoverable<T> && requires(T &&t) {
        get<frd::size_t{0}>(frd::forward<T>(t));
    };

    template<typename T>
    concept _nothrow_get = (
        (_member_getable<T> && noexcept(frd::declval<T>().template get<frd::size_t{0}>())) ||
        (_adl_getable<T>    && noexcept(get<frd::size_t{0}>(frd::declval<T>())))
    );

    template<frd::size_t I, typename Getable>
    requires (_member_getable<Getable> || _adl_getable<Getable>)
    constexpr decltype(auto) get(Getable &&getable) noexcept(_nothrow_get<Getable>) {
        if constexpr (_member_getable<Getable>) {
            return frd::forward<Getable>(getable).template get<I>();
        } else {
            return get<I>(frd::forward<Getable>(getable));
        }
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
