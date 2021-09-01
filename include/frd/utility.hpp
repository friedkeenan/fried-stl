#pragma once

#include <utility>
#include <memory>

#include <frd/type_traits.hpp>

namespace frd {

    template<typename T>
    [[nodiscard]]
    constexpr remove_reference<T> &&move(T &&t) noexcept {
        return static_cast<remove_reference<T> &&>(t);
    }

    template<typename T>
    [[nodiscard]]
    constexpr T &&forward(remove_reference<T> &t) noexcept {
        return static_cast<T &&>(t);
    }

    template<typename T>
    [[nodiscard]]
    constexpr T &&forward(remove_reference<T> &&t) noexcept {
        return static_cast<T &&>(t);
    }

    template<typename T, typename U = T>
    [[nodiscard]]
    constexpr T exchange(T &obj, U &&new_value) {
        T old_value = move(obj);

        obj = forward<U>(new_value);

        return old_value;
    }

    /* A utility function that will use ADL-discovered swap or std::swap. */
    template<typename LHS, typename RHS>
    constexpr void swap(LHS &&lhs, RHS &&rhs) {
        using std::swap;

        swap(forward<LHS>(lhs), forward<RHS>(rhs));
    }

    template<typename T>
    [[nodiscard]]
    constexpr bool same_obj(const T &obj, const T &other) noexcept {
        return std::addressof(obj) == std::addressof(other);
    }

    template<typename T>
    T &&__declval_impl();

    template<typename T>
    [[nodiscard]]
    decltype(__declval_impl<T>()) declval() noexcept {
        static_assert(dependent_false<T>, "Don't use declval in evaluated contexts!");

        return __declval_impl<T>();
    }

}
