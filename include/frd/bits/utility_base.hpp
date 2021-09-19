#pragma once

#include <memory>

#include <frd/bits/type_traits_base.hpp>

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

    template<typename T>
    [[nodiscard]]
    constexpr bool same_obj(const T &obj, const T &other) noexcept {
        return std::addressof(obj) == std::addressof(other);
    }

    template<typename T>
    T &&_declval_impl();

    template<typename T>
    [[nodiscard]]
    decltype(_declval_impl<T>()) declval() noexcept {
        static_assert(dependent_false<T>, "Don't use declval in evaluated contexts!");

        return frd::_declval_impl<T>();
    }

    [[noreturn]]
    inline void unreachable() {
        /* Here we can have different implementations for unreachable code. */

        __builtin_unreachable();
    }

    [[noreturn]]
    inline void unreachable(const char *msg) {
        FRD_UNUSED(msg);

        frd::unreachable();
    }

}

/* NOTE: 'FRD_ASSERT' does not give a meaningful error at runtime, but instead ends up invoking undefined behavior. */

/* Don't use braced-if so that we require a semicolon after the macro. */
#define FRD_ASSERT(expr, ...) if (!(expr)) ::frd::unreachable()
