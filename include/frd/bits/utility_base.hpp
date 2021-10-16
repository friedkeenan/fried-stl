#pragma once

#include <memory>

#include <frd/bits/utility_discard.hpp>
#include <frd/bits/arithmetic_base.hpp>
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
        static_assert(!is_lvalue_reference<T>, "Cannot forward lvalue references to this overload!");

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

    /* TODO: Implement 'string_view' and make 'consteval_string_view'. */
    struct _dummy_compile_time_str {
        consteval _dummy_compile_time_str() = default;

        /* Implicit cast from string literal. */
        template<frd::size_t N>
        consteval _dummy_compile_time_str(const char (&str)[N]) {
            frd::discard(str);
        }
    };

    [[noreturn]]
    inline void unreachable(const _dummy_compile_time_str msg = {}) noexcept {
        frd::discard(msg);

        /* Here we can have different implementations for unreachable code. */

        __builtin_unreachable();
    }

    /* TODO: Does a macro give better compiler diagnostics than a function? */
    constexpr void precondition(bool success, const _dummy_compile_time_str msg = {}) noexcept {
        if (success) {
            return;
        }

        /* Potentially have different functionality for failed precondiitons here. */

        frd::unreachable(msg);
    }

}
