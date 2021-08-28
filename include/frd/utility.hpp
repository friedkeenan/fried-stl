#pragma once

#include <frd/type_traits.hpp>

namespace frd {

    template<typename T>
    constexpr remove_reference<T> &&move(T &&t) noexcept {
        return static_cast<remove_reference<T> &&>(t);
    }

    template<typename T>
    constexpr T &&forward(remove_reference<T> &t) noexcept {
        return static_cast<T &&>(t);
    }

    template<typename T>
    constexpr T &&forward(remove_reference<T> &&t) noexcept {
        return static_cast<T &&>(t);
    }

    template<typename T>
    T &&__declval_impl();

    template<typename T>
    decltype(__declval_impl<T>()) declval() noexcept {
        static_assert(dependent_false<>, "Don't use declval in evaluated contexts!");

        return __declval_impl<T>();
    }

}
