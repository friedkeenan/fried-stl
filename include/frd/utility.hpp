#pragma once

#include <frd/bits/utility_base.hpp>

#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename T>
    constexpr decltype(auto) decay(T &&t) {
        using U = remove_reference<T>;

        if constexpr (array_type<U>) {
            return static_cast<remove_extent<U> *>(forward<T>(t));
        } else if constexpr(function<U>) {
            return static_cast<U *>(forward<T>(t));
        } else {
            return static_cast<remove_cv<U>>(forward<T>(t));
        }
    }

}
