#pragma once

#include <frd/concepts.hpp>

namespace frd {

    template<arithmetic T>
    constexpr T abs(const T num) noexcept {
        if constexpr (unsigned_type<T>) {
            return num;
        } else {
            if (num < 0) {
                return -num;
            }

            return num;
        }
    }

}
