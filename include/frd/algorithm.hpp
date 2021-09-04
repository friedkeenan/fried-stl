#pragma once

#include <frd/concepts.hpp>

namespace frd {

    template<typename T, same_as<T>... Rest>
    constexpr T min(const T &first, const Rest &... rest) noexcept {
        if constexpr (sizeof...(Rest) == 0) {
            return first;
        } else {
            const auto rest_min = min(rest...);

            if (first < rest_min) {
                return first;
            }

            return rest_min;
        }
    }

}
