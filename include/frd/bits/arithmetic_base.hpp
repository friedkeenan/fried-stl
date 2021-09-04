#pragma once

#include <frd/defines.hpp>

namespace frd {

    using size_t = decltype(sizeof(int));
    static_assert(FRD_BITSIZEOF(size_t) >= 16, "Since C++11, size_t must be at least 16 bits wide");

    using ptrdiff_t = decltype(static_cast<int *>(nullptr) - static_cast<int *>(nullptr));
    static_assert(FRD_BITSIZEOF(ptrdiff_t) >= 17, "Since C++11, ptrdiff_t must be at least 17 bits wide");

}
