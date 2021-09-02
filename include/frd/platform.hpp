#pragma once

#define FRD_PLATFORM_CLANG false
#define FRD_PLATFORM_GCC   false

/* Must detect Clang before GCC as it defines __GNUC__. */
#if defined(__clang__)

#undef  FRD_PLATFORM_CLANG
#define FRD_PLATFORM_CLANG true

#elif defined(__GNUC__)

#undef  FRD_PLATFORM_GCC
#define FRD_PLATFORM_GCC true

#else

#error Unsupported compiler!

#endif

namespace frd::platform {

    consteval bool gcc() {
        return FRD_PLATFORM_GCC;
    }

    consteval bool clang() {
        return FRD_PLATFORM_CLANG;
    }

}
