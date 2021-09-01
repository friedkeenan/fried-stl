#pragma once

#define PLATFORM_CLANG false
#define PLATFORM_GCC   false

/* Must detect Clang before GCC as it defines __GNUC__. */
#if defined(__clang__)

#undef  PLATFORM_CLANG
#define PLATFORM_CLANG true

#elif defined(__GNUC__)

#undef  PLATFORM_GCC
#define PLATFORM_GCC true

#else

#error Unsupported compiler!

#endif

namespace platform {

    consteval bool gcc() {
        return PLATFORM_GCC;
    }

    consteval bool clang() {
        return PLATFORM_CLANG;
    }

}
