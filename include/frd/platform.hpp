#pragma once

#define FRD_PLATFORM_CLANG false
#define FRD_PLATFORM_GCC   false

/* Must detect Clang before GCC as it defines __GNUC__. */
#if defined(__clang__)

#error Clang is currently unsupported as https://bugs.llvm.org/show_bug.cgi?id=44833 plagues 'frd::view_interface'.

/*
    NOTE: We still have some code that differs based on GCC vs. Clang.

    Unless we run into issues however, I'd prefer to keep that code for
    whenever we can support Clang again, as I would still like to do that.
*/

#undef  FRD_PLATFORM_CLANG
#define FRD_PLATFORM_CLANG true

#elif defined(__GNUC__)

#undef  FRD_PLATFORM_GCC
#define FRD_PLATFORM_GCC true

#else

#error Unsupported compiler!

#endif

#define FRD_PLATFORM_HAS_INT_128 false

#if defined(__SIZEOF_INT128__)

#undef  FRD_PLATFORM_HAS_INT_128
#define FRD_PLATFORM_HAS_INT_128 true

#endif

#define FRD_PLATFORM_USES_EXTENSION __extension__

namespace frd::platform {

    consteval bool gcc() {
        return FRD_PLATFORM_GCC;
    }

    consteval bool clang() {
        return FRD_PLATFORM_CLANG;
    }

    consteval bool has_int128() {
        return FRD_PLATFORM_HAS_INT_128;
    }

}
