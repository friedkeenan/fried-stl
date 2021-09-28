#pragma once

#include <climits>
#include <cfloat>

#include <limits>

#include <frd/platform.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename T>
    struct _numeric_limits;

    #define NUMERIC_LIMITS(cls, _min, _max)  \
        template<>                           \
        struct _numeric_limits<cls> {        \
            static constexpr cls min = _min; \
            static constexpr cls max = _max; \
        }

    NUMERIC_LIMITS(bool,               false,     true);
    NUMERIC_LIMITS(char,               CHAR_MIN,  CHAR_MAX);
    NUMERIC_LIMITS(signed char,        SCHAR_MIN, SCHAR_MAX);
    NUMERIC_LIMITS(unsigned char,      0,         UCHAR_MAX);
    NUMERIC_LIMITS(wchar_t,            WCHAR_MIN, WCHAR_MAX);
    NUMERIC_LIMITS(char8_t,            0,         UCHAR_MAX);
    NUMERIC_LIMITS(char16_t,           0,         UINT_LEAST16_MAX);
    NUMERIC_LIMITS(char32_t,           0,         UINT_LEAST32_MAX);
    NUMERIC_LIMITS(short,              SHRT_MIN,  SHRT_MAX);
    NUMERIC_LIMITS(unsigned short,     0,         USHRT_MAX);
    NUMERIC_LIMITS(int,                INT_MIN,   INT_MAX);
    NUMERIC_LIMITS(unsigned int,       0,         UINT_MAX);
    NUMERIC_LIMITS(long,               LONG_MIN,  LONG_MAX);
    NUMERIC_LIMITS(unsigned long,      0,         ULONG_MAX);
    NUMERIC_LIMITS(long long,          LLONG_MIN, LLONG_MAX);
    NUMERIC_LIMITS(unsigned long long, 0,         ULLONG_MAX);
    NUMERIC_LIMITS(float,              FLT_MIN,   FLT_MAX);
    NUMERIC_LIMITS(double,             DBL_MIN,   DBL_MAX);
    NUMERIC_LIMITS(long double,        LDBL_MIN,  LDBL_MAX);

    #if FRD_PLATFORM_HAS_INT_128

    FRD_PLATFORM_USES_EXTENSION NUMERIC_LIMITS(         __int128, std::numeric_limits<__int128>::min(), std::numeric_limits<         __int128>::max());
    FRD_PLATFORM_USES_EXTENSION NUMERIC_LIMITS(unsigned __int128, 0,                                    std::numeric_limits<unsigned __int128>::max());

    #endif

    #undef NUMERIC_LIMITS

    template<arithmetic T>
    using numeric_limits = _numeric_limits<remove_cv<T>>;

}
