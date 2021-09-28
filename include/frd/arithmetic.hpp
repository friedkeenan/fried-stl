#pragma once

#include <frd/bits/arithmetic_base.hpp>

#include <frd/defines.hpp>
#include <frd/platform.hpp>
#include <frd/functional.hpp>
#include <frd/utility.hpp>
#include <frd/type_traits.hpp>

namespace frd {

    FRD_PLATFORM_USES_EXTENSION
    template<frd::size_t BitSize, auto Operator = equal_to>
    using int_for_bit_size = type_for_bit_size<BitSize, Operator,
        signed char,
        signed short,
        signed int,
        signed long,
        signed long long

        #if FRD_PLATFORM_HAS_INT_128

        /* Add comma for previous item. */
        , __int128

        #endif
    >;

    FRD_PLATFORM_USES_EXTENSION
    template<frd::size_t BitSize, auto Operator = equal_to>
    using uint_for_bit_size = type_for_bit_size<BitSize, Operator,
        unsigned char,
        unsigned short,
        unsigned int,
        unsigned long,
        unsigned long long

        #if FRD_PLATFORM_HAS_INT_128

        /* Add comma for previous item. */
        , unsigned __int128

        #endif
    >;

    template<frd::size_t BitSize> using  int_fits_bit_size =  int_for_bit_size<BitSize, greater_equal>;
    template<frd::size_t BitSize> using uint_fits_bit_size = uint_for_bit_size<BitSize, greater_equal>;

    template<typename T> using  int_fits_type =  int_for_bit_size<FRD_BITSIZEOF(T), greater_equal>;
    template<typename T> using uint_fits_type = uint_for_bit_size<FRD_BITSIZEOF(T), greater_equal>;

    template<frd::size_t BitSize, auto Operator = equal_to>
    using float_for_bit_size = type_for_bit_size<BitSize, Operator,
        float,
        double,
        long double
    >;

    #define DECLARE_INTS(bit_size)                                     \
        using  int      ##bit_size##_t =  int_for_bit_size<bit_size>;  \
        using uint      ##bit_size##_t = uint_for_bit_size<bit_size>;  \
        using  int_least##bit_size##_t =  int_fits_bit_size<bit_size>; \
        using uint_least##bit_size##_t = uint_fits_bit_size<bit_size>

    DECLARE_INTS(8);
    DECLARE_INTS(16);
    DECLARE_INTS(32);
    DECLARE_INTS(64);

    #if FRD_PLATFORM_HAS_INT_128

    DECLARE_INTS(128);

    #endif

    #undef DECLARE_INTS

    using  intptr_t =  int_fits_type<int *>;
    using uintptr_t = uint_fits_type<int *>;

    using float32_t  = float_for_bit_size<32>;
    using float64_t  = float_for_bit_size<64>;
    using float128_t = float_for_bit_size<128>;

    inline namespace arithmetic_literals {

        #define INTEGRAL_LITERAL(cls, literal)                                   \
            constexpr cls operator ""_##literal(unsigned long long n) noexcept { \
                return static_cast<cls>(n);                                      \
            }

        INTEGRAL_LITERAL(frd::size_t, sz)

        INTEGRAL_LITERAL(frd::int8_t,   i8)
        INTEGRAL_LITERAL(frd::uint8_t,  u8)
        INTEGRAL_LITERAL(frd::int16_t,  i16)
        INTEGRAL_LITERAL(frd::uint16_t, u16)
        INTEGRAL_LITERAL(frd::int32_t,  i32)
        INTEGRAL_LITERAL(frd::uint32_t, u32)
        INTEGRAL_LITERAL(frd::int64_t,  i64)
        INTEGRAL_LITERAL(frd::uint64_t, u64)

        #if FRD_PLATFORM_HAS_INT_128

        INTEGRAL_LITERAL(frd::int128_t,  i128)
        INTEGRAL_LITERAL(frd::uint128_t, u128)

        #endif

        #undef INTEGRAL_LITERAL

        #define FLOAT_LITERAL(cls, literal)                               \
            constexpr cls operator ""_##literal(long double f) noexcept { \
                return static_cast<cls>(f);                               \
            }

        FLOAT_LITERAL(frd::float32_t,  f32)
        FLOAT_LITERAL(frd::float64_t,  f64)
        FLOAT_LITERAL(frd::float128_t, f128)

        #undef FLOAT_LITERAL

    }

}
