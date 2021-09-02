#pragma once

#include <frd/defines.hpp>
#include <frd/functional.hpp>
#include <frd/utility.hpp>
#include <frd/type_traits.hpp>

namespace frd {

    /*
        '_size_t' comes from type_traits.hpp, which needs it for certain
        template parameters.

        TODO: Separate out some types like 'size_t' into a different
        header to avoid dependency gunkery like this?
    */
    using size_t = _size_t;
    static_assert(FRD_BITSIZEOF(size_t) >= 16, "Since C++11, size_t must be at least 16 bits wide");

    /* Could static_cast nullptr's, but 'declval' signals intent better. */
    using ptrdiff_t = decltype(declval<int *>() - declval<int *>());
    static_assert(FRD_BITSIZEOF(ptrdiff_t) >= 17, "Since C++11, ptrdiff_t must be at least 17 bits wide");

    template<size_t BitSize, auto Operator = equal_to>
    using int_for_bit_size = type_for_bit_size<BitSize, Operator,
        signed char,
        signed short,
        signed int,
        signed long,
        signed long long
    >;

    template<size_t BitSize, auto Operator = equal_to>
    using uint_for_bit_size = type_for_bit_size<BitSize, Operator,
        unsigned char,
        unsigned short,
        unsigned int,
        unsigned long,
        unsigned long long
    >;

    template<size_t BitSize> using  int_fits_bit_size =  int_for_bit_size<BitSize, greater_equal>;
    template<size_t BitSize> using uint_fits_bit_size = uint_for_bit_size<BitSize, greater_equal>;

    template<typename T> using  int_fits_type =  int_for_bit_size<FRD_BITSIZEOF(T), greater_equal>;
    template<typename T> using uint_fits_type = uint_for_bit_size<FRD_BITSIZEOF(T), greater_equal>;

    template<size_t BitSize, auto Operator = equal_to>
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

    #undef DECLARE_INTS

    using  intptr_t =  int_fits_type<int *>;
    using uintptr_t = uint_fits_type<int *>;

    using float32_t  = float_for_bit_size<32>;
    using float64_t  = float_for_bit_size<64>;
    using float128_t = float_for_bit_size<128>;

}
