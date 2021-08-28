#pragma once

#include <frd/concepts.hpp>

namespace frd {

    enum class byte : unsigned char [[gnu::may_alias]] { };

    template<integral IntegerType>
    constexpr IntegerType to_integer(const byte b) {
        return static_cast<IntegerType>(b);
    }

    template<integral IntegerType>
    constexpr byte operator <<(const byte b, IntegerType shift) noexcept {
        return static_cast<byte>(static_cast<unsigned int>(b) << shift);
    }

    template<integral IntegerType>
    constexpr byte operator >>(const byte b, IntegerType shift) noexcept {
        return static_cast<byte>(static_cast<unsigned int>(b) >> shift);
    }

    template<integral IntegerType>
    constexpr byte &operator <<=(byte &b, IntegerType shift) noexcept {
        b = (b << shift);

        return b;
    }

    template<integral IntegerType>
    constexpr byte &operator >>=(byte &b, IntegerType shift) noexcept {
        b = (b >> shift);

        return b;
    }

    constexpr byte operator |(const byte lhs, const byte rhs) noexcept {
        return static_cast<byte>(static_cast<unsigned int>(lhs) | static_cast<unsigned int>(rhs));
    }

    constexpr byte operator &(const byte lhs, const byte rhs) noexcept {
        return static_cast<byte>(static_cast<unsigned int>(lhs) & static_cast<unsigned int>(rhs));
    }

    constexpr byte operator ^(const byte lhs, const byte rhs) noexcept {
        return static_cast<byte>(static_cast<unsigned int>(lhs) ^ static_cast<unsigned int>(rhs));
    }

    constexpr byte &operator |=(byte &lhs, const byte rhs) noexcept {
        lhs = (lhs | rhs);

        return lhs;
    }

    constexpr byte &operator &=(byte &lhs, const byte rhs) noexcept {
        lhs = (lhs & rhs);

        return lhs;
    }

    constexpr byte &operator ^=(byte &lhs, const byte rhs) noexcept {
        lhs = (lhs ^ rhs);

        return lhs;
    }

    constexpr byte operator ~(const byte b) noexcept {
        return static_cast<byte>(~static_cast<unsigned int>(b));
    }

}
