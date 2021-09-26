#pragma once

#include <bit>

#include <frd/arithmetic.hpp>
#include <frd/array.hpp>
#include <frd/utility.hpp>
#include <frd/algorithm.hpp>
#include <frd/ranges.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /*
        An implementation of std::byte without type aliasing.

        It is not correct to use this as a type that can alias other types,
        as only 'char', 'unsigned char', and 'std::byte' are allowed to do so
        under the standard, and I can find no way to make a custom implementation
        do so under either Clang or GCC, even with the 'may_alias' attribute.
    */

    enum class strict_byte : unsigned char { };

    template<integral IntegerType>
    constexpr IntegerType to_integer(const strict_byte b) {
        return static_cast<IntegerType>(b);
    }

    template<integral IntegerType>
    constexpr strict_byte operator <<(const strict_byte b, IntegerType shift) noexcept {
        return static_cast<strict_byte>(static_cast<unsigned int>(b) << shift);
    }

    template<integral IntegerType>
    constexpr strict_byte operator >>(const strict_byte b, IntegerType shift) noexcept {
        return static_cast<strict_byte>(static_cast<unsigned int>(b) >> shift);
    }

    template<integral IntegerType>
    constexpr strict_byte &operator <<=(strict_byte &b, IntegerType shift) noexcept {
        b = (b << shift);

        return b;
    }

    template<integral IntegerType>
    constexpr strict_byte &operator >>=(strict_byte &b, IntegerType shift) noexcept {
        b = (b >> shift);

        return b;
    }

    constexpr strict_byte operator |(const strict_byte lhs, const strict_byte rhs) noexcept {
        return static_cast<strict_byte>(static_cast<unsigned int>(lhs) | static_cast<unsigned int>(rhs));
    }

    constexpr strict_byte operator &(const strict_byte lhs, const strict_byte rhs) noexcept {
        return static_cast<strict_byte>(static_cast<unsigned int>(lhs) & static_cast<unsigned int>(rhs));
    }

    constexpr strict_byte operator ^(const strict_byte lhs, const strict_byte rhs) noexcept {
        return static_cast<strict_byte>(static_cast<unsigned int>(lhs) ^ static_cast<unsigned int>(rhs));
    }

    constexpr strict_byte &operator |=(strict_byte &lhs, const strict_byte rhs) noexcept {
        lhs = (lhs | rhs);

        return lhs;
    }

    constexpr strict_byte &operator &=(strict_byte &lhs, const strict_byte rhs) noexcept {
        lhs = (lhs & rhs);

        return lhs;
    }

    constexpr strict_byte &operator ^=(strict_byte &lhs, const strict_byte rhs) noexcept {
        lhs = (lhs ^ rhs);

        return lhs;
    }

    constexpr strict_byte operator ~(const strict_byte b) noexcept {
        return static_cast<strict_byte>(~static_cast<unsigned int>(b));
    }

    template<frd::size_t StorageSize>
    struct bit_storage {
        template<frd::size_t Size>
        using StorageForSize = frd::array<strict_byte, Size>;

        template<typename T>
        static constexpr inline frd::size_t StorageSizeForType = sizeof(remove_reference<T>);

        template<typename T>
        using StorageForType = StorageForSize<StorageSizeForType<T>>;

        using Storage = StorageForSize<StorageSize>;

        Storage storage;

        constexpr bit_storage() : storage() { }

        template<typename T>
        requires (
            trivially_copyable<remove_reference<T>> &&
            StorageSizeForType<T> <= StorageSize
        )
        constexpr explicit bit_storage(T &&obj) noexcept {
            this->pack(frd::forward<T>(obj));
        }

        template<trivially_copyable T>
        requires (StorageSizeForType<T> <= StorageSize)
        constexpr T unpack() const noexcept {
            StorageForType<T> intermediary_storage;
            frd::copy(
                subrange(
                    this->storage.begin(),
                    this->storage.begin() + intermediary_storage.size()
                ),
                intermediary_storage.begin()
            );

            return std::bit_cast<T>(intermediary_storage);
        }

        template<typename T>
        requires (
            trivially_copyable<remove_reference<T>> &&
            StorageSizeForType<T> <= StorageSize
        )
        constexpr void pack(T &&obj) noexcept {
            const auto intermediary_storage = std::bit_cast<StorageForType<T>>(frd::forward<T>(obj));

            frd::copy(intermediary_storage, this->storage.begin());
        }

        constexpr void clear() noexcept {
            this->storage = {};
        }
    };

    template<typename T>
    bit_storage(T &&) -> bit_storage<sizeof(remove_reference<T>)>;

}
