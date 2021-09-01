#pragma once

#include <climits>

#define BITSIZEOF(cls) sizeof(cls) * CHAR_BIT

#define CHECK_SELF(other) if (this == std::addressof(other)) return *this

#define NON_COPYABLE(cls)                 \
    cls(const cls &) = delete;            \
    cls &operator =(const cls &) = delete

#define NON_MOVEABLE(cls)            \
    cls(cls &&) = delete;            \
    cls &operator =(cls &&) = delete

template<typename... Args>
constexpr void UNUSED(Args &&... args) {
    (static_cast<void>(args), ...);
}
