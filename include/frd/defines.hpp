#pragma once

#include <climits>

#define FRD_BITSIZEOF(cls) sizeof(cls) * CHAR_BIT

#define FRD_CHECK_SELF(other) if (this == std::addressof(other)) return *this

#define FRD_NON_COPYABLE(cls)             \
    cls(const cls &) = delete;            \
    cls &operator =(const cls &) = delete

#define FRD_NON_MOVEABLE(cls)        \
    cls(cls &&) = delete;            \
    cls &operator =(cls &&) = delete

#define FRD_RIGHT_UNARY_FROM_LEFT(cls, op, ...) \
    cls operator op(int) __VA_ARGS__ {          \
        const cls ret = *this;                  \
        op(*this);                              \
        return ret;                             \
    }

template<typename... Args>
constexpr void FRD_UNUSED(Args &&... args) {
    (static_cast<void>(args), ...);
}
