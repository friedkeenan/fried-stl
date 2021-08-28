#pragma once

#include <climits>

#define BITSIZEOF(cls) sizeof(cls) * CHAR_BIT

template<typename... Args>
constexpr void UNUSED(Args &&... args) {
    (static_cast<void>(args), ...);
}
