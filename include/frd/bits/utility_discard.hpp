#pragma once

namespace frd {

    template<typename... Args>
    constexpr void discard(Args &&... args) noexcept {
        (static_cast<void>(args), ...);
    }

}
