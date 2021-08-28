#pragma once

#include <frd/utility.hpp>

namespace frd {

    constexpr inline auto equal_to = []<typename LHS, typename RHS>(LHS &&lhs, RHS &&rhs) {
        return forward<LHS>(lhs) == forward<RHS>(rhs);
    };

    constexpr inline auto greater_equal = []<typename LHS, typename RHS>(LHS &&lhs, RHS &&rhs) {
        return forward<LHS>(lhs) >= forward<RHS>(rhs);
    };

}
