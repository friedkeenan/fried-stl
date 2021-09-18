#pragma once

#include <frd/bits/ranges_base.hpp>

#include <frd/math.hpp>
#include <frd/utility.hpp>

namespace frd {

    /* TODO: Delete unary '&' operators to avoid function pointer confusion? */

    /* TODO: Should we bother propagating 'noexcept' for these? libstdc++ doesn't. */

    struct _advance_fn {
        template<iterator It>
        constexpr void operator ()(It &it, iter_difference<It> n) const {
            if constexpr (random_access_iterator<It>) {
                it += n;
            } else if constexpr (bidirectional_iterator<It>) {
                /* Try to go forwards then backwards. */

                while (n > 0) {
                    n--;
                    it++;
                }

                while (n < 0) {
                    n++;
                    it--;
                }
            } else {
                FRD_ASSERT(n >= 0, "Only bidirectional iterators can go backwards!");

                while (n > 0) {
                    n--;
                    it++;
                }
            }
        }

        template<iterator It, sentinel_for<It> S>
        constexpr void operator ()(It &it, S bound) const {
            if constexpr (assignable_from<It &, S>) {
                it = frd::move(bound);
            } else if constexpr (sized_sentinel_for<S, It>) {
                (*this)(it, bound - it);
            } else {
                while (it != bound) {
                    it++;
                }
            }
        }

        template<iterator It, sentinel_for<It> S>
        constexpr iter_difference<It> operator ()(It &it, iter_difference<It> n, S bound) const {
            if constexpr (sized_sentinel_for<S, It>) {
                const auto diff = bound - it;

                FRD_ASSERT(n == 0 || diff == 0 || frd::sign(n) == frd::sign(diff), "The bound is in a different direction from the advance amount!");

                if (frd::abs(n) > frd::abs(diff)) {
                    (*this)(it, bound);

                    return n - diff;;
                }

                (*this)(it, n);

                return 0;
            } else if constexpr (bidirectional_iterator<It>) {
                /* Try to go forwards then backwards. */

                while (n > 0 && it != bound) {
                    n--;
                    it++;
                }

                while (n < 0 && it != bound) {
                    n++;
                    it--;
                }

                return n;
            } else {
                FRD_ASSERT(n >= 0, "Only bidirectional iterators can go backwards!");

                while (n > 0 && it != bound) {
                    n--;
                    it++;
                }

                return n;
            }
        }
    };

    constexpr inline _advance_fn advance;

    struct _prev_fn {
        template<bidirectional_iterator It>
        [[nodiscard]]
        constexpr It operator ()(It it) const {
            it--;

            return it;
        }

        template<bidirectional_iterator It>
        [[nodiscard]]
        constexpr It operator ()(It it, iter_difference<It> n) const {
            frd::advance(it, -n);

            return it;
        }

        template<bidirectional_iterator It, sentinel_for<It> S = It>
        [[nodiscard]]
        constexpr It operator ()(It it, iter_difference<It> n, S bound) const {
            frd::advance(it, n, bound);

            return it;
        }
    };

    constexpr inline _prev_fn prev;

}
