#pragma once

#include <frd/bits/ranges/iterators.hpp>
#include <frd/bits/ranges/access.hpp>

#include <frd/utility.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /* TODO: Make these niebloids? */

    template<less_than_comparable T, same_as<T>... Rest>
    constexpr T min(const T &first, const Rest &... rest) noexcept {
        if constexpr (sizeof...(Rest) == 0) {
            return first;
        } else {
            const auto rest_min = min(rest...);

            if (first < rest_min) {
                return first;
            }

            return rest_min;
        }
    }

    template<less_than_comparable T, same_as<T>... Rest>
    constexpr T max(const T &first, const Rest &... rest) noexcept {
        if constexpr (sizeof...(Rest) == 0) {
            return first;
        } else {
            const auto rest_max = max(rest...);

            if (first < rest_max) {
                return rest_max;
            }

            return first;
        }
    }

    template<typename In, typename Out>
    struct in_out_result {
        [[no_unique_address]] In  in;
        [[no_unique_address]] Out out;

        template<typename InOther, typename OutOther>
        requires (
            convertible_to<const In  &, InOther> &&
            convertible_to<const Out &, OutOther>
        )
        constexpr operator in_out_result<InOther, OutOther>() const &
        noexcept(
            nothrow_convertible_to<const In  &, InOther>  &&
            nothrow_convertible_to<const Out &, OutOther>
        ) {
            return {this->in, this->out};
        }

        template<typename InOther, typename OutOther>
        requires (
            convertible_to<In,  InOther> &&
            convertible_to<Out, OutOther>
        )
        constexpr operator in_out_result<InOther, OutOther>() &&
        noexcept(
            nothrow_convertible_to<In,  InOther>  &&
            nothrow_convertible_to<Out, OutOther>
        ) {
            return {frd::move(this->in), frd::move(this->out)};
        }
    };

    template<typename In, typename Out>
    using copy_result = in_out_result<In, Out>;

    struct _copy_fn {
        template<input_range R, weakly_incrementable Out>
        requires (indirectly_copyable<range_iterator<R>, Out>)
        constexpr copy_result<borrowed_iterator<R>, Out> operator ()(R &&r, Out result) const {
            /* TODO: Optimizations. */

            auto input = frd::begin(r);
            auto end   = frd::end(r);

            for (; input != end; input++) {
                *result = *input;

                result++;
            }

            return {frd::move(input), frd::move(result)};
        }
    };

    constexpr inline _copy_fn copy;

}
