#pragma once

#include <frd/utility.hpp>
#include <frd/concepts.hpp>

namespace frd {

    class sign {
        public:
            struct _only_zero {
                /*
                    Make implicit conversion from a pointer, as only
                    zero-like literals can be implicitly converted to a pointer.
                */
                consteval _only_zero(_only_zero *) { }
            };

            static const sign zero;
            static const sign positive;
            static const sign negative;

            frd::int8_t _sign;

            template<arithmetic T>
            constexpr explicit sign(const T num) noexcept {
                if (num > 0) {
                    this->_sign = 1;
                } else if (num < 0) {
                    this->_sign = -1;
                } else {
                    this->_sign = 0;
                }
            }

            constexpr bool operator ==(const sign rhs) const noexcept {
                return this->_sign == rhs._sign;
            }

            constexpr auto operator <=>(const _only_zero rhs) const noexcept {
                frd::discard(rhs);

                return (this->_sign <=> 0);
            }

            constexpr bool operator ==(const _only_zero rhs) const noexcept {
                return (*this <=> rhs) == 0;
            }

            template<arithmetic T>
            constexpr explicit operator T() const noexcept {
                return static_cast<T>(this->_sign);
            }
    };

    constexpr inline sign sign::zero     = sign( 0);
    constexpr inline sign sign::positive = sign( 1);
    constexpr inline sign sign::negative = sign(-1);

    struct _abs_fn {
        template<arithmetic T>
        constexpr T operator ()(const T num) noexcept {
            if constexpr (unsigned_type<T>) {
                return num;
            } else {
                if (num < 0) {
                    return -num;
                }

                return num;
            }
        }
    };

    constexpr inline _abs_fn abs{};

}
