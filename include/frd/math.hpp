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

    template<typename T>
    constexpr inline bool disable_abs = false;

    /*
        TODO: Should we require the return of 'frd::abs' to be an arithmetic type?

        For e.g. complex numbers, *technically* in math the absolute value is still
        a complex number, just with imaginary component 0. In code, however, that
        might not be especially useful.
    */

    template<typename T>
    concept _member_abs = !disable_abs<remove_cvref<T>> && requires(T &&t) {
        frd::decay_copy(frd::forward<T>(t).abs());
    };

    namespace _adl {

        /* Lookups for '_adl_size'. */
        void abs(auto &) = delete;
        void abs(const auto &) = delete;

        /* Having ADL-lookup for 'frd::abs' is desirable for e.g. 'std::complex'. */
        template<typename T>
        concept _adl_abs = !disable_abs<remove_cvref<T>> && requires(T &&t) {
            frd::decay_copy(frd::forward<T>(t));
        };

    }

    struct _abs_fn {
        /* Have as a separate overload to avoid passing arithmetic types by reference. */
        template<arithmetic T>
        constexpr T operator ()(const T num) const noexcept {
            if constexpr (unsigned_type<T>) {
                return num;
            } else {
                if (num < 0) {
                    return -num;
                }

                return num;
            }
        }

        template<typename T>
        requires (_member_abs<T> || _adl::_adl_abs<T>)
        constexpr auto operator ()(T &&t) const
        noexcept(
            []() {
                if constexpr (_member_abs<T>) {
                    return noexcept(frd::decay_copy(frd::forward<T>(t).abs()));
                } else {
                    return noexcept(frd::decay_copy(abs(frd::forward<T>(t))));
                }
            }()
        ) {
            if constexpr (_member_abs<T>) {
                return frd::forward<T>(t).abs();
            } else {
                return abs(frd::forward<T>(t));
            }
        }
    };

    /* Needs to be in own namespace to avoid ADL conflicts. */
    namespace {

        constexpr inline _abs_fn abs{};

    }

}
