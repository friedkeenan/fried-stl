#pragma once

#include <memory>

#include <frd/ranges.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /* A range over an interval [start, end), similar to Python's 'range'. */
    template<weakly_incrementable Start, weakly_equality_comparable_with<Start> End = Start>
    class interval {
        public:
            /* Forward declare. */
            class _sentinel;

            class iterator {
                public:
                    using value_type      = Start;
                    using pointer         = const Start *;
                    using reference       = const Start &;
                    using difference_type = iter_difference<Start>;

                    Start _value;

                    constexpr iterator() = default;
                    constexpr explicit iterator(const Start &value) noexcept : _value(value) { }

                    constexpr iterator &operator ++() noexcept {
                        this->_value++;

                        return *this;
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, ++, noexcept)

                    constexpr iterator &operator --() noexcept requires (weakly_decrementable<Start>) {
                        this->_value--;

                        return *this;
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, --, noexcept requires (weakly_decrementable<Start>))

                    template<typename Delta>
                    requires (weakly_addable_with<Start, Delta>)
                    constexpr iterator operator +(const Delta &delta) const noexcept {
                        return iterator(this->_value + delta);
                    }

                    template<typename Delta>
                    requires (in_place_addable_with<Start, Delta> || (weakly_addable_with<Start, Delta> && copy_assignable<Start>))
                    constexpr iterator &operator +=(const Delta &delta) noexcept {
                        if constexpr (in_place_addable_with<Start, Delta>) {
                            this->_value += delta;
                        } else {
                            this->_value = (this->_value + delta);
                        }

                        return *this;
                    }

                    template<typename Delta>
                    requires (weakly_subtractable_with<Start, Delta>)
                    constexpr iterator operator -(const Delta &delta) const noexcept {
                        return iterator(this->_value - delta);
                    }

                    template<typename Delta>
                    requires (in_place_subtractable_with<Start, Delta> || (weakly_subtractable_with<Start, Delta> && copy_assignable<Start>))
                    constexpr iterator &operator +=(const Delta &delta) noexcept {
                        if constexpr (in_place_subtractable_with<Start, Delta>) {
                            this->_value -= delta;
                        } else {
                            this->_value = (this->_value - delta);
                        }

                        return *this;
                    }

                    constexpr const Start &operator *() const noexcept {
                        return this->_value;
                    }

                    constexpr const Start *operator ->() const noexcept {
                        return std::addressof(this->_value);
                    }

                    constexpr auto operator <=>(const iterator &rhs) const noexcept = default;

                    /* Needed because of the '_sentinel' overload. */
                    constexpr bool operator ==(const iterator &rhs) const noexcept {
                        return (*this <=> rhs) == 0;
                    }

                    constexpr bool operator ==(const _sentinel &rhs) const noexcept {
                        return this->_value == rhs._value;
                    }
            };

            using const_iterator = iterator;

            class _sentinel {
                public:
                    End _value;

                constexpr _sentinel() = default;
                constexpr explicit _sentinel(const End &value) noexcept : _value(value) { }

                constexpr bool operator ==(const iterator &rhs) const noexcept {
                    return this->_value == rhs._value;
                }
            };

            /*
                If 'Start' and 'End' are the same, we can just use 'iterator' as our sentinel.
                This allows certain optimizations with regards to iterator operations.
            */
            using sentinel = conditional<same_as<Start, End>, iterator, _sentinel>;

            Start _start;
            End   _end;

            constexpr explicit interval(const End &end) noexcept requires(integral<Start>) : _start(), _end(end) { }

            constexpr interval(const Start &start, const End &end) : _start(start), _end(end) { }

            /* TODO: Add 'subinterval' method basically just for fun. */

            constexpr iterator begin() const noexcept {
                return iterator(this->_start);
            }

            constexpr sentinel end() const noexcept {
                return sentinel(this->_end);
            }
    };

    template<integral T>
    interval(T) -> interval<T, T>;

    /* TODO: Enable borrowed range. */

}
