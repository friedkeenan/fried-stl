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
            using sentinel = condition<same_as<Start, End>, iterator, _sentinel>;

            Start _start;
            End   _end;

            constexpr explicit interval(const End &end) noexcept requires(integral<Start>) : _start(), _end(end) { }

            constexpr interval(const Start &start, const End &end) : _start(start), _end(end) { }

            constexpr iterator begin() const noexcept {
                return iterator(this->_start);
            }

            constexpr sentinel end() const noexcept {
                return sentinel(this->_end);
            }
    };

    template<integral T>
    interval(T) -> interval<T, T>;

}
