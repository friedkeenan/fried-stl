#pragma once

#include <frd/bits/ranges/iterators.hpp>
#include <frd/bits/ranges/access.hpp>
#include <frd/bits/ranges/view_base.hpp>

#include <frd/defines.hpp>
#include <frd/utility.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /*
        A view over an interval [start, end), similar to Python's 'range'.

        Can become an infinite range if passed an unreachable sentinel as the bound.

        'End' must be semiregular so that the sentinel may also be semiregular.

        'std::ranges::enable_borrowed_range' is specialized to 'true' further down.
    */
    template<weakly_incrementable Start, weakly_equality_comparable_with<Start> End = Start>
    requires (semiregular<End>)
    class interval : public view_interface<interval<Start, End>> {
        public:
            class iterator {
                public:
                    using value_type      = Start;
                    using difference_type = iter_difference<Start>;
                    using reference       = Start;

                    Start _value;

                    constexpr iterator() = default;

                    template<forwarder_for<Start> StartFwd>
                    constexpr explicit iterator(StartFwd &&value) noexcept(nothrow_constructible_from<Start, StartFwd>)
                        : _value(frd::forward<StartFwd>(value)) { }

                    constexpr iterator &operator ++() {
                        this->_value++;

                        return *this;
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, ++)

                    constexpr iterator &operator --() requires (weakly_decrementable<Start>) {
                        this->_value--;

                        return *this;
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, --, requires (weakly_decrementable<Start>))

                    constexpr iterator operator +(const difference_type delta) const
                    requires (
                        weakly_addable_with<Start, difference_type>
                    ) {
                        return iterator(this->_value + delta);
                    }

                    /* Commutative addition. */
                    friend constexpr iterator operator +(const difference_type delta, const iterator &it) {
                        return it + delta;
                    }

                    constexpr iterator &operator +=(const difference_type delta)
                    requires (
                        in_place_addable_with<Start, difference_type>                           ||
                        (weakly_addable_with<Start, difference_type> && copy_assignable<Start>)
                    ) {
                        if constexpr (in_place_addable_with<Start, difference_type>) {
                            this->_value += delta;
                        } else {
                            this->_value = (this->_value + delta);
                        }

                        return *this;
                    }

                    constexpr difference_type operator -(const iterator &rhs) const
                    requires (
                        subtractable<Start>
                    ) {
                        return this->_value - rhs._value;
                    }

                    constexpr iterator operator -(const difference_type delta) const
                    requires (
                        weakly_subtractable_with<Start, difference_type>
                    ) {
                        return iterator(this->_value - delta);
                    }

                    constexpr iterator &operator -=(const difference_type &delta)
                    requires (
                        in_place_subtractable_with<Start, difference_type>                           ||
                        (weakly_subtractable_with<Start, difference_type> && copy_assignable<Start>)
                    ) {
                        if constexpr (in_place_subtractable_with<Start, difference_type>) {
                            this->_value -= delta;
                        } else {
                            this->_value = (this->_value - delta);
                        }

                        return *this;
                    }

                    constexpr Start operator *() const noexcept(nothrow_constructible_from<Start, const Start &>) {
                        return this->_value;
                    }

                    constexpr const Start *operator ->() const noexcept {
                        return std::addressof(this->_value);
                    }

                    constexpr Start operator [](const difference_type delta) const
                    requires (
                        weakly_addable_with<Start, difference_type>
                    ) {
                        return this->_value + delta;
                    }

                    constexpr auto operator <=>(const iterator &rhs) const
                    noexcept(
                        nothrow_synthetic_three_way_comparable<const Start &>
                    )
                    requires (
                        synthetic_three_way_comparable<const Start &>
                    ) {
                        return frd::synthetic_three_way_compare(this->_value, rhs._value);
                    }

                    constexpr bool operator ==(const iterator &rhs) const
                    noexcept(
                        noexcept(static_cast<bool>(this->_value == rhs._value))
                    )
                    requires (
                        equality_comparable<Start>
                    ) {
                        return static_cast<bool>(this->_value == rhs._value);
                    }
            };

            using const_iterator = iterator;

            class _sentinel {
                public:
                    End _value;

                    constexpr _sentinel() = default;

                    template<forwarder_for<End> EndFwd>
                    constexpr explicit _sentinel(EndFwd &&value) noexcept(nothrow_constructible_from<End, EndFwd>)
                        : _value(frd::forward<EndFwd>(value)) { }

                    constexpr iter_difference<Start> operator -(const iterator &rhs) const
                    requires (
                        weakly_subtractable_with<const Start, const End, iter_difference<Start>>
                    ) {
                        return this->_value - rhs._value;
                    }

                    friend constexpr iter_difference<Start> operator -(const iterator &lhs, const _sentinel &rhs)
                    requires (
                        weakly_subtractable_with<const Start, const End, iter_difference<Start>>
                    ) {
                        return lhs._value - rhs._value;
                    }

                    constexpr auto operator <=>(const iterator &rhs) const
                    noexcept(
                        nothrow_synthetic_three_way_comparable_with<const End &, const Start &>
                    )
                    requires (
                        synthetic_three_way_comparable_with<const End &, const Start &>
                    ) {
                        return frd::synthetic_three_way_compare(this->_value, rhs._value);
                    }

                    constexpr bool operator ==(const iterator &rhs) const
                    noexcept(
                        noexcept(static_cast<bool>(this->_value == rhs._value))
                    ) {
                        return static_cast<bool>(this->_value == rhs._value);
                    }
            };

            /*
                Use 'iterator' as our sentinel if we can.

                This enables certain iterator operations and certain optimizations for iterator operations.
            */
            using sentinel = conditional<
                same_as<Start, End> && sentinel_for<iterator, iterator>,

                iterator,
                _sentinel
            >;

            [[no_unique_address]] Start _start;
            [[no_unique_address]] End   _end;

            template<forwarder_for<Start> StartFwd, forwarder_for<End> EndFwd>
            constexpr interval(StartFwd &&start, EndFwd &&end)
            noexcept(
                nothrow_constructible_from<Start, StartFwd> &&
                nothrow_constructible_from<End,   EndFwd>
            ) : _start(frd::forward<StartFwd>(start)), _end(frd::forward<EndFwd>(end)) { }

            template<forwarder_for<End> EndFwd>
            requires (integral<Start>)
            constexpr explicit interval(EndFwd &&end)
            noexcept(
                nothrow_constructible_from<End, EndFwd>
            ) : interval(Start{}, frd::forward<EndFwd>(end)) { }

            [[nodiscard]]
            constexpr iterator begin() const
            noexcept(
                nothrow_constructible_from<iterator, const Start &>
            )
            requires (
                copy_constructible<Start>
            ) {
                return iterator(this->_start);
            }

            [[nodiscard]]
            constexpr iterator begin()
            noexcept(
                nothrow_constructible_from<iterator, Start &&>
            )
            requires (
                !copy_constructible<Start>
            ) {
                /* To be weakly incrementable, a type must be movable, so no need to check here. */

                return iterator(frd::move(this->_start));
            }

            [[nodiscard]]
            constexpr sentinel end() const
            noexcept(
                nothrow_constructible_from<sentinel, const End &>
            ) {
                return sentinel(this->_end);
            }
    };

    template<typename T>
    requires (integral<remove_cvref<T>>)
    interval(T &&) -> interval<decay<T>, decay<T>>;

    template<typename Start, typename End>
    interval(Start &&, End &&) -> interval<decay<Start>, decay<End>>;

    namespace views {

        /*
            TODO: Should we conditonally only do 'ref_view' or 'subrange'
            to heighten the probability for a borrowed range?
        */

        /*
            A range adaptor closure that gets the iterators from a range.

            'all_t<R>' must be a borrowed range because 'interval' doesn't store a range.
        */
        constexpr inline range_adaptor_closure iterators =
            []<viewable_range R>(R &&r) requires (borrowed_range<all_t<R>>) {
                auto all_rng = views::all(frd::forward<R>(r));

                return interval(frd::begin(all_rng), frd::end(all_rng));
            };

    }

}

namespace std::ranges {

    template<typename Start, typename End>
    constexpr inline bool enable_borrowed_range<frd::interval<Start, End>> = true;

}
