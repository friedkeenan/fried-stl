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

        'Bound' must be semiregular so that the sentinel may also be semiregular.

        'std::ranges::enable_borrowed_range' is specialized to 'true' further down.
    */
    template<weakly_incrementable Start, weakly_equality_comparable_with<Start> Bound = Start>
    requires (semiregular<Bound>)
    class interval : public view_interface<interval<Start, Bound>> {
        public:
            class iterator {
                public:
                    using value_type = Start;
                    using reference  = Start;

                    /* TODO: Make this conditionally 'int128_t' to avoid overflow issues? */
                    using difference_type = iter_difference<Start>;

                    [[no_unique_address]] Start _value;

                    constexpr iterator() = default;

                    template<forwarder_for<Start> StartFwd>
                    constexpr explicit iterator(StartFwd &&value) noexcept(nothrow_constructible_from<Start, StartFwd>)
                        : _value(frd::forward<StartFwd>(value)) { }

                    constexpr iterator &operator ++() {
                        this->_value++;

                        return *this;
                    }

                    /* 'weakly_incrementable' does not guarantee any return value for post-increment. */
                    constexpr decltype(auto) operator ++(int) {
                        return this->_value++;
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, ++, requires (incrementable<Start>))

                    constexpr iterator &operator --() requires (decrementable<Start>) {
                        this->_value--;

                        return *this;
                    }

                    /*
                        NOTE: We do not need to consider just 'weakly_decrementable' as bidirectional
                        iterators must be copyable and 'decrementable'.
                    */
                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, --, requires (decrementable<Start>))

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
                    [[no_unique_address]] Bound _value;

                    constexpr _sentinel() = default;

                    template<forwarder_for<Bound> BoundFwd>
                    constexpr explicit _sentinel(BoundFwd &&value) noexcept(nothrow_constructible_from<Bound, BoundFwd>)
                        : _value(frd::forward<BoundFwd>(value)) { }

                    constexpr iter_difference<Start> operator -(const iterator &rhs) const
                    requires (
                        weakly_subtractable_with<const Start, const Bound, iter_difference<Start>>
                    ) {
                        return this->_value - rhs._value;
                    }

                    friend constexpr iter_difference<Start> operator -(const iterator &lhs, const _sentinel &rhs)
                    requires (
                        weakly_subtractable_with<const Start, const Bound, iter_difference<Start>>
                    ) {
                        return lhs._value - rhs._value;
                    }

                    constexpr auto operator <=>(const iterator &rhs) const
                    noexcept(
                        nothrow_synthetic_three_way_comparable_with<const Bound &, const Start &>
                    )
                    requires (
                        synthetic_three_way_comparable_with<const Bound &, const Start &>
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
                same_as<Start, Bound> && sentinel_for<iterator, iterator>,

                iterator,
                _sentinel
            >;

            [[no_unique_address]] Start start;
            [[no_unique_address]] Bound bound;

            template<forwarder_for<Start> StartFwd, forwarder_for<Bound> BoundFwd>
            constexpr interval(StartFwd &&start, BoundFwd &&end)
            noexcept(
                nothrow_constructible_from<Start, StartFwd> &&
                nothrow_constructible_from<Bound, BoundFwd>
            ) : start(frd::forward<StartFwd>(start)), bound(frd::forward<BoundFwd>(end)) { }

            template<forwarder_for<Bound> BoundFwd>
            requires (integral<Start>)
            constexpr explicit interval(BoundFwd &&end)
            noexcept(
                nothrow_constructible_from<Bound, BoundFwd>
            ) : interval(Start{}, frd::forward<BoundFwd>(end)) { }

            [[nodiscard]]
            constexpr iterator begin() const
            noexcept(
                nothrow_constructible_from<iterator, const Start &>
            )
            requires (
                copy_constructible<Start>
            ) {
                return iterator(this->start);
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

                return iterator(frd::move(this->start));
            }

            [[nodiscard]]
            constexpr sentinel end() const
            noexcept(
                nothrow_constructible_from<sentinel, const Bound &>
            ) {
                return sentinel(this->bound);
            }
    };

    template<typename T>
    requires (integral<remove_cvref<T>>)
    interval(T &&) -> interval<decay<T>, decay<T>>;

    template<typename Start, typename Bound>
    interval(Start &&, Bound &&) -> interval<decay<Start>, decay<Bound>>;

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

    template<typename Start, typename Bound>
    constexpr inline bool enable_borrowed_range<frd::interval<Start, Bound>> = true;

}
