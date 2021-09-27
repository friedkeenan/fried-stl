#pragma once

#include <frd/bits/ranges/access.hpp>
#include <frd/bits/ranges/view_base.hpp>

#include <frd/arithmetic.hpp>
#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<view V>
    requires (forward_range<V>)
    class repeat_view : public view_interface<repeat_view<V>> {
        public:
            using size_type = frd::size_t;

            class iterator {
                /*
                    NOTE: At best right now only a forward iterator.

                    If we wanted to do gross caching or have some kinda strict
                    requirements we could bump it up to at the very least bidirectional,
                    but I don't feel like putting in the work for that.
                */

                public:
                    using value_type      = range_value<V>;
                    using difference_type = make_signed<size_type>;
                    using reference       = range_reference<V>;

                    using _iterator = range_iterator<V>;
                    using _sentinel = range_sentinel<V>;

                    _iterator _begin = _iterator();
                    _iterator _it    = _iterator();
                    _sentinel _bound = _sentinel();

                    size_type _loops_done  = 0;
                    size_type _repetitions = 0;

                    constexpr iterator() = default;

                    template<forwarder_for<_iterator> ItFwd, forwarder_for<_sentinel> SFwd>
                    constexpr iterator(ItFwd &&it, SFwd &&bound, const size_type repetitions)
                    noexcept(
                        nothrow_constructible_from<_iterator, ItFwd> &&
                        nothrow_constructible_from<_sentinel, SFwd>
                    ) : _begin(frd::forward<ItFwd>(it)), _it(this->_begin), _bound(frd::forward<SFwd>(bound)), _repetitions(repetitions) { }

                    constexpr iterator &operator ++() {
                        this->_it++;

                        if (this->_it == this->_bound) {
                            this->_it = this->_begin;

                            this->_loops_done++;
                        }

                        return *this;
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, ++)

                    constexpr reference operator *() const {
                        return *this->_it;
                    }

                    constexpr auto operator ->() const noexcept
                    requires (
                        convertible_to_address<_iterator>
                    ) {
                        return frd::to_address(this->_it);
                    }

                    constexpr bool operator ==(const iterator &rhs) const = default;

                    constexpr bool operator ==(const default_sentinel_t &rhs) const {
                        FRD_UNUSED(rhs);

                        return this->_loops_done == this->_repetitions;
                    }
            };

            V _base = V();
            size_type _repetitions = 0;

            constexpr repeat_view() requires (default_constructible<V>) = default;

            constexpr repeat_view(V base, const size_type repetitions) noexcept(nothrow_constructible_from<V, V &&>)
                : _base(frd::move(base)), _repetitions(repetitions) { }

            constexpr V base() const &
            noexcept(
                nothrow_constructible_from<V, const V &>
            )
            requires (
                copy_constructible<V>
            ) {
                return this->_base;
            }

            constexpr V base() &&
            noexcept(
                nothrow_constructible_from<V, V &&>
            ) {
                return frd::move(this->_base);
            }

            constexpr iterator begin() const {
                return iterator(frd:: begin(this->_base), frd::end(this->_base), this->_repetitions);
            }

            constexpr default_sentinel_t end() const {
                return default_sentinel;
            }

            constexpr size_type size() const
            noexcept(
                noexcept(frd::size(this->_base))
            )
            requires (
                sized_range<V>
            ) {
                return frd::size(this->_base) * this->_repetitions;
            }
    };

    template<typename R, typename SizeType>
    repeat_view(R &&, SizeType) -> repeat_view<views::all_t<R>>;

    namespace views {

        template<typename T>
        constexpr inline bool _repeat_view_specialization = false;

        template<typename V>
        constexpr inline bool _repeat_view_specialization<repeat_view<V>> = true;

        constexpr inline frd::range_adaptor repeat =
            []<viewable_range R>(R &&r, const frd::size_t repetitions) {
                /* Try to unwrap range if possible. */
                if constexpr (_repeat_view_specialization<R>) {
                    /*
                        Use after forward is okay here as a move of an integral type is
                        guaranteed to be a copy.
                    */
                    return repeat_view(frd::forward<R>(r).base(), r._repetitions * repetitions);
                } else {
                    return repeat_view(frd::forward<R>(r), repetitions);
                }
            };

    }

}

namespace std::ranges {

    template<typename V>
    constexpr inline bool enable_borrowed_range<frd::repeat_view<V>> = enable_borrowed_range<V>;

}
