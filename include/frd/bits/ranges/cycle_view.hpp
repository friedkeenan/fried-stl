#pragma once

#include <frd/bits/ranges/access.hpp>
#include <frd/bits/ranges/view_base.hpp>

#include <frd/arithmetic.hpp>
#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<view V, frd::size_t NumCycles = dynamic_extent>
    requires (forward_range<V>)
    class cycle_view : public view_interface<cycle_view<V>> {
        public:
            using base_view = V;
            using size_type = frd::size_t;

            static constexpr size_type num_cycles = NumCycles;

            static constexpr bool HasDynamicCycles = (NumCycles == dynamic_extent);

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

                    size_type _loops_done = 0;

                    maybe_present<HasDynamicCycles, size_type> _cycles = {};

                    constexpr iterator() = default;

                    template<forwarder_for<_iterator> ItFwd, forwarder_for<_sentinel> SFwd>
                    constexpr iterator(ItFwd &&it, SFwd &&bound, const size_type cycles)
                    noexcept(
                        nothrow_constructible_from<_iterator, ItFwd> &&
                        nothrow_constructible_from<_sentinel, SFwd>
                    ) : _begin(frd::forward<ItFwd>(it)), _it(this->_begin), _bound(frd::forward<SFwd>(bound)), _cycles(cycles) {
                        if constexpr (!HasDynamicCycles) {
                            frd::precondition(cycles == NumCycles, "Mismatched cycle count for cycle_view with static cycle count!");
                        }
                    }

                    constexpr size_type cycles() const noexcept {
                        if constexpr (HasDynamicCycles) {
                            return this->_cycles;
                        } else {
                            return NumCycles;
                        }
                    }

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
                        frd::discard(rhs);

                        return this->_loops_done == this->cycles();
                    }
            };

            V _base = V();
            maybe_present<HasDynamicCycles, size_type> _cycles = {};

            constexpr cycle_view()
            requires (
                default_initializable<V>             &&
                (NumCycles == 0 || HasDynamicCycles)
            ) = default;

            constexpr cycle_view(V base, const size_type cycles)
            noexcept(
                nothrow_constructible_from<V, V &&>
            ) : _base(frd::move(base)), _cycles(cycles) {
                if constexpr (!HasDynamicCycles) {
                    frd::precondition(cycles == NumCycles, "Mismatched cycle count for cycle_view with static cycle count!");
                }
            }

            constexpr size_type cycles() const noexcept {
                if constexpr (HasDynamicCycles) {
                    return this->_cycles;
                } else {
                    return NumCycles;
                }
            }

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
                return iterator(frd::begin(this->_base), frd::end(this->_base), this->cycles());
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
                return frd::size(this->_base) * this->cycles();
            }
    };

    template<typename R, typename SizeType>
    cycle_view(R &&, SizeType) -> cycle_view<views::all_t<R>>;

    namespace views {

        template<typename T>
        constexpr inline bool _cycle_view_specialization = false;

        template<typename V, frd::size_t N>
        constexpr inline bool _cycle_view_specialization<cycle_view<V, N>> = true;

        /* Have different types and underlying invocables depending on 'NumCycles'. */
        template<frd::size_t NumCycles = dynamic_extent>
        constexpr inline inert_type cycle;

        template<frd::size_t NumCycles>
        requires (NumCycles == dynamic_extent)
        constexpr inline frd::range_adaptor cycle<NumCycles> =
            []<viewable_range R>(R &&r, const frd::size_t cycles) {
                /*
                    Try to unwrap range if possible.

                    We can't check if 'cycles' equals 1 as it is not a template
                    parameter, and so we would need some common type between
                    'views::all(frd::forward<R>(r))' and its 'cycle_view'.
                */
                if constexpr (_cycle_view_specialization<R>) {
                    /*
                        Use after forward is okay here as a move of an integral type is
                        guaranteed to be a copy.
                    */

                    return cycle_view(frd::forward<R>(r).base(), r.cycles() * cycles);
                } else {
                    return cycle_view(frd::forward<R>(r), cycles);
                }
            };

        /*
            If 'NumCycles' is not 'dynamic_extent' then we don't need to
            take any extra arguments, so use 'range_adaptor_closure'.
        */
        template<frd::size_t NumCycles>
        requires (NumCycles != dynamic_extent)
        constexpr inline frd::range_adaptor_closure cycle<NumCycles> =
            []<viewable_range R>(R &&r) {
                /* Try to unwrap range if possible. */
                if constexpr (NumCycles == 1) {
                    return views::all(frd::forward<R>(r));
                } else if constexpr (_cycle_view_specialization<R>) {
                    using base_view = remove_reference<R>::base_view;

                    constexpr auto base_cycles = remove_reference<R>::num_cycles;

                    if constexpr (base_cycles == dynamic_extent) {
                        /*
                            Use after forward is okay here as a move of an integral type is
                            guaranteed to be a copy.
                        */

                        return cycle_view(frd::forward<R>(r).base(), r.cycles() * NumCycles);
                    } else {
                        return cycle_view<base_view, base_cycles * NumCycles>(frd::forward<R>(r).base(), base_cycles * NumCycles);
                    }
                } else {
                    return cycle_view<all_t<R>, NumCycles>(frd::forward<R>(r), NumCycles);
                }
            };

    }

}

namespace std::ranges {

    template<typename V>
    constexpr inline bool enable_borrowed_range<frd::cycle_view<V>> = enable_borrowed_range<V>;

}
