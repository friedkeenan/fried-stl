#pragma once

#include <compare>

#include <frd/bits/ranges/iterators.hpp>
#include <frd/bits/ranges/view_base.hpp>
#include <frd/bits/memory_base.hpp>

#include <frd/defines.hpp>
#include <frd/span.hpp>
#include <frd/utility.hpp>
#include <frd/concepts.hpp>
#include <frd/type_traits.hpp>

namespace frd {

    template<typename It>
    concept _has_direct_iterator_concept = requires {
        typename It::iterator_concept;
    };

    template<typename It>
    concept _has_direct_iterator_catgeory = requires {
        typename It::iterator_category;
    };

    /*
        'counted_iterator' has certain type aliases defined only if certain conditions are met.
        We use base classes with overloads for those conditions to avoid cude duplication.
    */

    template<typename It>
    struct _counted_value_type { };

    template<indirectly_readable It>
    struct _counted_value_type<It> {
        using value_type = iter_value<It>;
    };

    template<typename It>
    struct _counted_iterator_concept { };

    template<_has_direct_iterator_concept It>
    struct _counted_iterator_concept<It> {
        using iterator_concept = It::iterator_concept;
    };

    template<typename It>
    struct _counted_iterator_category { };

    template<_has_direct_iterator_catgeory It>
    struct _counted_iterator_category<It> {
        using iterator_category = It::iterator_category;
    };

    template<iterator It>
    class counted_iterator : public combine_bases<
        _counted_value_type       <It>,
        _counted_iterator_concept <It>,
        _counted_iterator_category<It>
    > {
        public:
            using iterator_type   = It;
            using difference_type = iter_difference<It>;

            It              _it     = It();
            difference_type _length = 0;

            constexpr counted_iterator() requires (default_initializable<It>) = default;

            constexpr counted_iterator(It it, const difference_type n)
            noexcept(
                nothrow_constructible_from<It, It &&>
            ) : _it(frd::move(it)), _length(n) {
                frd::precondition(n >= 0, "Length of a counted iterator must be non-negative!");
            }

            /* Require that 'ItOther' is not 'It' so we can be potentially trivially copyable. */
            template<typename ItOther>
            requires (!same_as<ItOther, It> && convertible_to<const ItOther &, It>)
            constexpr counted_iterator(const ItOther &other)
            noexcept(nothrow_constructible_from<It, const ItOther &>) : _it(other._it), _length(other._length) { }

            /* Require that 'ItOther' is not 'It' so we can be potentially trivially copy-assignable. */
            template<typename ItOther>
            requires (!same_as<ItOther, It> && assignable_from<It &, const ItOther &>)
            constexpr counted_iterator &operator =(const counted_iterator<ItOther> &rhs)
            noexcept(
                nothrow_assignable_from<It &, const ItOther &>
            ) {
                FRD_CHECK_SELF(rhs);

                this->_it     = rhs._it;
                this->_length = rhs._length;

                return *this;
            }

            constexpr const It &base() const & noexcept {
                return this->_it;
            }

            constexpr It base() &&
            noexcept(
                nothrow_constructible_from<It, It &&>
            ) {
                return frd::move(this->_it);
            }

            constexpr decltype(auto) operator *()
            noexcept(
                noexcept(*this->_it)
            ) {
                frd::precondition(this->_length > 0, "Cannot dereference counted iterator of non-positive length!");

                return *this->_it;
            }

            constexpr decltype(auto) operator *() const
            noexcept(
                noexcept(*this->_it)
            )
            requires (
                _star_dereferenceable<const It>
            ) {
                frd::precondition(this->_length > 0, "Cannot dereference counted iterator of non-positive length!");

                return *this->_it;
            }

            constexpr auto operator ->() const noexcept
            requires (
                contiguous_iterator<It>
            ) {
                return frd::to_address(this->_it);
            }

            constexpr decltype(auto) operator [](const difference_type delta) const
            noexcept(
                noexcept(this->_it[delta])
            )
            requires (
                random_access_iterator<It>
            ) {
                frd::precondition(delta < this->_length, "Cannot access beyond bounds of counted iterator!");

                return this->_it[delta];
            }

            constexpr counted_iterator &operator ++() {
                frd::precondition(this->_length > 0, "Cannot increment counted iterator beyond its length!");

                this->_it++;
                this->_length--;

                return *this;
            }

            /*
                Input and output iterators do not need to return a copy in
                their post-increment operators.
            */
            constexpr decltype(auto) operator ++(int) {
                frd::precondition(this->_length > 0, "Cannot increment counted iterator beyond its length!");

                this->_length--;

                try {
                    return this->_it++;
                } catch(...) {
                    /* Undo length decrement. */
                    this->_length++;

                    throw;
                }
            }

            constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(counted_iterator, ++, requires (forward_iterator<It>))

            constexpr counted_iterator &operator --()
            requires (
                bidirectional_iterator<It>
            ) {
                this->_it--;
                this->_length++;

                return *this;
            }

            constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(counted_iterator, --, requires (bidirectional_iterator<It>))

            constexpr counted_iterator operator +(const difference_type delta) const
            requires (
                random_access_iterator<It>
            ) {
                return counted_iterator(this->_it + delta, this->_length - delta);
            }

            /* Commutative addition. */
            friend constexpr counted_iterator operator +(const difference_type delta, const counted_iterator &it) {
                return it + delta;
            }

            constexpr counted_iterator &operator +=(const difference_type delta)
            requires (
                random_access_iterator<It>
            ) {
                frd::precondition(delta <= this->_length, "Cannot increment counted iterator beyond its length!");

                this->_it     += delta;
                this->_length -= delta;

                return *this;
            }

            constexpr counted_iterator operator -(const difference_type delta) const
            requires (
                random_access_iterator<It>
            ) {
                return counted_iterator(this->_it - delta, this->_length + delta);
            }

            template<common_with<It> ItOther>
            constexpr iter_difference<ItOther> operator -(const counted_iterator<ItOther> &rhs) const {
                /* Precondition that 'rhs' and 'this' point to the same sequence. */

                return rhs._length - this->_length;
            }

            constexpr difference_type operator -(const default_sentinel_t rhs) const noexcept {
                frd::discard(rhs);

                return -this._length;
            }

            friend constexpr difference_type operator -(const default_sentinel_t s, const counted_iterator &it) noexcept {
                frd::discard(s);

                return it._length;
            }

            constexpr counted_iterator &operator -=(const difference_type delta)
            requires (
                random_access_iterator<It>
            ) {
                frd::precondition(-delta <= this->_length, "Cannot decrement counted iterator beyond its length!");

                this->_it     -= delta;
                this->_length += delta;

                return *this;
            }

            constexpr bool operator ==(const default_sentinel_t rhs) const noexcept {
                frd::discard(rhs);

                return this->_length == 0;
            }


            template<common_with<It> ItOther>
            constexpr std::strong_ordering operator <=>(const counted_iterator<ItOther> &rhs) const noexcept {
                /* Precondition that 'rhs' and 'this' point to the same sequence. */

                /* Length counts down, not up, so flip comparison to have 'rhs' on the left. */
                return rhs._length <=> this->_length;
            }

            template<common_with<It> ItOther>
            constexpr bool operator ==(const counted_iterator<ItOther> &rhs) const noexcept {
                /* Precondition that 'rhs' and 'this' point to the same sequence. */

                return (*this <=> rhs) == 0;
            }

            /* ADL-discovered iter_move. */
            friend constexpr iter_rvalue_reference<It> iter_move(const counted_iterator &it)
            noexcept(
                noexcept(frd::iter_move(it._it))
            ) {
                frd::precondition(it._length > 0, "Cannot iter_move iterator which is beyond its length!");

                return frd::iter_move(it._it);
            }

            /* ADL-discovered iter_swap. */
            template<indirectly_swappable<It> ItOther>
            friend constexpr void iter_swap(const counted_iterator &lhs, const counted_iterator<ItOther> &rhs)
            noexcept(
                noexcept(frd::iter_swap(lhs._it, rhs._it))
            ) {
                frd::precondition(lhs._length > 0 && rhs._length, "Cannot iter_swap iterators which are beyond their length!");

                frd::iter_swap(lhs._it, rhs._it);
            }
    };

    namespace views {

        constexpr inline auto counted =
            []<
                typename ItFwd,
                typename Length,
                typename It         = decay<ItFwd>,
                typename Difference = iter_difference<It>
            >(ItFwd &&it, Length &&length)
            requires (
                iterator<It>                       &&
                convertible_to<Length, Difference>
            ) {
                if constexpr (contiguous_iterator<It>) {
                    return span(frd::to_address(frd::forward<ItFwd>(it)), static_cast<Difference>(length));
                } else if constexpr (random_access_iterator<It>) {
                    return subrange(it, it + static_cast<Difference>(length));
                } else {
                    return subrange(counted_iterator(frd::forward<It>(it), static_cast<Difference>(length)), default_sentinel);
                }
            };

    }

}

namespace std {

    template<frd::input_iterator It>
    requires (!frd::primary_iterator_traits<It>)
    struct iterator_traits<frd::counted_iterator<It>> : std::iterator_traits<It> {
        using pointer = frd::conditional<
            frd::contiguous_iterator<It>,

            frd::add_pointer<frd::iter_reference<It>>,
            void
        >;
    };

}
