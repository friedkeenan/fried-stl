#pragma once

#include <frd/bits/ranges/iterators.hpp>
#include <frd/bits/ranges/access.hpp>
#include <frd/bits/ranges/adaptor.hpp>
#include <frd/bits/ranges/view_base.hpp>

#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<bidirectional_iterator It>
    class reverse_iterator {
        public:
            using iterator_concept = conditional<
                random_access_iterator<It>,

                std::random_access_iterator_tag,
                std::bidirectional_iterator_tag
            >;

            using _iterator_category = typename iterator_traits<It>::iterator_category;

            using iterator_category = conditional<
                derived_from<_iterator_category, std::random_access_iterator_tag>,

                std::random_access_iterator_tag,
                _iterator_category
            >;

            using value_type      = iter_value<It>;
            using difference_type = iter_difference<It>;
            using pointer         = typename std::iterator_traits<It>::pointer;
            using reference       = iter_reference<It>;

            /*
                NOTE: The standard stores this as a 'protected' member named 'current'.

                I'm not sure how much I like the name 'current' so I don't use it here,
                though if it's ever desirable for users to want to inherit from our
                'reverse_iterator' and access the underlying iterator, then we can rename
                this member.
            */
            It _it = It();

            constexpr reverse_iterator() noexcept(nothrow_constructible_from<It>) = default;

            template<forwarder_for<It> ItFwd>
            constexpr explicit reverse_iterator(ItFwd &&it) noexcept(nothrow_constructible_from<It, ItFwd>)
                : _it(frd::forward<ItFwd>(it)) { }

            template<typename ItOther>
            requires (!same_as<ItOther, It> && convertible_to<const ItOther &, It>)
            constexpr reverse_iterator(const reverse_iterator<ItOther> &other) : _it(other._it) { }

            template<typename ItOther>
            requires (
                !same_as<ItOther, It>                  &&
                convertible_to<const ItOther &, It>    &&
                assignable_from<It &, const ItOther &>
            )
            constexpr reverse_iterator &operator =(const reverse_iterator<ItOther> &rhs)
            noexcept(
                nothrow_assignable_from<It &, const ItOther &>
            ) {
                this->_it = rhs._it;

                return *this;
            }

            constexpr It base() const
            noexcept(
                nothrow_constructible_from<It, const It &>
            ) {
                /*
                    NOTE: Since all bidirectional iterators are forward iterators and
                    forward iterators are sentinels for themselves and sentinels must
                    be copyable¸ 'It' is copyable as well.
                */

                return this->_it;
            }

            /* ADL-discovered iter_move. */
            friend constexpr iter_rvalue_reference<It> iter_move(const reverse_iterator &it)
            noexcept(
                nothrow_constructible_from<It, const It &>       &&
                noexcept(frd::iter_move(--frd::declval<It &>()))
            ) {
                auto underlying_it = it.base();

                return frd::iter_move(--underlying_it);
            }

            /* ADL-discovered iter_swap. */
            template<typename ItOther>
            friend constexpr void iter_swap(const reverse_iterator &lhs, const reverse_iterator<ItOther> &rhs)
            noexcept(
                nothrow_constructible_from<It, const It &>                                &&
                nothrow_constructible_from<ItOther, const ItOther &>                      &&
                noexcept(frd::iter_swap(--frd::declval<It &>(), --frd::declval<ItOther &>()))
            ) {
                auto underlying_lhs = lhs.base();
                auto underlying_rhs = rhs.base();

                frd::iter_swap(--underlying_lhs, --underlying_rhs);
            }

            constexpr reverse_iterator &operator ++() {
                this->_it--;

                return *this;
            }

            constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(reverse_iterator, ++)

            constexpr reverse_iterator &operator --() {
                this->_it++;

                return *this;
            }

            constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(reverse_iterator, --)

            constexpr reverse_iterator operator +(const difference_type delta) const
            requires (
                random_access_iterator<It>
            ) {
                return reverse_iterator(this->_it - delta);
            }

            /* Commutative addition. */
            friend constexpr reverse_iterator operator +(const difference_type delta, const reverse_iterator &it)
            requires (
                random_access_iterator<It>
            ) {
                return it + delta;
            }

            constexpr reverse_iterator operator -(const difference_type delta) const
            requires (
                random_access_iterator<It>
            ) {
                return reverse_iterator(this->_it + delta);
            }

            template<typename ItOther>
            constexpr auto operator -(const reverse_iterator<ItOther> &rhs) const {
                return this->_it - rhs._it;
            }

            constexpr reverse_iterator &operator +=(const difference_type delta) const
            requires (
                random_access_iterator<It>
            ) {
                this->_it -= delta;

                return *this;
            }

            constexpr reverse_iterator &operator -=(const difference_type delta) const
            requires (
                random_access_iterator<It>
            ) {
                this->_it += delta;

                return *this;
            }

            constexpr reference operator *() const {
                return *frd::prev(this->_it);
            }

            constexpr pointer operator ->() const noexcept
            requires (
                convertible_to_address<It>
            ) {
                return frd::to_address(frd::prev(this->_it));
            }

            constexpr reference operator [](const difference_type delta) const
            noexcept(
                nothrow_constructible_from<reference, reference>
            )
            requires (
                random_access_iterator<It>
            ) {
                return this->_it[-delta - 1];
            }

            /*
                NOTE: The standard says to forwward each individual comparison operator
                onto the 'base()' iterators if the expression is well-formed.

                I do not hate my code so much as to do all that, so we provide the spaceship
                operator instead.
            */
            template<typename ItOther>
            requires (synthetic_three_way_comparable_with<const It &, const ItOther &>)
            constexpr auto operator <=>(const reverse_iterator<ItOther> &rhs) const
            noexcept(
                nothrow_synthetic_three_way_comparable_with<const It &, const ItOther &>
            ) {
                return frd::synthetic_three_way_compare(this->_it, rhs._it);
            }

            template<weakly_equality_comparable_with<It> ItOther>
            constexpr bool operator ==(const reverse_iterator<ItOther> &rhs) const
            noexcept(
                noexcept(static_cast<bool>(this->_it == rhs._it))
            ) {
                return static_cast<bool>(this->_it == rhs._it);
            }
    };

    template<typename It>
    reverse_iterator(It &&) -> reverse_iterator<decay<It>>;

    template<view V>
    requires (bidirectional_range<V>)
    class reverse_view : public view_interface<reverse_view<V>> {
        public:
            /* Don't define 'const_iterator' as we're not sure 'const V' is a range. */
            using iterator = reverse_iterator<range_iterator<V>>;

            static constexpr bool StoreCachedCommonEnd = !(
                common_range<V> &&

                /*
                    If it's a random access range with a sized sentinel, we can
                    also get the common end of the view in constant time.
                */
                (
                    random_access_range<V> &&
                    sized_sentinel_for<
                        range_sentinel<V>,
                        range_iterator<V>
                    >
                )
            );

            [[no_unique_address]] V _base = V();

            [[no_unique_address]] maybe_present<StoreCachedCommonEnd, cached_iterator<V>> _cached_common_end = {};

            constexpr reverse_view() requires (default_constructible<V>) = default;

            constexpr reverse_view(V base) : _base(frd::move(base)) { }

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
            )
            requires (
                !copy_constructible<V>
            ) {
                return frd::move(this->_base);
            }

            constexpr iterator begin()
            noexcept(
                []() {
                    if constexpr (common_range<V>) {
                        return noexcept(reverse_iterator(frd::end(frd::declval<V &>())));
                    } else {
                        return false;
                    }
                }()
            ) {
                if constexpr (common_range<V>) {
                    return reverse_iterator(frd::end(this->_base));
                } else if constexpr (StoreCachedCommonEnd) {
                    if (this->_cached_common_end.has_value()) {
                        return reverse_iterator(this->_cached_common_end.get(this->_base));
                    }

                    auto common_end = frd::next(frd::begin(this->_base), frd::end(this->_base));

                    this->_cached_common_end.set(this->_base, common_end);

                    return reverse_iterator(frd::move(common_end));
                } else {
                    return reverse_iterator(frd::next(frd::begin(this->_base), frd::end(this->_base)));
                }
            }

            constexpr auto begin() const
            noexcept(
                noexcept(reverse_iterator(frd::end(this->_base)))
            )
            requires (
                common_range<const V>
            ) {
                return reverse_iterator(frd::end(this->_base));
            }

            constexpr iterator end()
            noexcept(
                noexcept(reverse_iterator(frd::begin(this->_base)))
            ) {
                return reverse_iterator(frd::begin(this->_base));
            }

            /*
                NOTE: The standard says this requires 'common_range<const V>',
                but technically it's fine even if that's not true. I believe
                it requires a common range for parity with the 'begin' method.
            */
            constexpr auto end() const
            noexcept(
                noexcept(reverse_iterator(frd::begin(this->_base)))
            )
            requires (
                common_range<const V>
            ) {
                return reverse_iterator(frd::begin(this->_base));
            }

            constexpr auto size()
            noexcept(
                noexcept(frd::size(this->_base))
            )
            requires (
                sized_range<V>
            ) {
                return frd::size(this->_base);
            }

            constexpr auto size() const
            noexcept(
                noexcept(frd::size(this->_base))
            )
            requires (
                sized_range<const V>
            ) {
                return frd::size(this->_base);
            }
    };

    template<typename R>
    reverse_view(R &&) -> reverse_view<views::all_t<R>>;

    namespace views {

        template<typename T>
        constexpr inline bool _reverse_view_specialization = false;

        template<typename V>
        constexpr inline bool _reverse_view_specialization<reverse_view<V>> = true;

        template<typename T>
        constexpr inline bool _reversed_subrange = false;

        template<typename It, subrange_kind Kind>
        constexpr inline bool _reversed_subrange<subrange<reverse_iterator<It>, reverse_iterator<It>, Kind>> = true;

        constexpr inline frd::range_adaptor_closure reverse =
            []<viewable_range R>(R &&r) {
                /* Unwrap already reversed ranges if we can. */

                if constexpr (_reverse_view_specialization<remove_cvref<R>>) {
                    return frd::forward<R>(r).base();
                } else if constexpr (_reversed_subrange<remove_cvref<R>>) {
                    if constexpr (sized_range<R>) {
                        return subrange(r.end().base(), r.begin.base(), r.size());
                    } else {
                        return subrange(r.end.base(), r.begin.base());
                    }
                } else {
                    return reverse_view(frd::forward<R>(r));
                }
            };

    }

}

namespace std {

    namespace ranges {

        template<typename V>
        constexpr inline bool enable_borrowed_range<frd::reverse_view<V>> = enable_borrowed_range<V>;

    }

    template<typename It>
    constexpr inline bool disable_sized_sentinel_for<frd::reverse_iterator<It>, frd::reverse_iterator<It>> = disable_sized_sentinel_for<It, It>;

}
