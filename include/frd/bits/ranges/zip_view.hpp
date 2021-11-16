#pragma once

#include <iterator>
#include <type_traits>

#include <frd/bits/ranges/iterators.hpp>
#include <frd/bits/ranges/access.hpp>
#include <frd/bits/ranges/view_base.hpp>
#include <frd/bits/ranges/empty_view.hpp>
#include <frd/bits/ranges/interval.hpp>

#include <frd/defines.hpp>
#include <frd/math.hpp>
#include <frd/tuple.hpp>
#include <frd/utility.hpp>
#include <frd/algorithm.hpp>
#include <frd/type_traits.hpp>

namespace frd {

    template<typename... Rs>
    concept _zip_is_common = (
        (sizeof...(Rs) == 1 && (common_range<Rs> && ...)) ||

        (!(bidirectional_range<Rs> && ...) && (common_range<Rs> && ...)) ||

        ((random_access_range<Rs> && ...) && (sized_range<Rs> && ...))
    );

    template<bool Const, typename... Views>
    concept _zip_all_forward = (forward_range<maybe_const<Const, Views>> && ...);

    template<bool Const, typename... Views>
    concept _zip_all_bidirectional = (bidirectional_range<maybe_const<Const, Views>> && ...);

    template<bool Const, typename... Views>
    concept _zip_all_random_access = (random_access_range<maybe_const<Const, Views>> && ...);

    template<typename It>
    concept _zip_sized_sentinel_for_self_impl = sized_sentinel_for<It, It>;

    template<bool Const, typename View>
    concept _zip_sized_sentinel_for_self = _zip_sized_sentinel_for_self_impl<range_iterator<maybe_const<Const, View>>>;

    template<input_range... Views>
    requires ((view<Views> && ...) && sizeof...(Views) > 0)
    class zip_view : public view_interface<zip_view<Views...>> {
        public:
            /*
                NOTE: The standard uses an exposition only 'tuple-or-pair'
                template that uses a tuple or pair depending on the amount
                of elements. We do not need to do so as our 'pair' is just
                an alias for 'tuple'.
            */

            template<bool Const>
            struct _iterator_category { };

            template<bool Const>
            requires (_zip_all_forward<Const, Views...>)
            struct _iterator_category<Const> {
                using iterator_category = std::input_iterator_tag;
            };

            template<bool Const>
            class iterator : public _iterator_category<Const> {
                public:
                    using iterator_concept = conditional<
                        _zip_all_random_access<Const, Views...>,

                        std::random_access_iterator_tag,

                        conditional<
                            _zip_all_bidirectional<Const, Views...>,

                            std::bidirectional_iterator_tag,

                            conditional<
                                _zip_all_forward<Const, Views...>,

                                std::forward_iterator_tag,

                                std::input_iterator_tag
                            >
                        >
                    >;

                    using value_type      = tuple<range_value<maybe_const<Const, Views>>...>;
                    using difference_type = std::common_type_t<range_difference<maybe_const<Const, Views>>...>;

                    using _iterators = tuple<range_iterator<maybe_const<Const, Views>>...>;

                    _iterators _its;

                    constexpr iterator() = default;

                    constexpr explicit iterator(_iterators its)
                    noexcept(
                        nothrow_constructible_from<_iterators, _iterators &&>
                    ) : _its(frd::move(its)) { }

                    constexpr iterator(iterator<!Const> it)
                    noexcept(
                        (nothrow_constructible_from<range_iterator<const Views>, range_iterator<Views>> && ...)
                    )
                    requires (
                        Const &&

                        (convertible_to<range_iterator<Views>, range_iterator<const Views>> && ...)
                    ) : _its(frd::tuple_convert<range_iterator<const Views>...>(frd::move(it._its))) { }

                    constexpr auto operator *() const {
                        return frd::tuple_transform(this->_its, [](auto &it) -> decltype(auto) {
                            return *it;
                        });
                    }

                    constexpr iterator &operator ++() {
                        frd::apply_for_each([](auto &it) {
                            ++it;
                        }, this->_its);

                        return *this;
                    }

                    /*
                        Cannot propagate return type on since we're advancing mutliple
                        iterators, and objects cannot contain potentially 'void' members.
                    */
                    constexpr void operator ++(int) {
                        ++(*this);
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, ++, requires (_zip_all_forward<Const, Views...>))

                    constexpr iterator &operator --()
                    requires (
                        _zip_all_bidirectional<Const, Views...>
                    ) {
                        frd::apply_for_each([](auto &it) {
                            --it;
                        }, this->_its);

                        return *this;
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, --, requires(_zip_all_bidirectional<Const, Views...>))

                    constexpr iterator &operator +=(const difference_type delta)
                    requires (
                        _zip_all_random_access<Const, Views...>
                    ) {
                        frd::apply_for_each([delta]<typename It>(It &it) {
                            it += static_cast<iter_difference<It>>(delta);
                        }, this->_its);

                        return *this;
                    }

                    constexpr iterator &operator -=(const difference_type delta)
                    requires (
                        _zip_all_random_access<Const, Views...>
                    ) {
                        frd::apply_for_each([delta]<typename It>(It &it) {
                            it -= static_cast<iter_difference<It>>(delta);
                        }, this->_its);

                        return *this;
                    }

                    constexpr auto operator [](const difference_type delta) const
                    requires (
                        _zip_all_random_access<Const, Views...>
                    ) {
                        return frd::tuple_transform(this->_its, [delta]<typename It>(It &it) -> decltype(auto) {
                            return it[static_cast<iter_difference<It>>(delta)];
                        });
                    }

                    template<frd::size_t... Indices>
                    constexpr bool _compare_equal(const iterator &rhs, frd::index_sequence<Indices...>) const {
                        /*
                            NOTE: This is not the same as comparing the underlying tuples.

                            Comparing tuples will back out once on of the elements *is not*
                            equal. We need to back out once one the elements *is* equal.
                        */

                        return (static_cast<bool>(frd::get<Indices>(this->_its) == frd::get<Indices>(rhs._its)) || ...);
                    }

                    constexpr bool operator ==(const iterator &rhs) const
                    requires (
                        equality_comparable<range_iterator<maybe_const<Const, Views>>> && ...
                    ) {
                        if constexpr (_zip_all_bidirectional<Const, Views...>) {
                            return this->_its == rhs._its;
                        } else {
                            return this->template _compare_equal(rhs, frd::make_index_sequence<sizeof...(Views)>{});
                        }
                    }

                    constexpr auto operator <=>(const iterator &rhs) const
                    requires (
                        _zip_all_random_access<Const, Views...>
                    ) {
                        return this->_its <=> rhs._its;
                    }

                    constexpr iterator operator +(const difference_type delta) const
                    requires (
                        _zip_all_random_access<Const, Views...>
                    ) {
                        auto ret = *this;
                        ret += delta;

                        return ret;
                    }

                    /* Commutative addition. */
                    friend constexpr iterator operator +(const difference_type delta, const iterator &it) {
                        return it + delta;
                    }

                    constexpr iterator operator -(const difference_type delta) const
                    requires (
                        _zip_all_random_access<Const, Views...>
                    ) {
                        auto ret = *this;
                        ret -= delta;

                        return ret;
                    }

                    template<frd::size_t... Indices>
                    constexpr difference_type _difference(const iterator &rhs, frd::index_sequence<Indices...>) const {
                        return frd::min_with_projection(
                            frd::abs,

                            static_cast<difference_type>(frd::get<Indices>(this->_its) - frd::get<Indices>(rhs._its))...
                        );
                    }

                    constexpr difference_type operator -(const iterator &rhs) const
                    requires (
                        _zip_sized_sentinel_for_self<Const, Views> && ...
                    ) {
                        return this->_difference(rhs, frd::make_index_sequence<sizeof...(Views)>{});
                    }

                    /* ADL-discovered iter_move. */
                    friend constexpr auto iter_move(const iterator &it)
                    noexcept(
                        (noexcept(frd::iter_move(frd::declval<const range_iterator<maybe_const<Const, Views>> &>())) && ...) &&
                        (nothrow_move_constructible<range_rvalue_reference<maybe_const<Const, Views>>> && ...)
                    ) {
                        return frd::tuple_transform(it._its, frd::iter_move);
                    }

                    template<frd::size_t... Indices>
                    constexpr void _iter_swap(const iterator &rhs, frd::index_sequence<Indices...>) const
                    noexcept(
                        (noexcept(frd::iter_swap(frd::get<Indices>(this->_its), frd::get<Indices>(rhs._its))) && ...)
                    ) {
                        (frd::iter_swap(frd::get<Indices>(this->_its), frd::get<Indices>(rhs._its)), ...);
                    }

                    /* ADL-discovered iter_swap. */
                    friend constexpr void iter_swap(const iterator &lhs, const iterator &rhs)
                    noexcept(
                        noexcept(lhs._iter_swap(rhs, frd::make_index_sequence<sizeof...(Views)>{}))
                    )
                    requires(
                        indirectly_swappable<range_iterator<maybe_const<Const, Views>>> && ...
                    ) {
                        lhs._iter_swap(rhs, frd::make_index_sequence<sizeof...(Views)>{});
                    }
            };

            template<bool Const>
            class sentinel {
                public:
                    using _sentinels = tuple<range_sentinel<maybe_const<Const, Views>>...>;

                    _sentinels _bounds;

                    constexpr sentinel() = default;

                    constexpr explicit sentinel(_sentinels bounds) : _bounds(frd::move(bounds)) { }

                    constexpr sentinel(sentinel<!Const> s)
                    noexcept(
                        (nothrow_constructible_from<range_sentinel<const Views>, range_sentinel<Views>> && ...)
                    )
                    requires (
                        Const &&

                        (convertible_to<range_sentinel<Views>, range_sentinel<const Views>> && ...)
                    ) : _bounds(frd::tuple_convert<range_sentinel<const Views>...>(s._bounds)) { }

                    template<bool OtherConst, frd::size_t... Indices>
                    constexpr bool _compare_equal(const iterator<OtherConst> &rhs, frd::index_sequence<Indices...>) const {
                        /*
                            NOTE: This is not the same as comparing the underlying tuples.

                            Comparing tuples will back out once one of the elements *is not*
                            equal. We need to back out once one the elements *is* equal.
                        */

                        return (static_cast<bool>(frd::get<Indices>(this->_bounds) == frd::get<Indices>(rhs._its)) || ...);
                    }

                    /*
                        NOTE: The standard here checks that our sentinels are in fact
                        sentinels for the iterators, but I don't see how that would ever
                        not be the case.
                    */
                    template<bool OtherConst>
                    constexpr bool operator ==(const iterator<OtherConst> &rhs) const {
                        return this->_compare_equal(rhs, frd::make_index_sequence<sizeof...(Views)>{});
                    }

                    template<bool OtherConst, frd::size_t... Indices>
                    constexpr iterator<OtherConst>::difference_type _difference(const iterator<OtherConst> &rhs, frd::index_sequence<Indices...>) const {
                        return frd::min_with_projection(
                            frd::abs,

                            static_cast<iterator<OtherConst>::difference_type>(frd::get<Indices>(this->_bounds) - frd::get<Indices>(rhs._its))...
                        );
                    }

                    template<bool OtherConst>
                    requires (
                        sized_sentinel_for<range_sentinel<maybe_const<Const, Views>>, range_iterator<maybe_const<Const, Views>>> && ...
                    )
                    constexpr iterator<OtherConst>::difference_type operator -(const iterator<OtherConst> &rhs) const {
                        return this->_difference(rhs, frd::make_index_sequence<sizeof...(Views)>{});
                    }

                    template<bool OtherConst>
                    requires (
                        sized_sentinel_for<range_sentinel<maybe_const<Const, Views>>, range_iterator<maybe_const<Const, Views>>> && ...
                    )
                    friend constexpr iterator<OtherConst>::difference_type operator -(const iterator<OtherConst> &lhs, const sentinel<Const> &rhs) {
                        return -(rhs - lhs);
                    }
            };

            tuple<Views...> _views;

            constexpr zip_view() = default;

            constexpr explicit zip_view(Views... views) : _views({frd::move(views)...}) { }

            constexpr auto begin() requires (!(_simple_view<Views> && ...)) {
                return iterator<false>(frd::tuple_transform(this->_views, frd::begin));
            }

            constexpr auto begin() const requires (range<const Views> && ...) {
                return iterator<true>(frd::tuple_transform(this->_views, frd::begin));
            }

            constexpr auto end() requires (!(_simple_view<Views> && ...)) {
                if constexpr (!_zip_is_common<Views...>) {
                    return sentinel<false>(frd::tuple_transform(this->_views, frd::end));
                } else if constexpr ((random_access_range<Views> && ...)) {
                    return this->begin() + static_cast<iter_difference<iterator<false>>>(this->size());
                } else {
                    return iterator<false>(frd::tuple_transform(this->_views, frd::end));
                }
            }

            constexpr auto end() const requires (range<const Views> && ...) {
                if constexpr (!_zip_is_common<const Views...>) {
                    return sentinel<true>(frd::tuple_transform(this->_views, frd::end));
                } else if constexpr ((random_access_range<const Views> && ...)) {
                    return this->begin() + static_cast<iter_difference<iterator<true>>>(this->size());
                } else {
                    return iterator<true>(frd::tuple_transform(this->_views, frd::end));
                }
            }

            constexpr auto size()
            requires (
                (sized_range<Views> && ...)
            ) {
                return frd::apply([](auto... sizes) {
                    using CommonSize = make_unsigned<std::common_type_t<decltype(sizes)...>>;

                    return frd::min(static_cast<CommonSize>(sizes)...);
                }, frd::tuple_transform(this->_views, frd::size));
            }

            constexpr auto size() const
            requires (
                (sized_range<const Views> && ...)
            ) {
                return frd::apply([](auto... sizes) {
                    using CommonSize = make_unsigned<std::common_type_t<decltype(sizes)...>>;

                    return frd::min(static_cast<CommonSize>(sizes)...);
                }, frd::tuple_transform(this->_views, frd::size));
            }
    };

    template<typename... Rs>
    zip_view(Rs &&...) -> zip_view<views::all_t<Rs>...>;

    namespace views {

        constexpr inline auto zip =
            []<viewable_range... Rs>(Rs &&... rs)
            requires (
                (input_range<Rs> && ...)
            ) {
                if constexpr (sizeof...(Rs) == 0) {
                    return views::empty<tuple<>>;
                } else {
                    return zip_view(frd::forward<Rs>(rs)...);
                }
            };

        constexpr inline frd::range_adaptor_closure enumerate =
            []<viewable_range R>(R &&r) {
                /* TODO: This needs to be changed if 'interval' begins promoting range sizes to higher widths. */

                return zip_view(interval(range_size<R>{0}, unreachable_sentinel), frd::forward<R>(r));
            };

    }

}

namespace std::ranges {

    template<typename... Views>
    constexpr inline bool enable_borrowed_range<frd::zip_view<Views...>> = (enable_borrowed_range<Views> && ...);

}
