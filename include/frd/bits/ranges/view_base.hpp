#pragma once

#include <memory>
#include <tuple>

#include <frd/bits/ranges/iterators.hpp>
#include <frd/bits/ranges/access.hpp>
#include <frd/bits/ranges/operations.hpp>
#include <frd/bits/ranges/util.hpp>
#include <frd/bits/ranges/adaptor.hpp>

#include <frd/arithmetic.hpp>
#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<class_type Child>
    requires (same_as<Child, remove_cvref<Child>>)
    class view_interface : public view_tag {
        public:
            constexpr Child &_child() noexcept {
                static_assert(derived_from<Child, view_interface>, "Invalid use of CRTP with 'view_interface'!");

                return static_cast<Child &>(*this);
            }

            constexpr const Child &_child() const noexcept {
                static_assert(derived_from<Child, view_interface>, "Invalid use of CRTP with 'view_interface'!");

                return static_cast<const Child &>(*this);
            }

            /*
                NOTE: Clang currently does not support this code, as it will evaluate the
                'requires' clauses on 'view_interface' initialization, where 'Child' will
                be an incomplete type, and not on member initialization, like GCC does.
                I believe this to be a genuine bug, though it may also be a place where
                there's ambiguity in the standard. See https://bugs.llvm.org/show_bug.cgi?id=44833

                If we *really* wanted Clang support, we could make these all templated functions,
                like the index operator further down. However, that would make those template arguments
                specifiable, and also I'm not sure if I like making the code meaningfully worse just
                to support Clang when it bugs/has a slightly different (imo incorrect) perspective
                than GCC.
            */

            [[nodiscard]]
            constexpr bool empty()
            noexcept(
                noexcept(static_cast<bool>(frd::begin(this->_child()) == frd::end(this->_child())))
            )
            requires (
                forward_range<Child>
            ) {
                return static_cast<bool>(frd::begin(this->_child()) == frd::end(this->_child()));
            }

            [[nodiscard]]
            constexpr bool empty() const
            noexcept(
                noexcept(static_cast<bool>(frd::begin(this->_child()) == frd::end(this->_child())))
            )
            requires (
                forward_range<const Child>
            ) {
                return static_cast<bool>(frd::begin(this->_child()) == frd::end(this->_child()));
            }

            [[nodiscard]]
            constexpr auto data()
            noexcept(
                noexcept(frd::begin(this->_child()))
            )
            requires (
                contiguous_iterator<range_iterator<Child>>
            ) {
                return frd::to_address(frd::begin(this->_child()));
            }

            [[nodiscard]]
            constexpr auto data() const
            noexcept(
                noexcept(frd::begin(this->_child()))
            )
            requires (
                contiguous_iterator<range_iterator<const Child>>
            ) {
                return frd::to_address(frd::begin(this->_child()));
            }

            [[nodiscard]]
            constexpr auto size()
            noexcept(
                noexcept(frd::decay_copy(frd::end(this->_child()) - frd::begin(this->_child())))
            )
            requires (
                forward_range<Child>       &&
                sized_sentinel_for<
                    range_sentinel<Child>,
                    range_iterator<Child>
                >
            ) {
                return frd::end(this->_child()) - frd::begin(this->_child());
            }

            [[nodiscard]]
            constexpr auto size() const
            noexcept(
                noexcept(frd::decay_copy(frd::end(this->_child()) - frd::begin(this->_child())))
            )
            requires (
                forward_range<const Child>       &&
                sized_sentinel_for<
                    range_sentinel<const Child>,
                    range_iterator<const Child>
                >
            ) {
                return frd::end(this->_child()) - frd::begin(this->_child());
            }

            [[nodiscard]]
            constexpr decltype(auto) front()
            noexcept(
                noexcept(*frd::begin(this->_child()))
            )
            requires (
                forward_range<Child>
            ) {
                return *frd::begin(this->_child());
            }

            [[nodiscard]]
            constexpr decltype(auto) front() const
            noexcept(
                noexcept(*frd::begin(this->_child()))
            )
            requires (
                forward_range<const Child>
            ) {
                return *frd::begin(this->_child());
            }

            [[nodiscard]]
            constexpr decltype(auto) back()
            requires(
                bidirectional_range<Child> &&
                common_range<Child>
            ) {
                return *frd::prev(frd::end(this->_child()));
            }

            [[nodiscard]]
            constexpr decltype(auto) back() const
            requires(
                bidirectional_range<const Child> &&
                common_range<const Child>
            ) {
                return *frd::prev(frd::end(this->_child()));
            }

            /*
                NOTE: Since the index operator has a parameter that depends on the child
                view, it must be a templated function as otherwise the parameter type
                would be evaluated on 'view_interface' instantiation, where 'Child' would
                be an incomplete type.
            */

            template<random_access_range R = Child>
            [[nodiscard]]
            constexpr decltype(auto) operator [](const range_difference<R> n)
            noexcept(
                noexcept(frd::begin(this->_child())[n])
            ) {
                return frd::begin(this->_child())[n];
            }

            template<random_access_range R = const Child>
            [[nodiscard]]
            constexpr decltype(auto) operator [](const range_difference<R> n) const
            noexcept(
                noexcept(frd::begin(this->_child())[n])
            ) {
                return frd::begin(this->_child())[n];
            }
    };

    /* 'std::ranges::enable_borrowed_range' is enabled further down. */
    template<range R>
    requires (object<R>)
    class ref_view : public view_interface<ref_view<R>> {
        public:
            /* Does not need a body. */
            static void _constructor_test(R &);
            static void _constructor_test(R &&) = delete;

            R *_rng;

            template<typename T>
            requires (
                !same_as<remove_cvref<T>, ref_view>                &&
                convertible_to<T, R &>                             &&
                requires { _constructor_test(frd::declval<T>()); }
            )
            constexpr ref_view(T &&t) : _rng(std::addressof(t)) { }

            [[nodiscard]]
            constexpr R &base() const {
                return *this->_rng;
            }

            [[nodiscard]]
            constexpr range_iterator<R> begin() const
            noexcept(
                noexcept(frd::decay_copy(frd::begin(*this->_rng)))
            ) {
                return frd::begin(*this->_rng);
            }

            [[nodiscard]]
            constexpr range_sentinel<R> end() const
            noexcept(
                noexcept(frd::decay_copy(frd::end(*this->_rng)))
            ) {
                return frd::end(*this->_rng);
            }

            [[nodiscard]]
            constexpr bool empty() const
            noexcept(
                noexcept(frd::empty(*this->_rng))
            )
            requires (
                possibly_empty_range<R>
            ) {
                return frd::empty(*this->_rng);
            }

            [[nodiscard]]
            constexpr auto data() const
            noexcept(
                noexcept(frd::decay_copy(frd::data(*this->_rng)))
            )
            requires (
                contiguous_range<R>
            ) {
                return frd::data(*this->_rng);
            }
    };

    template<typename R>
    ref_view(R&) -> ref_view<R>;

    template<typename PairLike, typename T, typename U>
    concept _pair_like_convertible_from = (
        !range<PairLike> &&
        pair_like<PairLike> &&
        constructible_from<PairLike, T, U> &&
        convertible_to_non_slicing<T, tuple_element<0, PairLike>> &&
        convertible_to<U, tuple_element<1, PairLike>>
    );

    enum class subrange_kind : bool {
        unsized,
        sized,
    };

    /* 'std::ranges::enable_borrowed_range' is enabled further down. */
    template<
        iterator It,
        sentinel_for<It> S = It,
        subrange_kind Kind = []() {
            if constexpr (sized_sentinel_for<S, It>) {
                return subrange_kind::sized;
            } else {
                return subrange_kind::unsized;
            }
        }()
    >
    class subrange : public view_interface<subrange<It, S, Kind>> {
        public:
            using difference_type = iter_difference<It>;
            using size_type       = make_unsigned<difference_type>;

            static constexpr bool StoreSize = !sized_sentinel_for<S, It> && Kind == subrange_kind::sized;

            [[no_unique_address]] It _it    = It();
            [[no_unique_address]] S  _bound = S();

            [[no_unique_address]] maybe_present<StoreSize, size_type> _size = {};

            constexpr subrange() requires (default_constructible<It>) = default;

            template<convertible_to_non_slicing<It> ItOther>
            constexpr subrange(ItOther it, S bound) requires (!StoreSize) : _it(frd::move(it)), _bound(bound) { }

            template<convertible_to_non_slicing<It> ItOther>
            requires (Kind == subrange_kind::sized && !StoreSize)
            constexpr subrange(ItOther it, S bound, const size_type size) : _it(frd::move(it)), _bound(bound) {
                FRD_UNUSED(size);
            }

            template<convertible_to_non_slicing<It> ItOther>
            requires (Kind == subrange_kind::sized && StoreSize)
            constexpr subrange(ItOther it, S bound, const size_type size) : _it(frd::move(it)), _bound(bound), _size(size) { }

            template<borrowed_range R>
            requires (
                convertible_to_non_slicing<range_iterator<R>, It> &&
                convertible_to<range_sentinel<R>, S>              &&
                Kind == subrange_kind::sized
            )
            constexpr subrange(R &&r, const size_type size) : subrange(frd::begin(r), frd::end(r), size) { }

            template<typename R>
            requires (
                !same_as<decay<R>, subrange>                      &&
                borrowed_range<R>                                 &&
                convertible_to_non_slicing<range_iterator<R>, It> &&
                convertible_to<range_sentinel<R>, S>              &&
                (StoreSize && sized_range<R>)
            )
            constexpr subrange(R &&r) : subrange(r, frd::size(r)) { }

            template<typename R>
            requires (
                !same_as<decay<R>, subrange>                      &&
                borrowed_range<R>                                 &&
                convertible_to_non_slicing<range_iterator<R>, It> &&
                convertible_to<range_sentinel<R>, S>              &&
                (!StoreSize)
            )
            constexpr subrange(R &&r) : subrange(frd::begin(r), frd::end(r)) { }

            constexpr bool empty() const
            noexcept(
                noexcept(static_cast<bool>(this->_it == this->_bound))
            ) {
                return static_cast<bool>(this->_it == this->_bound);
            }

            constexpr size_type size() const
            noexcept(
                []() {
                    if constexpr (StoreSize) {
                        return true;
                    } else {
                        return noexcept(static_cast<size_type>(frd::declval<const S &>() - frd::declval<const It &>()));
                    }
                }()
            )
            requires (
                Kind == subrange_kind::sized
            ) {
                if constexpr (StoreSize) {
                    return this->_size;
                } else {
                    return static_cast<size_type>(this->_bound - this->_it);
                }
            }

            constexpr It begin() const
            noexcept(
                nothrow_constructible_from<It, const It &>
            )
            requires (
                copyable<It>
            ) {
                return this->_it;
            }

            constexpr It begin()
            noexcept(
                nothrow_constructible_from<It, It &&>
            )
            requires (
                !copyable<It>
            ) {
                return frd::move(this->_it);
            }

            constexpr S end() const
            noexcept(
                nothrow_constructible_from<S, const S &>
            ) {
                return this->_bound;
            }

            template<frd::size_t I>
            requires ((I < 2) && copyable<It>)
            constexpr auto get() const &
            noexcept(
                []() {
                    if constexpr (I == 0) {
                        return nothrow_constructible_from<It, const It &>;
                    } else {
                        return nothrow_constructible_from<S, const S &>;
                    }
                }()
            ) {
                if constexpr (I == 0) {
                    return this->_it;
                } else {
                    return this->_bound;
                }
            }

            template<frd::size_t I>
            requires ((I < 2))
            constexpr auto get() &&
            noexcept(
                []() {
                    if constexpr (I == 0) {
                        if constexpr (copyable<It>) {
                            return nothrow_constructible_from<It, It &>;
                        } else {
                            return nothrow_constructible_from<It, It &&>;
                        }
                    } else {
                        return nothrow_constructible_from<S, S &>;
                    }
                }()
            ) {
                if constexpr (I == 0) {
                    /* Only move the iterator if we have to. */
                    if constexpr (copyable<It>) {
                        return this->_it;
                    } else {
                        return frd::move(this->_it);
                    }
                } else {
                    return this->_bound;
                }
            }

            template<typename PairLike>
            requires (
                !same_as<decay<PairLike>, subrange>                          &&
                _pair_like_convertible_from<PairLike, const It &, const S &>
            )
            constexpr operator PairLike() const {
                return PairLike(this->_it, this->_bound);
            }

            constexpr subrange &advance(const difference_type n) {
                /* TODO: Do we need to worry about the stored size here? */
                const auto diff = frd::advance(this->_it, n, this->_bound);

                if constexpr (StoreSize) {
                    this->_size -= frd::to_unsigned(n - diff);
                }

                return *this;
            }

            [[nodiscard]]
            constexpr subrange next(const difference_type n) const &
            requires (
                forward_iterator<It>
            ) {
                auto new_rng = *this;
                new_rng.advance(n);

                return new_rng;
            }

            [[nodiscard]]
            constexpr subrange next(const difference_type n) &&
            requires (
                forward_iterator<It>
            ) {
                this->advance(n);

                return frd::move(*this);
            }

            [[nodiscard]]
            constexpr subrange prev(const difference_type n = 1) const &
            requires (
                bidirectional_iterator<It>
            ) {
                auto new_rng = *this;
                new_rng.advance(-n);

                return new_rng;
            }

            [[nodiscard]]
            constexpr subrange prev(const difference_type n = 1) &&
            requires (
                bidirectional_iterator<It>
            ) {
                this->advance(-n);

                return frd::move(*this);
            }
    };

    /*
        Don't think I should need to specify 'Kind', but GCC freaks out if I don't.

        TODO: This seems to be fixed in GCC trunk.
    */
    template<iterator It, sentinel_for<It> S>
    subrange(It, S) -> subrange<
        It,
        S,
        []() {
            if constexpr (sized_sentinel_for<S, It>) {
                return subrange_kind::sized;
            } else {
                return subrange_kind::unsized;
            }
        }()
    >;

    template<iterator It, sentinel_for<It> S>
    subrange(It, S, make_unsigned<iter_difference<It>>) -> subrange<It, S, subrange_kind::sized>;

    template<borrowed_range R>
    subrange(R &&) -> subrange<
        range_iterator<R>,
        range_sentinel<R>,
        []() {
            if constexpr (
                sized_range<R>                                           ||
                sized_sentinel_for<range_sentinel<R>, range_iterator<R>>
            ) {
                return subrange_kind::sized;
            } else {
                return subrange_kind::unsized;
            }
        }()
    >;

    template<borrowed_range R>
    subrange(R &&, make_unsigned<range_difference<R>>) -> subrange<
        range_iterator<R>,
        range_sentinel<R>,
        subrange_kind::sized
    >;

    namespace views {

        template<typename R>
        concept _can_ref_view = requires(R &&r) {
            frd::ref_view(frd::forward<R>(r));
        };

        template<typename R>
        concept _can_subrange = requires(R &&r) {
            frd::subrange(frd::forward<R>(r));
        };

        constexpr inline range_adaptor_closure all =
            []<viewable_range R>(R &&r)
            requires (
                view<R>          ||
                _can_ref_view<R> ||
                _can_subrange<R>
            ) {
                if constexpr (view<R>) {
                    return frd::decay_copy(frd::forward<R>(r));
                } else if constexpr (_can_ref_view<R>) {
                    return frd::ref_view(frd::forward<R>(r));
                } else {
                    return frd::subrange(frd::forward<R>(r));
                }
            };

        template<viewable_range R>
        using all_t = decltype(views::all(frd::declval<R>()));

    }

}

/* Templated variable specializations must be in the proper namespace. */
namespace std {

    namespace ranges {

        template<typename R>
        constexpr inline bool enable_borrowed_range<frd::ref_view<R>> = true;

        template<typename It, typename S, frd::subrange_kind Kind>
        constexpr inline bool enable_borrowed_range<frd::subrange<It, S, Kind>> = true;

    }

    template<typename It, typename S, frd::subrange_kind Kind>
    struct tuple_size<frd::subrange<It, S, Kind>> : frd::constant_holder<frd::size_t{2}> { };

    template<typename It, typename S, frd::subrange_kind Kind>
    struct tuple_element<0, frd::subrange<It, S, Kind>> : frd::type_holder<It> { };

    template<typename It, typename S, frd::subrange_kind Kind>
    struct tuple_element<1, frd::subrange<It, S, Kind>> : frd::type_holder<S> { };

    template<frd::size_t I, typename It, typename S, frd::subrange_kind Kind>
    struct tuple_element<I, const frd::subrange<It, S, Kind>> : tuple_element<I, frd::subrange<It, S, Kind>> { };

}
