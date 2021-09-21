#pragma once

#include <memory>
#include <iterator>

#include <frd/bits/ranges_base.hpp>
#include <frd/bits/ranges_operations.hpp>

#include <frd/arithmetic.hpp>
#include <frd/utility.hpp>
#include <frd/functional.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /*
        My implementations of the range adaptor and range adaptor closure design
        takes heavy guidance from http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p2387r1.html
        and its outline of GCC 10's implementation.
    */

    /*
        Forward declare 'range_adaptor_closure' so we can
        have a deduction guide that 'range_adaptor' can use.
    */
    template<typename T>
    class range_adaptor_closure;

    template<typename T>
    range_adaptor_closure(T &&) -> range_adaptor_closure<decay<T>>;

    template<typename Invocable>
    class range_adaptor {
        public:
            [[no_unique_address]] Invocable _invocable;

            /* Don't make constructor explicit so range adaptors can be converted from lambdas implicitly. */
            template<typename InvOther>
            requires (constructible_from<Invocable, InvOther>)
            constexpr range_adaptor(InvOther &&inv) noexcept(nothrow_constructible_from<Invocable, InvOther>)
                : _invocable(frd::forward<InvOther>(inv)) { }

            /*
                NOTE: We are not as strict about the argument count as we could be.

                Currently a range adaptor can be called with the wrong
                number of arguments, which will result in an error later,
                but not necessarily an easy to track down error.

                The issue is that range adaptors don't *have* to take a
                fixed number of arguments, and even if they did the number
                would have to be manually specified as there's not a way
                to get the number of arguments from a templated function.

                This could possibly be changed so the argument count is
                specified in a template parameter, maybe could make the
                invocable a template parameter too, but in general I lean
                towards not requiring a certain number of arguments for
                simplicity, correctness, and readability when constructing
                range adaptors.
            */
            template<typename... Args>
            requires (sizeof...(Args) > 0)
            constexpr auto operator ()(Args &&... args) const
            noexcept(
                nothrow_invocable<Invocable, Args...> ||
                (!invocable<Invocable, Args...> && noexcept(
                    range_adaptor_closure(frd::bind_back(this->_invocable, frd::forward<Args>(args)...))
                ))
            ) {
                if constexpr (invocable<Invocable, Args...>) {
                    /*
                        If the range adaptor can be called with all the arguments, do so.

                        (A range adaptor 'A' can be called like 'A(R, ...)' if 'R' is a viewable range.)
                    */

                    return frd::invoke(this->_invocable, frd::forward<Args>(args)...);
                } else {
                    /*
                        Else, return a 'range_adaptor_closure' with the arguments bound to the back.

                        (A range adaptor 'A' can be called like 'A(...)', which will produce a range adaptor closure.)
                    */
                    return range_adaptor_closure(frd::bind_back(this->_invocable, frd::forward<Args>(args)...));
                }
            }
    };

    template<typename T>
    range_adaptor(T &&) -> range_adaptor<decay<T>>;

    /*
        We inherit from 'range_adaptor' for its constructor and member attribute,
        and for semantics, as all range adaptor closures are range adaptors.
    */
    template<typename Invocable>
    class range_adaptor_closure : public range_adaptor<Invocable> {
        /*
            A range adaptor closure is a range adaptor that accepts only
            one argument, the range, and is the final stage of the range
            adaptor design that ranges interact with when piping.
        */

        public:
            using Base = range_adaptor<Invocable>;

            /* Inherit constructor. */
            using Base::Base;

            /*
                By defining our own call operator, we remove the call operator
                inherited from 'range_adaptor', stopping things like 'A(...)(...)'
                with a range adaptor 'A' and arbitrary arguments.
            */
            template<viewable_range R>
            requires (invocable<Invocable, R>)
            constexpr auto operator ()(R &&r) const noexcept(nothrow_invocable<Invocable, R>) {
                /* A range adaptor closure 'C' can be called like 'C(R)' where 'R' is a viewable range. */

                return frd::invoke(this->_invocable, frd::forward<R>(r));
            }

            template<viewable_range R>
            requires (invocable<Invocable, R>)
            friend constexpr auto operator |(R &&r, const range_adaptor_closure &closure) noexcept(nothrow_invocable<Invocable, R>) {
                /*
                    A range adaptor closure 'C' can be used like 'R | C',
                    where 'R' is a viewable range, as an equivalent expression to 'C(R)'.
                */

                return frd::invoke(closure._invocable, frd::forward<R>(r));
            }

            template<typename T>
            friend constexpr auto operator |(const range_adaptor_closure<T> &lhs, const range_adaptor_closure &rhs)
            noexcept(
                nothrow_constructible_from<range_adaptor_closure<T>, const range_adaptor_closure<T> &> &&
                nothrow_constructible_from<range_adaptor_closure,    const range_adaptor_closure &>
            ) {
                /*
                    For two range adaptor closures 'C' and 'D' and viewable range 'R',
                    '(R | C) | D' and 'R | (C | D)' must be equivalent expressions.
                    This means that 'C | D' must produce a new range adaptor closure.
                */

                return range_adaptor_closure(
                    [lhs, rhs]<viewable_range R>(R &&r) {
                        return (frd::forward<R>(r) | lhs) | rhs;
                    }
                );
            }
    };

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

            struct _data_without_size {
                [[no_unique_address]] It it;
                [[no_unique_address]] S  bound;
            };

            struct _data_with_size {
                [[no_unique_address]] It it;
                [[no_unique_address]] S  bound;

                size_type size = 0;
            };

            /* Only store the size if we need to. */
            using data = conditional<StoreSize, _data_with_size, _data_without_size>;

            data _data;

            constexpr subrange() requires (default_constructible<It>) : _data(It(), S()) { }

            template<convertible_to_non_slicing<It> ItOther>
            constexpr subrange(ItOther it, S bound) requires (!StoreSize) : _data({frd::move(it), bound}) { }

            template<convertible_to_non_slicing<It> ItOther>
            requires (Kind == subrange_kind::sized && !StoreSize)
            constexpr subrange(ItOther it, S bound, const size_type size) : _data({frd::move(it), bound}) { }

            template<convertible_to_non_slicing<It> ItOther>
            requires (Kind == subrange_kind::sized && StoreSize)
            constexpr subrange(ItOther it, S bound, const size_type size) : _data({frd::move(it), bound, size}) { }

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
                noexcept(static_cast<bool>(this->_data.it == this->_data.bound))
            ) {
                return static_cast<bool>(this->_data.it == this->_data.bound);
            }

            constexpr size_type size() const
            noexcept(
                [this]() {
                    if constexpr (StoreSize) {
                        return true;
                    } else {
                        return noexcept(static_cast<size_type>(this->_data.bound - this->_data.it));
                    }
                }
            )
            requires (
                Kind == subrange_kind::sized
            ) {
                if constexpr (StoreSize) {
                    return this->_data.size;
                } else {
                    return static_cast<size_type>(this->_data.bound - this->_data.it);
                }
            }

            constexpr It begin() const
            noexcept(
                nothrow_constructible_from<It, const It &>
            )
            requires (
                copyable<It>
            ) {
                return this->_data.it;
            }

            constexpr It begin()
            noexcept(
                nothrow_constructible_from<It, It &&>
            )
            requires (
                !copyable<It>
            ) {
                return frd::move(this->_data.it);
            }

            constexpr S end() const
            noexcept(
                nothrow_constructible_from<S, const S &>
            ) {
                return this->_data.bound;
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
                    return this->_data.it;
                } else {
                    return this->_data.bound;
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
                        return this->_data.it;
                    } else {
                        return frd::move(this->_data.it);
                    }
                } else {
                    return this->_data.bound;
                }
            }

            template<typename PairLike>
            requires (
                !same_as<decay<PairLike>, subrange>                          &&
                _pair_like_convertible_from<PairLike, const It &, const S &>
            )
            constexpr operator PairLike() const {
                return PairLike(this->_data.it, this->_data.bound);
            }

            constexpr subrange &advance(const difference_type n) {
                /* TODO: Do we need to worry about the stored size here? */
                const auto diff = frd::advance(this->_data.it, n, this->_data.bound);

                if constexpr (StoreSize) {
                    this->_data.size -= frd::to_unsigned(n - diff);
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

    template<iterator It, sentinel_for<It> S>
    subrange(It, S) -> subrange<It, S>;

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

    template<typename It, typename S, subrange_kind Kind>
    constexpr inline frd::size_t tuple_size<subrange<It, S, Kind>> = 2;

    template<typename It, typename S, subrange_kind Kind>
    struct tuple_element_holder<0, subrange<It, S, Kind>> : type_holder<It> { };

    template<typename It, typename S, subrange_kind Kind>
    struct tuple_element_holder<1, subrange<It, S, Kind>> : type_holder<S> { };

    template<frd::size_t I, typename It, typename S, subrange_kind Kind>
    struct tuple_element_holder<I, const subrange<It, S, Kind>> : tuple_element_holder<I, subrange<It, S, Kind>> { };

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

    #define VIEW_DEDUCTION_GUIDE(cls)     \
        template<typename R>              \
        cls(R &&) -> cls<views::all_t<R>>


    template<bidirectional_iterator It>
    class reverse_iterator {
        public:
            using iterator_concept = conditional<
                random_access_iterator<It>,

                std::random_access_iterator_tag,
                std::bidirectional_iterator_tag
            >;

            using _iterator_category = typename std::iterator_traits<It>::iterator_category;

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
            It _it;

            constexpr reverse_iterator() : _it() { }

            template<forwarder_for<It> ItFwd>
            constexpr explicit reverse_iterator(ItFwd &&it) : _it(frd::forward<ItFwd>(it)) { }

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

    /*
        A view over an interval [start, end), similar to Python's 'range'.

        'End' must be semiregular so that the sentinel may also be semiregular.

        Could potentially implement 'iota_view' and have 'interval' inherit from that,
        but unless we need an infinitely incrementing range (could end up being useful
        for an 'enumerate' view), I would rather not.

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
                    using const_reference = const Start;

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
            ) : _start(frd::forward<StartFwd>(start)), _end(frd::forward<EndFwd>(end))
            {
                if constexpr (weakly_less_than_comparable_with<const Start &, const End &>) {
                    FRD_ASSERT(start < end, "Interval malformed!");
                }
            }

            template<forwarder_for<End> EndFwd>
            requires (integral<Start>)
            constexpr explicit interval(EndFwd &&end)
            noexcept(
                nothrow_constructible_from<End, EndFwd>
            ) : interval(Start{}, frd::forward<EndFwd>(end)) { }

            [[nodiscard]]
            constexpr iterator begin() const &
            noexcept(
                nothrow_constructible_from<iterator, const Start &>
            )
            requires (
                copyable<Start>
            ) {
                return iterator(this->_start);
            }

            [[nodiscard]]
            constexpr iterator begin() &&
            noexcept(
                nothrow_constructible_from<iterator, Start &&>
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

    #undef VIEW_DEDUCTION_GUIDE

}

/* Templated variable specializations must be in the proper namespace. */
namespace std {

    namespace ranges {

        template<typename R>
        constexpr inline bool enable_borrowed_range<frd::ref_view<R>> = true;

        template<typename It, typename S, frd::subrange_kind Kind>
        constexpr inline bool enable_borrowed_range<frd::subrange<It, S, Kind>> = true;

        template<typename Start, typename End>
        constexpr inline bool enable_borrowed_range<frd::interval<Start, End>> = true;

    }

    template<typename It>
    constexpr inline bool disable_sized_sentinel_for<frd::reverse_iterator<It>, frd::reverse_iterator<It>> = disable_sized_sentinel_for<It, It>;

}
