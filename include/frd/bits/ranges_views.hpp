#pragma once

#include <memory>

#include <frd/bits/ranges_base.hpp>
#include <frd/bits/ranges_operations.hpp>

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

    namespace views {

        template<typename R>
        concept _can_ref_view = requires(R &&r) {
            frd::ref_view(frd::forward<R>(r));
        };

        constexpr inline range_adaptor_closure all = []<viewable_range R>(R &&r)
        requires (
            view<R>          ||
            _can_ref_view<R> /* || ... */
        ) {
            if constexpr (view<R>) {
                return frd::decay_copy(frd::forward<R>(r));
            } else if constexpr (_can_ref_view<R>) {
                return frd::ref_view(frd::forward<R>(r));
            } /* else ... */
        };

        template<viewable_range R>
        using all_t = decltype(views::all(frd::declval<R>()));

    }

    #define VIEW_DEDUCTION_GUIDE(cls)     \
        template<typename R>              \
        cls(R &&) -> cls<views::all_t<R>>

    /*
        A view over an interval [start, end), similar to Python's 'range'.

        'std::ranges::enable_borrowed_range' is specialized to 'true' further down.
    */
    template<weakly_incrementable Start, weakly_equality_comparable_with<Start> End = Start>
    class interval : public view_interface<interval<Start, End>> {
        public:
            class iterator {
                public:
                    using value_type      = Start;
                    using difference_type = iter_difference<Start>;

                    Start _value;

                    constexpr iterator() = default;
                    constexpr explicit iterator(const Start &value) noexcept(nothrow_constructible_from<Start, const Start &>)
                        : _value(value) { }

                    constexpr iterator &operator ++() {
                        this->_value++;

                        return *this;
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, ++)

                    constexpr iterator &operator --() noexcept requires (weakly_decrementable<Start>) {
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
                    constexpr explicit _sentinel(const End &value) noexcept : _value(value) { }

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

                    /* Commutative spaceship. */
                    friend constexpr auto operator <=>(const iterator &lhs, const _sentinel &rhs)
                    noexcept(
                        nothrow_synthetic_three_way_comparable_with<const Start &, const End &>
                    )
                    requires (
                        synthetic_three_way_comparable_with<const Start &, const End &>
                    ) {
                        return frd::synthetic_three_way_compare(lhs._value, rhs._value);
                    }

                    constexpr bool operator ==(const iterator &rhs) const
                    noexcept(
                        noexcept(static_cast<bool>(this->_value == rhs._value))
                    ) {
                        return static_cast<bool>(this->_value == rhs._value);
                    }

                    /* Commutative equality. */
                    friend constexpr bool operator ==(const iterator &lhs, const _sentinel &rhs)
                    noexcept(
                        noexcept(static_cast<bool>(lhs._value == rhs._value))
                    ) {
                        return static_cast<bool>(lhs._value == rhs._value);
                    }
            };

            /*
                If 'Start' and 'End' are the same, we can just use 'iterator' as our sentinel.
                This enables certain iterator operations and certain optimizations for said operations.
            */
            using sentinel = conditional<same_as<Start, End>, iterator, _sentinel>;

            Start _start;
            End   _end;

            constexpr interval(const Start &start, const End &end) noexcept : _start(start), _end(end) {
                if constexpr (weakly_less_than_comparable_with<const Start &, const End &>) {
                    FRD_ASSERT(start < end, "Interval malformed!");
                }
            }

            constexpr explicit interval(const End &end) noexcept requires(integral<Start>) : interval(Start{}, end) { }

            [[nodiscard]]
            constexpr iterator begin() const noexcept {
                return iterator(this->_start);
            }

            [[nodiscard]]
            constexpr sentinel end() const noexcept {
                return sentinel(this->_end);
            }
    };

    template<integral T>
    interval(T) -> interval<T, T>;

    namespace views {

        /*
            A range adaptor closure that gets the iterators from a range.

            'all_t<R>' must be a borrowed range because 'interval' doesn't store a range.
        */
        constexpr inline range_adaptor_closure iterators = []<viewable_range R>(R &&r)
        requires (
            borrowed_range<all_t<R>>
        ) {
            auto all_rng = views::all(frd::forward<R>(r));

            return interval(frd::begin(all_rng), frd::end(all_rng));
        };

    }

    #undef VIEW_DEDUCTION_GUIDE

}

/* Templated variable specializations must be in the proper namespace. */
namespace std::ranges {

    template<typename Start, typename End>
    constexpr inline bool enable_borrowed_range<frd::interval<Start, End>> = true;

    template<typename R>
    constexpr inline bool enable_borrowed_range<frd::ref_view<R>> = true;

}
