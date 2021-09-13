#pragma once

#include <frd/bits/ranges_base.hpp>

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
        constexpr Child &_child() noexcept {
            return static_cast<Child &>(*this);
        }

        constexpr const Child &_child() const noexcept {
            return static_cast<const Child &>(*this);
        }
    };

    namespace views {

        constexpr inline range_adaptor_closure all = []<viewable_range R>(R &&r)
        requires (
            view<R> /* || ... */
        ) {
            if constexpr (view<R>) {
                return frd::decay_copy(frd::forward<R>(r));
            } /* else ... */
        };

        template<viewable_range R>
        using all_t = decltype(views::all(frd::declval<R>()));

    }

    #define VIEW_DEDUCTION_GUIDE(cls)      \
        template<typename R>               \
        cls(R &&) -> cls<views::all_t<R>>;

    #undef VIEW_DEDUCTION_GUIDE

    /* A view over an interval [start, end), similar to Python's 'range'. */
    template<weakly_incrementable Start, weakly_equality_comparable_with<Start> End = Start>
    class interval : public view_interface<interval<Start, End>> {
        public:
            /* Forward declare. */
            class _sentinel;

            class iterator {
                public:
                    using value_type      = Start;
                    using difference_type = iter_difference<Start>;

                    Start _value;

                    constexpr iterator() = default;
                    constexpr explicit iterator(const Start &value) noexcept : _value(value) { }

                    constexpr iterator &operator ++() noexcept {
                        this->_value++;

                        return *this;
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, ++, noexcept)

                    constexpr iterator &operator --() noexcept requires (weakly_decrementable<Start>) {
                        this->_value--;

                        return *this;
                    }

                    constexpr FRD_RIGHT_UNARY_OP_FROM_LEFT(iterator, --, noexcept requires (weakly_decrementable<Start>))

                    constexpr iterator operator +(const difference_type delta) const noexcept
                    requires (
                        weakly_addable_with<Start, difference_type>
                    ) {
                        return iterator(this->_value + delta);
                    }

                    constexpr iterator &operator +=(const difference_type delta) noexcept
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

                    template<typename Delta>
                    constexpr iterator operator -(const difference_type delta) const noexcept
                    requires (
                        weakly_subtractable_with<Start, difference_type>
                    ) {
                        return iterator(this->_value - delta);
                    }

                    constexpr iterator &operator -=(const difference_type &delta) noexcept
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

                    constexpr Start operator *() const noexcept {
                        return this->_value;
                    }

                    constexpr Start operator [](const difference_type delta) const noexcept
                    requires (
                        weakly_addable_with<Start, difference_type>
                    ) {
                        return this->_value + delta;
                    }

                    constexpr auto operator <=>(const iterator &rhs) const noexcept = default;

                    /* Needed because of the '_sentinel' overload. */
                    constexpr bool operator ==(const iterator &rhs) const noexcept
                    requires (
                        equality_comparable<Start>
                    ) {
                        return this->_value == rhs._value;
                    }

                    constexpr bool operator ==(const _sentinel &rhs) const noexcept {
                        return this->_value == rhs._value;
                    }
            };

            using const_iterator = iterator;

            class _sentinel {
                public:
                    End _value;

                constexpr _sentinel() = default;
                constexpr explicit _sentinel(const End &value) noexcept : _value(value) { }

                constexpr bool operator ==(const iterator &rhs) const noexcept {
                    return this->_value == rhs._value;
                }
            };

            /*
                If 'Start' and 'End' are the same, we can just use 'iterator' as our sentinel.
                This allows certain optimizations with regards to iterator operations.
            */
            using sentinel = conditional<same_as<Start, End>, iterator, _sentinel>;

            Start _start;
            End   _end;

            constexpr explicit interval(const End &end) noexcept requires(integral<Start>) : _start(), _end(end) { }

            constexpr interval(const Start &start, const End &end) : _start(start), _end(end) { }

            constexpr iterator begin() const noexcept {
                return iterator(this->_start);
            }

            constexpr sentinel end() const noexcept {
                return sentinel(this->_end);
            }
    };

    template<integral T>
    interval(T) -> interval<T, T>;

    template<typename Start, typename End>
    constexpr inline bool enable_borrowed_range<interval<Start, End>> = true;

    namespace views {

        /*
            A range adaptor closure that gets the iterators from a range.

            'R' must be a borrowed range because 'interval' doesn't store a range.
        */
        constexpr inline range_adaptor_closure iterators = []<viewable_range R>(R &&r)
        requires (
            borrowed_range<R>
        ) {
            auto all_rng = views::all(frd::forward<R>(r));

            return interval(frd::begin(all_rng), frd::end(all_rng));
        };

    }

}
