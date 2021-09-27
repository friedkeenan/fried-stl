#pragma once

#include <frd/bits/ranges/access.hpp>

#include <frd/utility.hpp>
#include <frd/functional.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /*
        My implementations of the range adaptor and range adaptor closure design
        takes heavy guidance from http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p2387r1.html
        and its outline of GCC 10's implementation.
    */

    /*
        I got an idea to implement something like a 'workflow_adaptor' to make this
        range adaptor code even more generic. I considered it, and I have decided
        to try to keep it just an idea. It would be *interesting* to be able to do
        things like 'abs(x) | pow(3) | sqrt' instead of 'sqrt(pow(abs(x), 3))',
        but I can say that I do not feel a pressing need for that, and I would prefer
        for it be handled in the language instead, e.g. with a potential workflow
        operator in the form of '|>'.

        My urge to write abstract code may get the better of me, however.
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

}
