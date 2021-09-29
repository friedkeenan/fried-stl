#pragma once

#include <frd/bits/functional_base.hpp>
#include <frd/bits/arithmetic_base.hpp>

#include <frd/tuple.hpp>
#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    constexpr inline auto equal_to = []<typename LHS, typename RHS>(LHS &&lhs, RHS &&rhs) {
        return frd::forward<LHS>(lhs) == frd::forward<RHS>(rhs);
    };

    constexpr inline auto greater_equal = []<typename LHS, typename RHS>(LHS &&lhs, RHS &&rhs) {
        return frd::forward<LHS>(lhs) >= frd::forward<RHS>(rhs);
    };

    template<typename Child, move_constructible Fn, move_constructible... BoundArgs>
    struct _bind_front_back_impl {
        static constexpr frd::size_t NumBoundArgs = sizeof...(BoundArgs);

        [[no_unique_address]] Fn _fn;
        [[no_unique_address]] frd::tuple<BoundArgs...> _bound_args;

        /* Add leading dummy 'not_copy_move_tag_t' argument to avoid ambiguity with copy/move constructors. */
        template<typename FnOther, typename... BoundArgsOther>
        requires (constructible_from<Fn, FnOther> && (constructible_from<BoundArgs, BoundArgsOther> && ...))
        constexpr _bind_front_back_impl(not_copy_move_tag_t, FnOther &&fn, BoundArgsOther &&... bound_args)
        noexcept(
            nothrow_constructible_from<Fn, FnOther> && (nothrow_constructible_from<BoundArgs, BoundArgsOther> && ...)
        )
            : _fn(frd::forward<FnOther>(fn)), _bound_args({frd::forward<BoundArgsOther>(bound_args)...}) { }

        /*
            'bind_front' and 'bind_back' need to propagate lvalue and rvalue calls
            appropriately, so we need to have four overloads of the call operator.
        */

        template<typename... CallArgs>
        constexpr decltype(auto) operator ()(CallArgs &&... call_args) &
        noexcept(
            noexcept(Child::_call(*this, make_index_sequence<NumBoundArgs>{}, frd::forward<CallArgs>(call_args)...))
        ) {
            return Child::_call(*this, make_index_sequence<NumBoundArgs>{}, frd::forward<CallArgs>(call_args)...);
        }

        template<typename... CallArgs>
        constexpr decltype(auto) operator ()(CallArgs &&... call_args) const &
        noexcept(
            noexcept(Child::_call(*this, make_index_sequence<NumBoundArgs>{}, frd::forward<CallArgs>(call_args)...))
        ) {
            return Child::_call(*this, make_index_sequence<NumBoundArgs>{}, frd::forward<CallArgs>(call_args)...);
        }

        template<typename... CallArgs>
        constexpr decltype(auto) operator ()(CallArgs &&... call_args) &&
        noexcept(
            noexcept(Child::_call(frd::move(*this), make_index_sequence<NumBoundArgs>{}, frd::forward<CallArgs>(call_args)...))
        ) {
            return Child::_call(frd::move(*this), make_index_sequence<NumBoundArgs>{}, frd::forward<CallArgs>(call_args)...);
        }

        template<typename... CallArgs>
        constexpr decltype(auto) operator ()(CallArgs &&... call_args) const &&
        noexcept(
            noexcept(Child::_call(frd::move(*this), make_index_sequence<NumBoundArgs>{}, frd::forward<CallArgs>(call_args)...))
        ) {
            return Child::_call(frd::move(*this), make_index_sequence<NumBoundArgs>{}, frd::forward<CallArgs>(call_args)...);
        }
    };

    template<typename Fn, typename... BoundArgs>
    struct _bind_front_fn : _bind_front_back_impl<_bind_front_fn<Fn, BoundArgs...>, Fn, BoundArgs...> {
        using Base = _bind_front_back_impl<_bind_front_fn<Fn, BoundArgs...>, Fn, BoundArgs...>;

        template<typename Self, frd::size_t... I, typename... CallArgs>
        static constexpr decltype(auto) _call(Self &&self, frd::index_sequence<I...>, CallArgs &&... call_args)
        noexcept(
            noexcept(
                invoke(
                    frd::forward<Self>(self)._fn,
                    frd::get<I>(frd::forward<Self>(self)._bound_args)...,
                    frd::forward<CallArgs>(call_args)...
                )
            )
        ) {
            return invoke(
                frd::forward<Self>(self)._fn,
                frd::get<I>(frd::forward<Self>(self)._bound_args)...,
                frd::forward<CallArgs>(call_args)...
            );
        }

        /* Inherit constructor. */
        using Base::Base;
    };

    template<typename Fn, typename... BoundArgs>
    using _bind_front_fn_with_decay = _bind_front_fn<decay<Fn>, decay<BoundArgs>...>;

    template<typename Fn, typename... BoundArgs>
    constexpr _bind_front_fn_with_decay<Fn, BoundArgs...> bind_front(Fn &&fn, BoundArgs &&... bound_args)
    noexcept(
        nothrow_constructible_from<
            _bind_front_fn_with_decay<Fn, BoundArgs...>,

            not_copy_move_tag_t,

            Fn,
            BoundArgs...
        >
    ) {
        return _bind_front_fn_with_decay<Fn, BoundArgs...>(
            not_copy_move_tag,

            frd::forward<Fn>(fn),
            frd::forward<BoundArgs>(bound_args)...
        );
    }

    template<typename Fn, typename... BoundArgs>
    struct _bind_back_fn : _bind_front_back_impl<_bind_back_fn<Fn, BoundArgs...>, Fn, BoundArgs...> {
        using Base = _bind_front_back_impl<_bind_back_fn<Fn, BoundArgs...>, Fn, BoundArgs...>;

        template<typename Self, frd::size_t... I, typename... CallArgs>
        static constexpr decltype(auto) _call(Self &&self, frd::index_sequence<I...>, CallArgs &&... call_args)
        noexcept(
            noexcept(
                invoke(
                    frd::forward<Self>(self)._fn,
                    frd::forward<CallArgs>(call_args)...,
                    frd::get<I>(frd::forward<Self>(self)._bound_args)...
                )
            )
        ) {
            return invoke(
                frd::forward<Self>(self)._fn,
                frd::forward<CallArgs>(call_args)...,
                frd::get<I>(frd::forward<Self>(self)._bound_args)...
            );
        }

        /* Inherit constructor. */
        using Base::Base;
    };

    template<typename Fn, typename... BoundArgs>
    using _bind_back_fn_with_decay = _bind_back_fn<decay<Fn>, decay<BoundArgs>...>;

    template<typename Fn, typename... BoundArgs>
    constexpr _bind_back_fn_with_decay<Fn, BoundArgs...> bind_back(Fn &&fn, BoundArgs &&... bound_args)
    noexcept(
        nothrow_constructible_from<
            _bind_back_fn_with_decay<Fn, BoundArgs...>,

            not_copy_move_tag_t,

            Fn,
            BoundArgs...
        >
    ) {
        return _bind_back_fn_with_decay<Fn, BoundArgs...>(
            not_copy_move_tag,

            frd::forward<Fn>(fn),
            frd::forward<BoundArgs>(bound_args)...
        );
    }

    template<typename Callable>
    concept _member_callable = requires {
        &Callable::operator ();
    };

    template<_member_callable... Callables>
    requires (inheritable<Callables> && ...)
    struct overloaded : public Callables... {
        /* Constructor and deduction guide are implicitly defined. */

        using Callables::operator ()...;
    };

}
