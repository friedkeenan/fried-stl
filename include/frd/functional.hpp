#pragma once

#include <frd/arithmetic.hpp>
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

    /* TODO: Handle reference wrappers for 'invoke'? */

    template<typename Invocable, typename... Args>
    concept _normal_callable = requires(Invocable &&inv, Args &&... args) {
        frd::forward<Invocable>(inv)(frd::forward<Args>(args)...);
    };

    template<typename Invocable, typename... Args>
    requires (_normal_callable<Invocable, Args...>)
    [[nodiscard]]
    constexpr decltype(auto) invoke(Invocable &&inv, Args &&... args)
    noexcept(
        noexcept(frd::forward<Invocable>(inv)(frd::forward<Args>(args)...))
    ) {
        return frd::forward<Invocable>(inv)(frd::forward<Args>(args)...);
    }

    template<typename MemberObjPtr, typename Obj>
    concept _member_obj_callable = member_object_pointer<MemberObjPtr> &&
        requires(const MemberObjPtr mem_obj_ptr, Obj &&obj) {
            frd::forward<Obj>(obj).*mem_obj_ptr;
        };

    template<typename MemberObjPtr, typename Obj>
    requires (_member_obj_callable<MemberObjPtr, Obj>)
    [[nodiscard]]
    constexpr decltype(auto) invoke(const MemberObjPtr mem_obj_ptr, Obj &&obj) noexcept {
        return frd::forward<Obj>(obj).*mem_obj_ptr;
    }

    template<typename MemberObjPtr, typename ObjPtr>
    concept _member_obj_callable_with_ptr = member_object_pointer<MemberObjPtr> && pointer<ObjPtr> &&
        requires(const MemberObjPtr mem_obj_ptr, const ObjPtr obj_ptr) {
            obj_ptr->*mem_obj_ptr;
        };

    template<typename MemberObjPtr, typename ObjPtr>
    requires (_member_obj_callable_with_ptr<MemberObjPtr, ObjPtr>)
    [[nodiscard]]
    constexpr decltype(auto) invoke(const MemberObjPtr mem_obj_ptr, const ObjPtr obj_ptr) noexcept {
        return obj_ptr->*mem_obj_ptr;
    }

    template<typename MemberFuncPtr, typename Obj, typename... Args>
    concept _member_func_callable = member_function_pointer<MemberFuncPtr> &&
        requires(const MemberFuncPtr mem_func_ptr, Obj &&obj, Args &&... args) {
            (frd::forward<Obj>(obj).*mem_func_ptr)(frd::forward<Args>(args)...);
        };

    template<typename MemberFuncPtr, typename Obj, typename... Args>
    requires (_member_func_callable<MemberFuncPtr, Obj, Args...>)
    [[nodiscard]]
    constexpr decltype(auto) invoke(const MemberFuncPtr mem_func_ptr, Obj &&obj, Args &&... args)
    noexcept(
        noexcept((frd::forward<Obj>(obj).*mem_func_ptr)(frd::forward<Args>(args)...))
    ){
        return (frd::forward<Obj>(obj).*mem_func_ptr)(frd::forward<Args>(args)...);
    }

    template<typename MemberFuncPtr, typename ObjPtr, typename... Args>
    concept _member_func_callable_with_ptr = member_function_pointer<MemberFuncPtr> && pointer<ObjPtr> &&
        requires(const MemberFuncPtr mem_func_ptr, const ObjPtr obj_ptr, Args &&... args) {
            (obj_ptr->*mem_func_ptr)(frd::forward<Args>(args)...);
        };

    template<typename MemberFuncPtr, typename ObjPtr, typename... Args>
    requires (_member_func_callable_with_ptr<MemberFuncPtr, ObjPtr, Args...>)
    [[nodiscard]]
    constexpr decltype(auto) invoke(const MemberFuncPtr mem_func_ptr, const ObjPtr obj_ptr, Args &&... args)
    noexcept(
        noexcept((obj_ptr->*mem_func_ptr)(frd::forward<Args>(args)...))
    ) {
        return (obj_ptr->*mem_func_ptr)(frd::forward<Args>(args)...);
    }

    template<typename Invocable, typename... Args>
    concept invocable = requires(Invocable &&inv, Args &&...args) {
        invoke(frd::forward<Invocable>(inv), frd::forward<Args>(args)...);
    };

    template<typename Invocable, typename... Args>
    concept nothrow_invocable = invocable<Invocable, Args...> && noexcept(invoke(frd::declval<Invocable>(), frd::declval<Args>()...));

    template<typename Invocable, typename... Args>
    requires (invocable<Invocable, Args...>)
    using invoke_result = decltype(invoke(frd::declval<Invocable>(), frd::declval<Args>()...));

    template<typename Ret, typename Invocable, typename... Args>
    requires (invocable<Invocable, Args...>)
    [[nodiscard]]
    constexpr Ret invoke_r(Invocable &&inv, Args &&... args)
    noexcept(
        noexcept(invoke(frd::forward<Invocable>(inv), frd::forward<Args>(args)...))
    ) {
        if constexpr (void_type<Ret>) {
            /* If 'Ret' is a void type, discard the return value. */
            static_cast<void>(invoke(frd::forward<Invocable>(inv), frd::forward<Args>(args)...));
        } else {
            return invoke(frd::forward<Invocable>(inv), frd::forward<Args>(args)...);
        }
    }

    template<typename Child, move_constructible Fn, move_constructible... BoundArgs>
    struct _bind_front_back_impl {
        static constexpr frd::size_t NumBoundArgs = sizeof...(BoundArgs);

        Fn _fn;
        frd::tuple<BoundArgs...> _bound_args;

        /* Add leading dummy 'int' argument to avoid ambiguity with copy/move constructors. */
        template<typename FnOther, typename... BoundArgsOther>
        requires (constructible_from<Fn, FnOther> && (constructible_from<BoundArgs, BoundArgsOther> && ...))
        constexpr _bind_front_back_impl(int, FnOther &&fn, BoundArgsOther &&... bound_args)
        noexcept(
            nothrow_constructible_from<Fn, FnOther> && (nothrow_constructible_from<BoundArgs, BoundArgsOther> && ...)
        )
            : _fn(frd::forward<FnOther>(fn)), _bound_args(frd::forward<BoundArgsOther>(bound_args)...) { }

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
        ){
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
    constexpr auto bind_front(Fn &&fn, BoundArgs &&... bound_args)
    noexcept(
        nothrow_constructible_from<_bind_front_fn<decay<Fn>, decay<BoundArgs>...>, int, Fn, BoundArgs...>
    ) {
        return _bind_front_fn<decay<Fn>, decay<BoundArgs>...>(
            0,
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
        ){
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
    constexpr auto bind_back(Fn &&fn, BoundArgs &&... bound_args)
    noexcept(
        nothrow_constructible_from<_bind_back_fn<decay<Fn>, decay<BoundArgs>...>, int, Fn, BoundArgs...>
    ) {
        return _bind_back_fn<decay<Fn>, decay<BoundArgs>...>(
            0,
            frd::forward<Fn>(fn),
            frd::forward<BoundArgs>(bound_args)...
        );
    }

}
