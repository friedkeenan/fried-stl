#pragma once

#include <frd/utility.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /* TODO: Handle reference wrappers for 'invoke'? */

    /*
        NOTE: We do not add [[nodiscard]] to 'invoke' as we don't know what the
        invocable's nodiscard status is.
    */

    template<typename Invocable, typename... Args>
    concept _normal_callable = requires(Invocable &&inv, Args &&... args) {
        frd::forward<Invocable>(inv)(frd::forward<Args>(args)...);
    };

    template<typename Invocable, typename... Args>
    requires (_normal_callable<Invocable, Args...>)
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
    constexpr decltype(auto) invoke(const MemberFuncPtr mem_func_ptr, const ObjPtr obj_ptr, Args &&... args)
    noexcept(
        noexcept((obj_ptr->*mem_func_ptr)(frd::forward<Args>(args)...))
    ) {
        return (obj_ptr->*mem_func_ptr)(frd::forward<Args>(args)...);
    }

    template<typename Invocable, typename... Args>
    concept invocable = requires(Invocable &&inv, Args &&...args) {
        ::frd::invoke(frd::forward<Invocable>(inv), frd::forward<Args>(args)...);
    };

    template<typename Invocable, typename... Args>
    concept nothrow_invocable = invocable<Invocable, Args...> && noexcept(frd::invoke(frd::declval<Invocable>(), frd::declval<Args>()...));

    template<typename Invocable, typename... Args>
    requires (invocable<Invocable, Args...>)
    using invoke_result = decltype(frd::invoke(frd::declval<Invocable>(), frd::declval<Args>()...));

    /* NOTE: We add [[nodiscard]] since if you're specifying a return type, you probably care about the return value. */
    template<typename Ret, typename Invocable, typename... Args>
    requires (invocable<Invocable, Args...>)
    [[nodiscard]]
    constexpr Ret invoke_r(Invocable &&inv, Args &&... args)
    noexcept(
        noexcept(invoke(frd::forward<Invocable>(inv), frd::forward<Args>(args)...))
    ) {
        if constexpr (void_type<Ret>) {
            /* If 'Ret' is a void type, discard the return value. */
            static_cast<void>(frd::invoke(frd::forward<Invocable>(inv), frd::forward<Args>(args)...));
        } else {
            return frd::invoke(frd::forward<Invocable>(inv), frd::forward<Args>(args)...);
        }
    }

}
