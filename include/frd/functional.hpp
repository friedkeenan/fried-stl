#pragma once

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
    constexpr decltype(auto) invoke(Invocable &&inv, Args &&... args) noexcept(noexcept(frd::forward<Invocable>(inv)(frd::forward<Args>(args)...))) {
        return frd::forward<Invocable>(inv)(frd::forward<Args>(args)...);
    }

    /*
        For member pointers, the object they are being
        used on must be derived from the class the member
        pointer is for.
    */

    template<member_object_pointer MemberObjPtr, typename Obj>
    requires (derived_from<remove_reference<Obj>, member_pointer_class<MemberObjPtr>>)
    [[nodiscard]]
    constexpr decltype(auto) invoke(const MemberObjPtr mem_obj_ptr, Obj &&obj) noexcept {
        return forward<Obj>(obj).*mem_obj_ptr;
    }

    template<member_object_pointer MemberObjPtr, pointer ObjPtr>
    requires (derived_from<remove_pointer<ObjPtr>, member_pointer_class<MemberObjPtr>>)
    [[nodiscard]]
    constexpr decltype(auto) invoke(const MemberObjPtr mem_obj_ptr, const ObjPtr obj_ptr) noexcept {
        return invoke(mem_obj_ptr, *obj_ptr);
    }

    /* Const member functions can be used on const or non-const objects. */

    template<const_member_function_pointer MemberFuncPtr, typename Obj, typename... Args>
    requires (
        derived_from<remove_reference<Obj>, member_pointer_class<MemberFuncPtr>>                   &&
        _normal_callable<to_non_const_function<member_pointer_underlying<MemberFuncPtr>>, Args...>
    )
    [[nodiscard]]
    constexpr decltype(auto) invoke(const MemberFuncPtr mem_func_ptr, Obj &&obj, Args &&... args)
    noexcept(
        noexcept((frd::forward<Obj>(obj).*mem_func_ptr)(frd::forward<Args>(args)...))
    ){
        return (frd::forward<Obj>(obj).*mem_func_ptr)(frd::forward<Args>(args)...);
    }

    template<const_member_function_pointer MemberFuncPtr, pointer ObjPtr, typename... Args>
    requires (
        derived_from<remove_pointer<ObjPtr>, member_pointer_class<MemberFuncPtr>>                  &&
        _normal_callable<to_non_const_function<member_pointer_underlying<MemberFuncPtr>>, Args...>
    )
    [[nodiscard]]
    constexpr decltype(auto) invoke(const MemberFuncPtr mem_func_ptr, const ObjPtr obj_ptr, Args &&... args)
    noexcept(
        noexcept(invoke(mem_func_ptr, *obj_ptr, frd::forward<Args>(args)...))
    ) {
        return invoke(mem_func_ptr, *obj_ptr, frd::forward<Args>(args)...);
    }

    /* Non-const member functions can only be used on non-const objects. */

    template<member_function_pointer MemberFuncPtr, typename Obj, typename... Args>
    requires (
        derived_from<remove_reference<Obj>, member_pointer_class<MemberFuncPtr>> &&
        !const_type<remove_reference<Obj>>                                       &&
        _normal_callable<member_pointer_underlying<MemberFuncPtr>, Args...>
    )
    [[nodiscard]]
    constexpr decltype(auto) invoke(const MemberFuncPtr mem_func_ptr, Obj &&obj, Args &&... args)
    noexcept(
        noexcept((frd::forward<Obj>(obj).*mem_func_ptr)(frd::forward<Args>(args)...))
    ) {
        return (frd::forward<Obj>(obj).*mem_func_ptr)(frd::forward<Args>(args)...);
    }

    template<member_function_pointer MemberFuncPtr, pointer ObjPtr, typename... Args>
    requires (
        derived_from<remove_pointer<ObjPtr>, member_pointer_class<MemberFuncPtr>> &&
        !const_type<remove_pointer<ObjPtr>>                                       &&
        _normal_callable<member_pointer_underlying<MemberFuncPtr>, Args...>
    )
    [[nodiscard]]
    constexpr decltype(auto) invoke(const MemberFuncPtr mem_func_ptr, const ObjPtr obj_ptr, Args &&... args)
    noexcept(
        noexcept(invoke(mem_func_ptr, *obj_ptr, frd::forward<Args>(args)...))
    ) {
        return invoke(mem_func_ptr, *obj_ptr, frd::forward<Args>(args)...);
    }

    template<typename Invocable, typename... Args>
    concept invocable = requires(Invocable &&inv, Args &&...args) {
        invoke(frd::forward<Invocable>(inv), frd::forward<Args>(args)...);
    };

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

}
