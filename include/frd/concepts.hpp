#pragma once

#include <type_traits>

#include <frd/utility.hpp>
#include <frd/type_traits.hpp>

namespace frd {

    template<typename T, typename U>
    concept same_as = is_same<T, U>;

    template<typename From, typename To>
    concept convertible_to = requires (add_rvalue_reference<From> (&func)(To), From from) {
        static_cast<To>(func(from));
    };

    template<typename B>
    concept boolean_testable = convertible_to<B, bool> && requires(B &&b) {
        { !forward<B>(b) } -> convertible_to<bool>;
    };

    template<typename T>
    concept integral = (
        same_as<remove_signedness<remove_cv<T>>, bool>      ||
        same_as<remove_signedness<remove_cv<T>>, char>      ||
        same_as<remove_signedness<remove_cv<T>>, char8_t>   ||
        same_as<remove_signedness<remove_cv<T>>, char16_t>  ||
        same_as<remove_signedness<remove_cv<T>>, char32_t>  ||
        same_as<remove_signedness<remove_cv<T>>, wchar_t>   ||
        same_as<remove_signedness<remove_cv<T>>, short>     ||
        same_as<remove_signedness<remove_cv<T>>, int>       ||
        same_as<remove_signedness<remove_cv<T>>, long>      ||
        same_as<remove_signedness<remove_cv<T>>, long long>
    );

    template<typename T>
    concept floating_point = (
        same_as<remove_cv<T>, float>       ||
        same_as<remove_cv<T>, double>      ||
        same_as<remove_cv<T>, long double>
    );

    template<typename T>
    concept arithmetic = integral<T> || floating_point<T>;

    template<typename T>
    concept signed_type = arithmetic<T> && (T{-1} < T{0});

    template<typename T>
    concept unsigned_type = arithmetic<T> && !signed_type<T>;

    template<typename T>
    concept signed_integral = integral<T> && signed_type<T>;

    template<typename T>
    concept unsigned_integral = integral<T> && unsigned_type<T>;

    template<typename T>
    concept lvalue_reference = is_lvalue_reference<T>;

    template<typename T>
    concept rvalue_reference = is_rvalue_reference<T>;

    template<typename T>
    concept reference = lvalue_reference<T> || rvalue_reference<T>;

    template<typename T>
    concept const_type = same_as<T, const T>;

    template<typename T>
    concept unbound_array = is_unbound_array<T>;

    template<typename T>
    concept bound_array = is_bound_array<T>;

    /* Can't be called 'array' as that would conflict with the container type. */
    template<typename T>
    concept array_type = unbound_array<T> || bound_array<T>;

    template<typename T>
    concept function = !const_type<const T> && !reference<T>;

    template<typename T>
    concept object = (
        !function<T>      &&
        !reference<T>     &&
        !same_as<T, void>
    );

    template<typename T, typename U>
    concept common_reference_with = (
        same_as<std::common_reference_t<T, U>, std::common_reference_t<U, T>> &&
        convertible_to<T, std::common_reference_t<T, U>>                      &&
        convertible_to<U, std::common_reference_t<T, U>>
    );

    template<typename LHS, typename RHS>
    concept assignable_from = (
        lvalue_reference<LHS>                            &&
        common_reference_with<
            const remove_reference<LHS> &,
            const remove_reference<RHS> &
        >                                                &&
        requires(LHS lhs, RHS &&rhs) {
            { lhs = forward<RHS>(rhs) } -> same_as<LHS>;
        }
    );

    template<typename T>
    concept destructible =
        !(
            same_as<remove_cv<T>, void> ||
            function<T>                 ||
            unbound_array<T>
        ) &&
        (
            reference<T> ||
            (
                requires(T &t) {
                    t.~T();
                } &&
                noexcept(declval<T &>().~T())
            )
        );

    template<typename T, typename... Args>
    concept constructible_from =
        destructible<T> &&
        (reference<T> || object<T>) &&
        requires(Args &&... args) {
            T(forward<Args>(args)...);
        };

    template<typename T>
    concept move_constructible = constructible_from<T, T> && convertible_to<T, T>;

    template<typename T>
    concept move_assignable = assignable_from<T &, T &&>;

    template<typename T>
    concept copy_constructible =
        move_constructible<T> &&
        constructible_from<T, T &>       && convertible_to<T &,       T> &&
        constructible_from<T, const T &> && convertible_to<const T &, T> &&
        constructible_from<T, const T>   && convertible_to<const T,   T>;

    template<typename T>
    concept copy_assignable = move_assignable<T> && assignable_from<T &, const T &>;

    template<typename T, typename ToForward>
    concept forwarder = same_as<remove_reference<T>, ToForward>;

    /* Only concepts that are forced to use std APIs below. */

    template<typename T>
    concept trivially_copyable = std::is_trivially_copyable_v<T>;

    template<typename T>
    concept empty = std::is_empty_v<T>;

}
