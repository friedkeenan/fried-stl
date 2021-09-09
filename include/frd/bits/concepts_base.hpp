#pragma once

#include <type_traits>
#include <concepts>

#include <frd/bits/type_traits_base.hpp>
#include <frd/bits/utility_base.hpp>

namespace frd {

    template<typename T, typename U>
    concept same_as = is_same<T, U>;

    template<typename T, typename U>
    concept same_as_without_cv = same_as<remove_cv<T>, remove_cv<U>>;

    template<typename From, typename To>
    concept implicitly_convertible_to = requires(void (&func)(To), From from) {
        func(from);
    };

    template<typename From, typename To>
    concept nothrow_implicitly_convertible_to = implicitly_convertible_to<From, To> &&
        requires(void (&func)(To), From from) {
            requires noexcept(func(from));
        };

    template<typename From, typename To>
    concept convertible_to = (
        implicitly_convertible_to<From, To> &&
        requires(add_rvalue_reference<From> (&func)()) {
            static_cast<To>(func());
        }
    );

    template<typename B>
    concept boolean_testable = convertible_to<B, bool> && requires(B &&b) {
        { !forward<B>(b) } -> convertible_to<bool>;
    };

    template<typename T>
    concept integral = (
        same_as_without_cv<remove_signedness<T>, bool>      ||
        same_as_without_cv<remove_signedness<T>, char>      ||
        same_as_without_cv<remove_signedness<T>, char8_t>   ||
        same_as_without_cv<remove_signedness<T>, char16_t>  ||
        same_as_without_cv<remove_signedness<T>, char32_t>  ||
        same_as_without_cv<remove_signedness<T>, wchar_t>   ||
        same_as_without_cv<remove_signedness<T>, short>     ||
        same_as_without_cv<remove_signedness<T>, int>       ||
        same_as_without_cv<remove_signedness<T>, long>      ||
        same_as_without_cv<remove_signedness<T>, long long>
    );

    template<typename T>
    concept floating_point = (
        same_as_without_cv<T, float>       ||
        same_as_without_cv<T, double>      ||
        same_as_without_cv<T, long double>
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
    concept pointer = !same_as<remove_pointer<T>, T>;

    /*
        Must be implemented through variable templates as adding 'const'
        to a function type does not change anything, so you need specialization.
    */
    template<typename T>
    concept const_type = is_const<T>;

    template<typename T>
    concept unbound_array = is_unbound_array<T>;

    template<typename T>
    concept bound_array = is_bound_array<T>;

    /* Can't be called 'array' as that would conflict with the container type. */
    template<typename T>
    concept array_type = unbound_array<T> || bound_array<T>;

    template<typename T>
    concept enum_type = is_enum<T>;

    template<typename T>
    concept void_type = same_as_without_cv<T, void>;

    template<typename T>
    concept function = !const_type<const T> && !reference<T>;

    template<typename T>
    concept function_pointer = pointer<T> && function<remove_pointer<T>>;

    template<typename T>
    concept member_pointer = _is_member_pointer<remove_cv<T>>;

    template<typename T>
    concept member_function_pointer = member_pointer<T> && function<member_pointer_underlying<T>>;

    template<typename T>
    concept member_object_pointer = member_pointer<T> && !function<member_pointer_underlying<T>>;

    template<typename T>
    concept object = (
        !function<T>  &&
        !reference<T> &&
        !void_type<T>
    );

    template<typename T>
    concept class_type = requires {
        /* Check if you can have a member pointer for T. */
        typename type_holder<int T::*>;
    };

    template<typename T>
    concept incomplete = !requires {
        /* You cannot find the size of incomplete types. */
        sizeof(T);
    };

    template<typename Derived, typename Base>
    concept derived_from = class_type<Derived> && class_type<Base> && std::convertible_to<const volatile Derived *, const volatile Base *>;

    template<typename T>
    concept referenceable = !void_type<T>;

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
            { lhs = frd::forward<RHS>(rhs) } -> same_as<LHS>;
        }
    );

    template<typename T>
    concept destructible =
        !(
            void_type<T>     ||
            function<T>      ||
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
    concept constructible_from = (
        destructible<T>                     &&
        (reference<T> || object<T>)         &&
        requires(Args &&... args) {
            T(frd::forward<Args>(args)...);
        }
    );

    template<typename T>
    concept default_constructible = constructible_from<T> && requires {
        T{};

        /*
            Ensure 'T t;' is well formed.

            Cannot do 'T t;' for whatever reason.
        */
        ::new(static_cast<void *>(nullptr)) T;
    };

    template<typename T>
    concept move_constructible = constructible_from<T, T> && convertible_to<T, T>;

    template<typename T>
    concept move_assignable = assignable_from<T &, T &&>;

    template<typename T>
    concept copy_constructible = (
        move_constructible<T>                                            &&
        constructible_from<T, T &>       && convertible_to<T &,       T> &&
        constructible_from<T, const T &> && convertible_to<const T &, T> &&
        constructible_from<T, const T>   && convertible_to<const T,   T>
    );

    template<typename T>
    concept copy_assignable = move_assignable<T> && assignable_from<T &, const T &>;

    template<typename T, typename U>
    concept weakly_equality_comparable_with = requires(remove_reference<T> &t, remove_reference<U> &u) {
        { t == u } -> boolean_testable;
        { t != u } -> boolean_testable;
        { u == t } -> boolean_testable;
        { u != t } -> boolean_testable;
    };

    template<typename T>
    concept equality_comparable = weakly_equality_comparable_with<T, T>;

    template<typename T, typename U>
    concept weakly_less_than_comparable_with = requires(remove_reference<T> &t, remove_reference<U> &u) {
        { t < u } -> boolean_testable;
        { u < t } -> boolean_testable;
    };

    template<typename T>
    concept less_than_comparable = weakly_less_than_comparable_with<T, T>;

    template<typename T, typename U, typename Result = remove_reference<T>>
    concept weakly_addable_with = requires(remove_reference<T> &t, remove_reference<U> &u) {
        { t + u } -> same_as<Result>;
        { u + t } -> same_as<Result>;
    };

    template<typename T>
    concept addable = weakly_addable_with<T, T>;

    template<typename T, typename U>
    concept in_place_addable_with = requires(remove_reference<T> &t, U u) {
        { t += u } -> same_as<remove_reference<T> &>;
    };

    template<typename T, typename U, typename Result = remove_reference<T>>
    concept weakly_subtractable_with = requires(remove_reference<T> &t, remove_reference<U> &u) {
        { t - u } -> same_as<Result>;
        { u - t } -> same_as<Result>;
    };

    template<typename T>
    concept subtractable = weakly_subtractable_with<T, T>;

    template<typename T, typename U>
    concept in_place_subtractable_with = requires(remove_reference<T> &t, U u) {
        { t -= u } -> same_as<remove_reference<T> &>;
    };

    template<typename T, typename ToForward>
    concept forwarder_for = same_as<remove_cvref<T>, ToForward>;

    /* Only concepts that are forced to use STL APIs below. */

    template<typename T>
    concept trivially_copyable = std::is_trivially_copyable_v<T>;

    template<typename T>
    concept empty = std::is_empty_v<T>;

}
