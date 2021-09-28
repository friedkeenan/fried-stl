#pragma once

#include <type_traits>

#include <frd/bits/arithmetic_base.hpp>

#include <frd/defines.hpp>

namespace frd {

    namespace unsafe {

        /*
            An implementation of 'std::integral_constant' where you
            don't need to specify the type.

            It would be incorrect to use this in certain situations, such
            as with 'std::tuple_size' as it is UB to have a specialization
            of 'std::tuple_size' not inherit from 'std::integral_constant<std::size_t, N>'.
        */
        template<auto v>
        struct constant_holder {
            using value_type = decltype(v);
            using type       = constant_holder;

            static constexpr value_type value = v;

            constexpr operator value_type() const noexcept {
                return value;
            }

            constexpr value_type operator ()() const noexcept {
                return value;
            }
        };

    }

    template<auto v>
    struct constant_holder : std::integral_constant<decltype(v), v> { };

    using false_holder = constant_holder<false>;
    using true_holder  = constant_holder<true>;

    template<typename Head, typename... Tail> constexpr inline bool dependent_false = false;

    template<typename T, typename U> constexpr inline bool is_same       = false;
    template<typename T>             constexpr inline bool is_same<T, T> = true;

    template<typename T> constexpr inline bool is_const          = false;
    template<typename T> constexpr inline bool is_const<const T> = true;

    template<typename T> constexpr inline bool is_lvalue_reference      = false;
    template<typename T> constexpr inline bool is_lvalue_reference<T &> = true;

    template<typename T> constexpr inline bool is_rvalue_reference       = false;
    template<typename T> constexpr inline bool is_rvalue_reference<T &&> = true;

    template<typename T> constexpr inline bool is_unbound_array      = false;
    template<typename T> constexpr inline bool is_unbound_array<T[]> = true;

    template<typename T>                constexpr inline bool is_bound_array       = false;
    template<typename T, frd::size_t N> constexpr inline bool is_bound_array<T[N]> = true;

    template<typename T>                        constexpr inline bool _is_member_pointer                    = false;
    template<typename MemberType, typename Cls> constexpr inline bool _is_member_pointer<MemberType Cls::*> = true;

    template<typename T, frd::size_t Dim = 0>            constexpr inline frd::size_t extent            = 0;
    template<typename T>                                 constexpr inline frd::size_t extent<T[],  0>   = 0;
    template<typename T, frd::size_t Dim>                constexpr inline frd::size_t extent<T[],  Dim> = extent<T, Dim - 1>;
    template<typename T, frd::size_t N>                  constexpr inline frd::size_t extent<T[N], 0>   = N;
    template<typename T, frd::size_t N, frd::size_t Dim> constexpr inline frd::size_t extent<T[N], Dim> = extent<T, Dim - 1>;

    /* Only templated variables that are forced to use STL APIs below. */

    template<typename T>
    constexpr inline bool is_enum = std::is_enum_v<T>;

    template<typename T>
    constexpr inline bool is_union = std::is_union_v<T>;

    /* Only operations on types below. */

    template<typename T>
    struct type_holder {
        using type = T;
    };

    /*
        Needs to be more complex than just 'using type_identity = T;'
        to aid in template deduction.
    */
    template<typename T>
    using type_identity = typename type_holder<T>::type;

    template<typename...>
    struct type_list;

    template<typename Head, typename... Tail>
    struct type_list<Head, Tail...> {
        using head = Head;
        using tail = type_list<Tail...>;
    };

    template<>
    struct type_list<> { };

    template<typename T>
    struct _template_args;

    template<template<typename...> typename Template, typename... Args>
    struct _template_args<Template<Args...>> : type_holder<type_list<Args...>> { };

    template<typename T>
    using template_args = typename _template_args<T>::type;

    template<typename T>
    struct _first_template_arg;

    template<template<typename, typename...> typename Template, typename Head, typename... Tail>
    struct _first_template_arg<Template<Head, Tail...>> : type_holder<Head> { };

    template<typename T>
    using first_template_arg = typename _first_template_arg<T>::type;

    template<frd::size_t N, typename... Args>
    struct _remove_leading_args;

    template<frd::size_t N, typename Head, typename... Tail>
    requires (N != 0)
    struct _remove_leading_args<N, Head, Tail...> : _remove_leading_args<N - 1, Tail...> { };

    template<typename... Args>
    struct _remove_leading_args<0, Args...> : type_holder<type_list<Args...>> { };

    template<frd::size_t N, typename... Args>
    using remove_leading_args = typename _remove_leading_args<N, Args...>::type;

    template<template<typename...> typename Template, typename NewArgs, typename TailArgs>
    struct _join_template_args;

    template<template<typename...> typename Template, typename... NewArgs, typename... TailArgs>
    struct _join_template_args<Template, type_list<NewArgs...>, type_list<TailArgs...>> : type_holder<Template<NewArgs..., TailArgs...>> { };

    template<template<typename...> typename Template, typename NewArgs, typename TailArgs>
    using join_template_args = typename _join_template_args<Template, NewArgs, TailArgs>::type;

    template<typename T, typename... NewArgs>
    struct _replace_leading_template_args;

    template<template<typename...> typename Template, typename... OldArgs, typename... NewArgs>
    struct _replace_leading_template_args<Template<OldArgs...>, NewArgs...>
    : type_holder<join_template_args<Template, type_list<NewArgs...>, remove_leading_args<sizeof...(NewArgs), OldArgs...>>> { };

    template<typename T, typename... NewArgs>
    using replace_leading_template_args = typename _replace_leading_template_args<T, NewArgs...>::type;

    template<typename T, typename NewArg>
    using replace_first_template_arg = replace_leading_template_args<T, NewArg>;

    template<frd::size_t BitSize, auto Operator, typename... Types>
    struct _type_for_bit_size;

    template<frd::size_t BitSize, auto Operator>
    struct _type_for_bit_size<BitSize, Operator> {
        static_assert(dependent_false<decltype(BitSize)>, "No type for specified bit size!");
    };

    template<frd::size_t BitSize, auto Operator, typename Head, typename... Tail>
    requires (Operator(FRD_BITSIZEOF(Head), BitSize))
    struct _type_for_bit_size<BitSize, Operator, Head, Tail...> : type_holder<Head> { };

    template<frd::size_t BitSize, auto Operator, typename Head, typename... Tail>
    struct _type_for_bit_size<BitSize, Operator, Head, Tail...> : _type_for_bit_size<BitSize, Operator, Tail...> { };

    template<frd::size_t BitSize, auto Operator, typename... Types>
    using type_for_bit_size = typename _type_for_bit_size<BitSize, Operator, Types...>::type;

    template<frd::size_t Size, typename... Types>
    struct _type_with_size;

    template<frd::size_t Size>
    struct _type_with_size<Size> {
        static_assert(dependent_false<decltype(Size)>, "No type with specified size!");
    };

    template<frd::size_t Size, typename Head, typename... Tail>
    requires (sizeof(Head) == Size)
    struct _type_with_size<Size, Head, Tail...> : type_holder<Head> { };

    template<frd::size_t Size, typename Head, typename... Tail>
    struct _type_with_size<Size, Head, Tail...> : _type_with_size<Size, Tail...> { };

    template<frd::size_t Size, typename... Types>
    using type_with_size = typename _type_with_size<Size, Types...>::type;

    template<typename Source, typename Destination> struct _match_const                            : type_holder<Destination> { };
    template<typename Source, typename Destination> struct _match_const<const Source, Destination> : type_holder<const Destination> { };

    template<typename Source, typename Destination>
    using match_const = typename _match_const<Source, Destination>::type;

    template<typename Source, typename Destination> struct _match_volatile                               : type_holder<Destination> { };
    template<typename Source, typename Destination> struct _match_volatile<volatile Source, Destination> : type_holder<volatile Destination> { };

    template<typename Source, typename Destination>
    using match_volatile = typename _match_volatile<Source, Destination>::type;

    template<typename Source, typename Destination>
    using match_cv = match_volatile<Source, match_const<Source, Destination>>;

    template<typename T> struct _remove_const          : type_holder<T> { };
    template<typename T> struct _remove_const<const T> : type_holder<T> { };

    template<typename T>
    using remove_const = typename _remove_const<T>::type;

    template<typename T> struct _remove_volatile             : type_holder<T> { };
    template<typename T> struct _remove_volatile<volatile T> : type_holder<T> { };

    template<typename T>
    using remove_volatile = typename _remove_volatile<T>::type;

    template<typename T>
    using remove_cv = remove_volatile<remove_const<T>>;

    template<typename T> struct _remove_reference       : type_holder<T> { };
    template<typename T> struct _remove_reference<T &>  : type_holder<T> { };
    template<typename T> struct _remove_reference<T &&> : type_holder<T> { };

    template<typename T>
    using remove_reference = typename _remove_reference<T>::type;

    template<typename T>
    using remove_cvref = remove_cv<remove_reference<T>>;

    /*
        Need the overloads for cv-qualified pointers as removing
        a pointer off a non-pointer type should not remove cv qualifiers.
    */
    template<typename T> struct _remove_pointer                     : type_holder<T> { };
    template<typename T> struct _remove_pointer<T *               > : type_holder<T> { };
    template<typename T> struct _remove_pointer<T * const         > : type_holder<T> { };
    template<typename T> struct _remove_pointer<T *       volatile> : type_holder<T> { };
    template<typename T> struct _remove_pointer<T * const volatile> : type_holder<T> { };

    template<typename T>
    using remove_pointer = typename _remove_pointer<T>::type;

    template<typename T>                struct _remove_extent       : type_holder<T> { };
    template<typename T>                struct _remove_extent<T[]>  : type_holder<T> { };
    template<typename T, frd::size_t N> struct _remove_extent<T[N]> : type_holder<T> { };

    template<typename T>
    using remove_extent = typename _remove_extent<T>::type;

    template<typename T>                struct _remove_all_extents       : type_holder<T> { };
    template<typename T>                struct _remove_all_extents<T[]>  : _remove_all_extents<T> { };
    template<typename T, frd::size_t N> struct _remove_all_extents<T[N]> : _remove_all_extents<T> { };

    template<typename T>
    using remove_all_extents = typename _remove_all_extents<T>::type;

    template<typename T> struct _remove_signed   : type_holder<T> { };
    template<typename T> struct _remove_unsigned : type_holder<T> { };

    #define REMOVE_SIGNEDNESS(cls)                                               \
        template<> struct _remove_signed  <signed   cls> : type_holder<cls> { }; \
        template<> struct _remove_unsigned<unsigned cls> : type_holder<cls> { }

    REMOVE_SIGNEDNESS(char);
    REMOVE_SIGNEDNESS(short);
    REMOVE_SIGNEDNESS(int);
    REMOVE_SIGNEDNESS(long);
    REMOVE_SIGNEDNESS(long long);

    #undef REMOVE_SIGNEDNESS

    template<typename T>
    using remove_signed = match_cv<T, typename _remove_signed<remove_cv<T>>::type>;

    template<typename T>
    using remove_unsigned = match_cv<T, typename _remove_unsigned<remove_cv<T>>::type>;

    template<typename T>
    using remove_signedness = remove_unsigned<remove_signed<T>>;

    template<typename T> struct _make_signed   : type_holder<T> { };
    template<typename T> struct _make_unsigned : type_holder<T> { };

    #define ADD_SIGNEDNESS(cls)                                                \
        template<> struct _make_signed  <cls> : type_holder<  signed cls> { }; \
        template<> struct _make_unsigned<cls> : type_holder<unsigned cls> { }

    ADD_SIGNEDNESS(char);
    ADD_SIGNEDNESS(short);
    ADD_SIGNEDNESS(int);
    ADD_SIGNEDNESS(long);
    ADD_SIGNEDNESS(long long);

    #undef ADD_SIGNEDNESS

    template<typename T>
    using _ranked_int_with_same_size = type_with_size<sizeof(T),
        signed char,
        signed short,
        signed int,
        signed long,
        signed long long
    >;

    template<typename T>
    using _ranked_uint_with_same_size = type_with_size<sizeof(T),
        unsigned char,
        unsigned short,
        unsigned int,
        unsigned long,
        unsigned long long
    >;

    template<typename T> requires (is_enum<T>) struct _make_signed<T>   : type_holder<_ranked_int_with_same_size <T>> { };
    template<typename T> requires (is_enum<T>) struct _make_unsigned<T> : type_holder<_ranked_uint_with_same_size<T>> { };

    #define ADD_RANKED_SIGNEDNESS(cls)                                                             \
        template<> struct _make_signed<cls>   : type_holder<_ranked_int_with_same_size <cls>> { }; \
        template<> struct _make_unsigned<cls> : type_holder<_ranked_uint_with_same_size<cls>> { }

    ADD_RANKED_SIGNEDNESS(wchar_t);
    ADD_RANKED_SIGNEDNESS(char8_t);
    ADD_RANKED_SIGNEDNESS(char16_t);
    ADD_RANKED_SIGNEDNESS(char32_t);

    #undef ADD_RANKED_SIGNEDNESS

    template<typename T> using make_signed   = match_cv<T, typename _make_signed  <remove_cv<remove_signedness<T>>>::type>;
    template<typename T> using make_unsigned = match_cv<T, typename _make_unsigned<remove_cv<remove_signedness<T>>>::type>;

    template<typename T>
    using add_const = const T;

    template<typename T>
    using add_volatile = volatile T;

    template<typename T>
    using add_cv = const volatile T;

    template<typename T> struct _add_lvalue_reference : type_holder<T &> { };

    /* 'void' cannot be referenced, so specialize it. */
    template<typename T>
    requires (is_same<remove_cv<T>, void>)
    struct _add_lvalue_reference<T> : type_holder<T> { };

    template<typename T>
    using add_lvalue_reference = typename _add_lvalue_reference<T>::type;

    template<typename T> struct _add_rvalue_reference : type_holder<T &&> { };

    /* 'void' cannot be referenced, so specialize it. */
    template<typename T>
    requires (is_same<remove_cv<T>, void>)
    struct _add_rvalue_reference<T> : type_holder<T> { };

    template<typename T>
    using add_rvalue_reference = typename _add_rvalue_reference<T>::type;

    template<typename T>
    using add_pointer = remove_reference<T> *;

    template<template<typename...> typename Op, typename... Args>
    concept _op_works = requires {
        typename Op<Args...>;
    };

    template<typename Default, template<typename...> typename Op, typename... Args>
    struct _detected_else : type_holder<Default> { };

    template<typename Default, template<typename...> typename Op, typename... Args>
    requires (_op_works<Op, Args...>)
    struct _detected_else<Default, Op, Args...> : type_holder<Op<Args...>> { };

    template<typename Default, template<typename...> typename Op, typename... Args>
    using detected_else = typename _detected_else<Default, Op, Args...>::type;

    template<bool Condition, typename Success, typename Failure>
    struct _conditional;

    template<typename Success, typename Failure> struct _conditional<true,  Success, Failure> : type_holder<Success> { };
    template<typename Success, typename Failure> struct _conditional<false, Success, Failure> : type_holder<Failure> { };

    template<bool Condition, typename Success, typename Failure>
    using conditional = typename _conditional<Condition, Success, Failure>::type;

    template<typename T>
    requires (_is_member_pointer<remove_cv<T>>)
    struct _member_pointer_underlying;

    template<typename MemberType, typename Cls>
    struct _member_pointer_underlying<MemberType Cls::*> : type_holder<MemberType> { };

    template<typename T>
    using member_pointer_underlying = typename _member_pointer_underlying<remove_cv<T>>::type;

    template<typename T>
    requires (_is_member_pointer<remove_cv<T>>)
    struct _member_pointer_class;

    template<typename MemberType, typename Cls>
    struct _member_pointer_class<MemberType Cls::*> : type_holder<Cls> { };

    template<typename T>
    using member_pointer_class = typename _member_pointer_class<remove_cv<T>>::type;

    struct inert_type {
        constexpr inert_type() noexcept = default;

        template<typename... Args>
        constexpr inert_type(Args &&... args) noexcept {
            FRD_UNUSED(args...);
        }
    };

    template<bool IsPresent, typename T>
    using maybe_present = conditional<IsPresent, T, inert_type>;

    /* Only operations on types that are forced to use STL APIs below. */

    template<typename T>
    using underlying_type = std::underlying_type_t<T>;

}
