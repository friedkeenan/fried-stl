#pragma once

#include <type_traits>

#include <frd/defines.hpp>

namespace frd {

    using _size_t = decltype(sizeof(int));

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

    using false_holder = constant_holder<false>;
    using true_holder  = constant_holder<true>;

    template<typename Head, typename... Tail> constexpr inline bool dependent_false = false;

    template<typename T, typename U> constexpr inline bool is_same       = false;
    template<typename T>             constexpr inline bool is_same<T, T> = true;

    template<typename T> constexpr inline bool is_lvalue_reference      = false;
    template<typename T> constexpr inline bool is_lvalue_reference<T &> = true;

    template<typename T> constexpr inline bool is_rvalue_reference       = false;
    template<typename T> constexpr inline bool is_rvalue_reference<T &&> = true;

    template<typename T> constexpr inline bool is_unbound_array      = false;
    template<typename T> constexpr inline bool is_unbound_array<T[]> = true;

    template<typename T>            constexpr inline bool is_bound_array       = false;
    template<typename T, _size_t N> constexpr inline bool is_bound_array<T[N]> = true;

    /* Only templated variables that are forced to use STL APIs below. */

    template<typename T>
    constexpr inline bool is_enum = std::is_enum_v<T>;

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

    template<size_t N, typename... Args>
    struct _remove_leading_args;

    template<size_t N, typename Head, typename... Tail>
    requires (N != 0)
    struct _remove_leading_args<N, Head, Tail...> : _remove_leading_args<N - 1, Tail...> { };

    template<typename... Args>
    struct _remove_leading_args<0, Args...> : type_holder<type_list<Args...>> { };

    template<size_t N, typename... Args>
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

    template<_size_t BitSize, auto Operator, typename... Types>
    struct _type_for_bit_size;

    template<_size_t BitSize, auto Operator>
    struct _type_for_bit_size<BitSize, Operator> {
        static_assert(dependent_false<decltype(BitSize)>, "No type for specified bit size!");
    };

    template<_size_t BitSize, auto Operator, typename Head, typename... Tail>
    requires (Operator(BITSIZEOF(Head), BitSize))
    struct _type_for_bit_size<BitSize, Operator, Head, Tail...> : type_holder<Head> { };

    template<_size_t BitSize, auto Operator, typename Head, typename... Tail>
    struct _type_for_bit_size<BitSize, Operator, Head, Tail...> : _type_for_bit_size<BitSize, Operator, Tail...> { };

    template<_size_t BitSize, auto Operator, typename... Types>
    using type_for_bit_size = typename _type_for_bit_size<BitSize, Operator, Types...>::type;

    template<_size_t Size, typename... Types>
    struct _type_with_size;

    template<_size_t Size>
    struct _type_with_size<Size> {
        static_assert(dependent_false<decltype(Size)>, "No type with specified size!");
    };

    template<_size_t Size, typename Head, typename... Tail>
    requires (sizeof(Head) == Size)
    struct _type_with_size<Size, Head, Tail...> : type_holder<Head> { };

    template<_size_t Size, typename Head, typename... Tail>
    struct _type_with_size<Size, Head, Tail...> : _type_with_size<Size, Tail...> { };

    template<_size_t Size, typename... Types>
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

    template<typename T>            struct _remove_extent       : type_holder<T> { };
    template<typename T>            struct _remove_extent<T[]>  : type_holder<T> { };
    template<typename T, _size_t N> struct _remove_extent<T[N]> : type_holder<T> { };

    template<typename T>
    using remove_extent = typename _remove_extent<T>::type;

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
    using add_rvalue_reference = typename _add_lvalue_reference<T>::type;

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

    /* Only operations on types that are forced to use STL APIs below. */

    template<typename T>
    using underlying_type = std::underlying_type_t<T>;

}
