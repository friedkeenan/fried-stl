#pragma once

namespace frd {

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

    template<typename = void>
    constexpr inline bool dependent_false = false;

    template<typename T, typename U> constexpr inline bool is_same       = false;
    template<typename T>             constexpr inline bool is_same<T, T> = true;

    template<typename T> constexpr inline bool is_lvalue_reference      = false;
    template<typename T> constexpr inline bool is_lvalue_reference<T &> = true;

    template<typename T> constexpr inline bool is_rvalue_reference       = false;
    template<typename T> constexpr inline bool is_rvalue_reference<T &&> = true;

    template<typename T> constexpr inline bool is_unbound_array      = false;
    template<typename T> constexpr inline bool is_unbound_array<T[]> = true;

    template<typename T>                constexpr inline bool is_bound_array       = false;
    template<typename T, std::size_t N> constexpr inline bool is_bound_array<T[N]> = true;

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

    template<typename Source, typename Destination>
    struct _match_const : type_holder<Destination> { };

    template<typename Source, typename Destination>
    struct _match_const<const Source, Destination> : type_holder<const Destination> { };

    template<typename Source, typename Destination>
    using match_const = typename _match_const<Source, Destination>::type;

    template<typename Source, typename Destination>
    struct _match_volatile : type_holder<Destination> { };

    template<typename Source, typename Destination>
    struct _match_volatile<volatile Source, Destination> : type_holder<volatile Destination> { };

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

    template<typename T> struct _remove_reference       : type_holder<T> { };
    template<typename T> struct _remove_reference<T &>  : type_holder<T> { };
    template<typename T> struct _remove_reference<T &&> : type_holder<T> { };

    template<typename T>
    using remove_reference = typename _remove_reference<T>::type;

    template<typename T>
    using add_const = const T;

    template<typename T>
    using add_volatile = volatile T;

    template<typename T>
    using add_cv = const volatile T;

    template<typename T> struct _add_lvalue_reference : type_holder<T &> { };

    /* void cannot be referenced, so specialize it. */
    template<typename T>
    requires is_same<remove_cv<T>, void>
    struct _add_lvalue_reference<T> : type_holder<T> { };

    template<typename T>
    using add_lvalue_reference = typename _add_lvalue_reference<T>::type;

    template<typename T> struct _add_rvalue_reference : type_holder<T &&> { };

    /* void cannot be referenced, so specialize it. */
    template<typename T>
    requires is_same<remove_cv<T>, void>
    struct _add_rvalue_reference<T> : type_holder<T> { };

    template<typename T>
    using add_rvalue_reference = typename _add_lvalue_reference<T>::type;

    template<template<typename...> typename Op, typename... Args>
    concept _op_works = requires {
        type_holder<Op<Args...>>{};
    };

    template<typename Default, template<typename...> typename Op, typename... Args>
    struct _detected_else : type_holder<Default> { };

    template<typename Default, template<typename...> typename Op, typename... Args>
    requires _op_works<Op, Args...>
    struct _detected_else<Default, Op, Args...> : type_holder<Op<Args...>> { };

    template<typename Default, template<typename...> typename Op, typename... Args>
    using detected_else = typename _detected_else<Default, Op, Args...>::type;

}
