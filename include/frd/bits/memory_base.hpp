#pragma once

#include <frd/arithmetic.hpp>
#include <frd/limits.hpp>
#include <frd/utility.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename T>
    constexpr void destroy_at(T *location) noexcept(noexcept(location->~T())) {
        location->~T();
    }

    namespace unsafe {
        /*
            Implementations of unspecialized 'std::pointer_traits' and 'std::allocator_traits'.

            It would not be correct to use these implementations for
            allocator-aware types, as the aforementioned templates can be
            specialized (though you should not need to do so), but it's
            fun to implement I guess.
        */

        template<typename T, typename U>
        concept _has_rebind = requires {
            typename T::template rebind<U>;
        };

        template<typename T, typename U>
        struct _rebind : type_holder<replace_first_template_arg<T, U>> { };

        template<typename T, typename U>
        requires (_has_rebind<T, U>)
        struct _rebind<T, U> : type_holder<typename T::template rebind<U>> { };

        template<typename T, typename U>
        concept _has_rebind_other = requires {
            typename T::template rebind<U>::other;
        };

        template<typename T, typename U>
        struct _rebind_other : type_holder<replace_first_template_arg<T, U>> { };

        template<typename T, typename U>
        requires (_has_rebind_other<T, U>)
        struct _rebind_other<T, U> : type_holder<typename T::template rebind<U>::other> { };

        #define ATTR_ELSE(cls, attr, default_type)                 \
            template<typename T> using _##attr = typename T::attr; \
            using attr = detected_else<default_type, _##attr, cls>

        template<typename Ptr>
        struct pointer_traits {
            using pointer = Ptr;

            /*
                NOTE: If 'Ptr' is not a template specialization,
                'first_template_arg<Ptr>' is an incomplete type.
            */
            ATTR_ELSE(Ptr, element_type,    first_template_arg<Ptr>);
            ATTR_ELSE(Ptr, difference_type, ptrdiff_t);

            template<typename U>
            using rebind = typename _rebind<Ptr, U>::type;

            static constexpr pointer pointer_to(element_type &obj) noexcept {
                return Ptr::pointer_to(obj);
            }
        };

        template<typename T>
        struct pointer_traits<T *> {
            using pointer         = T *;
            using element_type    = T;
            using difference_type = ptrdiff_t;

            template<typename U>
            using rebind = U *;

            static constexpr pointer pointer_to(element_type &obj) noexcept {
                return std::addressof(obj);
            }
        };

        template<typename Allocator>
        struct allocator_traits {
            using allocator_type = Allocator;
            using value_type     = typename Allocator::value_type;

            ATTR_ELSE(Allocator, pointer,            value_type *);
            ATTR_ELSE(Allocator, const_pointer,      typename pointer_traits<pointer>::template rebind<const value_type>);
            ATTR_ELSE(Allocator, void_pointer,       typename pointer_traits<pointer>::template rebind<void>);
            ATTR_ELSE(Allocator, const_void_pointer, typename pointer_traits<pointer>::template rebind<const void>);
            ATTR_ELSE(Allocator, difference_type,    typename pointer_traits<pointer>::difference_type);
            ATTR_ELSE(Allocator, size_type,          make_unsigned<difference_type>);

            #define ATTR_ELSE_VALUE(name)                                               \
                template<typename U> using _##name##_impl = typename U::name;           \
                using _##name = detected_else<false_holder, _##name##_impl, Allocator>; \
                static constexpr bool name = _##name::value

            ATTR_ELSE_VALUE(propagate_on_container_copy_assignment);
            ATTR_ELSE_VALUE(propagate_on_container_move_assignment);
            ATTR_ELSE_VALUE(propagate_on_container_swap);

            #undef ATTR_ELSE_VALUE

            template<typename U> using _is_always_equal_impl = typename U::is_always_equal;
            using _is_always_equal = detected_else<constant_holder<empty_type<Allocator>>, _is_always_equal_impl, Allocator>;
            static constexpr bool always_equal = _is_always_equal::value;

            template<typename U>
            using rebind_alloc = typename _rebind_other<Allocator, U>::type;

            template<typename U>
            using rebind_traits = allocator_traits<rebind_alloc<U>>;

            [[nodiscard]]
            static constexpr pointer allocate(Allocator &alloc, size_type n) {
                return alloc.allocate(n);
            }

            [[nodiscard]]
            static constexpr pointer allocate(Allocator &alloc, size_type n, const_void_pointer hint) {
                if constexpr (requires {
                    alloc.allocate(n, hint);
                }) {
                    return alloc.allocate(n, hint);
                } else {
                    return alloc.allocate(n);
                }
            }

            static constexpr void deallocate(Allocator &alloc, pointer p, size_type n) {
                alloc.deallocate(p, n);
            }

            template<typename T, typename... Args>
            static constexpr void construct(Allocator &alloc, T *location, Args &&... args) {
                if constexpr (requires {
                    alloc.construct(location, frd::forward<Args>(args)...);
                }) {
                    alloc.construct(location, frd::forward<Args>(args)...);
                } else {
                    std::construct_at(location, frd::forward<Args>(args)...);
                }
            }

            template<typename T>
            static constexpr void destroy(Allocator &alloc, T *location) {
                if constexpr (requires {
                    alloc.destroy(location);
                }) {
                    alloc.destroy(location);
                } else {
                    destroy_at(location);
                }
            }

            static constexpr size_type max_size(const Allocator &alloc) noexcept {
                if constexpr (requires {
                    alloc.max_size();
                }) {
                    return alloc.max_size();
                } else {
                    return numeric_limits<size_type>::max / sizeof(value_type);
                }
            }

            static constexpr Allocator select_on_container_copy_construction(const Allocator &alloc) {
                if constexpr (requires {
                    alloc.select_on_container_copy_construction();
                }) {
                    return alloc.select_on_container_copy_construction();
                } else {
                    return alloc;
                }
            }
        };

        #undef ATTR_ELSE

    }

    /* An adaptor for std::allocator_traits that gives a slightly improved API. */
    template<typename Allocator>
    struct allocator_traits : public std::allocator_traits<Allocator> {
        #define TYPE_TO_VALUE(name) static constexpr bool name = std::allocator_traits<Allocator>::name::value

        TYPE_TO_VALUE(propagate_on_container_copy_assignment);
        TYPE_TO_VALUE(propagate_on_container_move_assignment);
        TYPE_TO_VALUE(propagate_on_container_swap);

        #undef TYPE_TO_VALUE

        static constexpr bool always_equal = std::allocator_traits<Allocator>::is_always_equal::value;

        [[nodiscard]]
        static constexpr bool are_equal(const Allocator &lhs, const Allocator &rhs) noexcept {
            if constexpr (always_equal) {
                return true;
            } else {
                return (lhs == rhs);
            }
        }
    };

    template<typename Allocator>
    concept allocator_type = requires {
        typename allocator_traits<Allocator>;
    };

    template<typename Allocator, typename Value>
    concept allocator_for = allocator_type<Allocator> && same_as<typename allocator_traits<Allocator>::value_type, Value>;

    template<typename Allocator, typename... Args>
    concept allocator_value_constructible_from = (
        allocator_type<Allocator> &&
        requires(Allocator &alloc, typename allocator_traits<Allocator>::pointer location, Args &&... args) {
            allocator_traits<Allocator>::construct(alloc, location, frd::forward<Args>(args)...);
        }
    );

    template<typename Allocator>
    concept allocator_value_default_constructible = allocator_value_constructible_from<Allocator>;

    template<typename Value>
    class allocator {
        public:
            using value_type = Value;
            using size_type  = frd::size_t;

            constexpr allocator() noexcept = default;
            constexpr allocator(const allocator &other) noexcept = default;

            template<typename OtherValue>
            constexpr allocator(const allocator<OtherValue> &other) {
                UNUSED(other);
            }

            constexpr ~allocator() = default;

            [[nodiscard]]
            constexpr Value *allocate(size_type n) const {
                /*
                    Sadly, the only reliable way to allocate uninitialized memory within
                    constant evaluation is with 'std::allocator'.
                */
                return std::allocator<Value>().allocate(n);
            }

            constexpr void deallocate(Value *ptr, size_type n) const noexcept {
                std::allocator<Value>().deallocate(ptr, n);
            }

            template<typename OtherValue>
            [[nodiscard]]
            constexpr bool operator ==(const allocator<OtherValue> &rhs) const noexcept {
                UNUSED(rhs);

                return true;
            }
    };

    template<typename T>
    [[nodiscard]]
    constexpr T *to_address(T *ptr) noexcept {
        static_assert(!function<T>);

        return ptr;
    }

    template<typename Ptr>
    concept _pointer_traits_to_address = (
        /* Check first that 'std::pointer_traits<Ptr>' is well-formed. */

        requires { typename Ptr::element_type; } ||
        !incomplete<first_template_arg<Ptr>>
    ) && (
        /* Next check that 'std::pointer_traits<Ptr>' has a 'to_address' static member function. */

        requires(const Ptr &ptr) {
            std::pointer_traits<Ptr>::to_address(ptr);
        }
    );

    template<typename Ptr>
    concept _arrow_dereferenceable_to_address = requires(const Ptr &ptr) {
        ptr.operator ->();
    };

    template<typename Ptr>
    requires (_pointer_traits_to_address<Ptr> || _arrow_dereferenceable_to_address<Ptr>)
    [[nodiscard]]
    constexpr auto to_address(const Ptr &ptr) noexcept {
        if constexpr (_pointer_traits_to_address<Ptr>) {
            return std::pointer_traits<Ptr>::to_address(ptr);
        } else {
            return frd::to_address(ptr.operator ->());
        }
    }

    template<typename Ptr>
    concept convertible_to_address = requires (const Ptr &ptr) {
        ::frd::to_address(ptr);
    };

    template<typename T>
    union uninitialized {
        T elem;

        /* Cannot be defaulted as then it will try to default-construct 'elem'. */
        constexpr uninitialized() noexcept { }

        /* Cannot be defaulted as then it will try to destroy the possibly uninitialized 'elem'. */
        constexpr ~uninitialized() noexcept { }
    };

}
