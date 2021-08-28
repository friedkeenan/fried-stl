#pragma once

#include <memory>

#include <frd/defines.hpp>
#include <frd/arithmetic.hpp>
#include <frd/limits.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename T>
    constexpr void destroy_at(T *location) noexcept(noexcept(location->~T())) {
        location->~T();
    }

    namespace impl {
        /*
            Implementations of unspecialized 'std::pointer_traits' and 'std::allocator_traits'.

            It would not be correct to use these implementations for
            allocator-aware types, as the aforementioned templates can be
            specialized (though you should not need to do so), but it's
            fun to implement I guess.
        */

        template<typename T, typename U>
        concept _has_rebind = requires {
            type_holder<typename T::rebind<U>>{};
        };

        template<typename T, typename U>
        struct _rebind : type_holder<replace_first_template_arg<T, U>> { };

        template<typename T, typename U>
        requires (_has_rebind<T, U>)
        struct _rebind<T, U> : type_holder<typename T::rebind<U>> { };

        template<typename T, typename U>
        concept _has_rebind_other = requires {
            type_holder<typename T::rebind<U>::other>{};
        };

        template<typename T, typename U>
        struct _rebind_other : type_holder<replace_first_template_arg<T, U>> { };

        template<typename T, typename U>
        requires (_has_rebind_other<T, U>)
        struct _rebind_other<T, U> : type_holder<typename T::rebind<U>::other> { };

        #define ATTR_ELSE(cls, attr, default_type)                 \
            template<typename T> using _##attr = typename T::attr; \
            using attr = detected_else<default_type, _##attr, cls>

        template<typename Ptr>
        struct pointer_traits {
            using pointer = Ptr;

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
            using value_type     = Allocator::value_type;

            ATTR_ELSE(Allocator, pointer,                                 value_type *);
            ATTR_ELSE(Allocator, const_pointer,                           typename pointer_traits<pointer>::rebind<const value_type>);
            ATTR_ELSE(Allocator, void_pointer,                            typename pointer_traits<pointer>::rebind<void>);
            ATTR_ELSE(Allocator, const_void_pointer,                      typename pointer_traits<pointer>::rebind<const void>);
            ATTR_ELSE(Allocator, difference_type,                         typename pointer_traits<pointer>::difference_type);
            ATTR_ELSE(Allocator, size_type,                               make_unsigned<difference_type>);
            ATTR_ELSE(Allocator, _propagate_on_container_copy_assignment, false_holder);
            ATTR_ELSE(Allocator, _propagate_on_container_move_assignment, false_holder);
            ATTR_ELSE(Allocator, _propagate_on_container_swap,            false_holder);
            ATTR_ELSE(Allocator, _is_always_equal,                        constant_holder<empty<Allocator>>);

            #define TYPE_TO_VALUE(name) static constexpr bool name = _##name::value

            TYPE_TO_VALUE(propagate_on_container_copy_assignment);
            TYPE_TO_VALUE(propagate_on_container_move_assignment);
            TYPE_TO_VALUE(propagate_on_container_swap);
            TYPE_TO_VALUE(is_always_equal);

            #undef TYPE_TO_VALUE

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
                    alloc.construct(location, forward<Args>(args)...);
                }) {
                    alloc.construct(location, forward<Args>(args)...);
                } else {
                    std::construct_at(location, forward<Args>(args)...);
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
        TYPE_TO_VALUE(is_always_equal);

        #undef TYPE_TO_VALUE
    };

    template<typename T>
    class allocator {
        public:
            using value_type = T;
            using size_type  = size_t;

            union UnitializedElement {
                T elem;
            };

            constexpr allocator() noexcept = default;
            constexpr allocator(const allocator &other) noexcept = default;

            template<typename U>
            constexpr allocator(const allocator<U> &other) { }

            constexpr ~allocator() = default;

            [[nodiscard]]
            constexpr T *allocate(size_type n) const {
                /*
                    I've checked as carefully as I can, and I don't believe this to be UB.

                    Clang will report that you can't cast from 'void *' in constexpr, but
                    GCC gobbles it up just fine, and if you implement it without constexpr
                    then Clang works just fine with this.
                */
                return static_cast<T *>(static_cast<void *>(new UnitializedElement[n]));
            }

            constexpr void deallocate(T *ptr, size_type n) const noexcept {
                UNUSED(n);

                /*
                    I believe the static_cast is unnecessary but am
                    including it to remind of how 'ptr' was created.
                */
                delete[] static_cast<UnitializedElement *>(static_cast<void *>(ptr));
            }

            template<typename U>
            constexpr bool operator ==(const allocator<U> &rhs) const noexcept {
                return true;
            }
    };

    template<typename T>
    constexpr T *to_address(T *ptr) noexcept {
        static_assert(!function<T>);

        return ptr;
    }

    template<typename Ptr>
    constexpr auto to_address(const Ptr &ptr) noexcept {
        if constexpr (requires {
            std::pointer_traits<Ptr>::to_address(ptr);
        }) {
            return std::pointer_traits<Ptr>::to_address(ptr);
        } else {
            return to_address(ptr.operator ->());
        }
    }

}
