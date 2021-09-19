#pragma once

#include <frd/bits/concepts_base.hpp>

#include <frd/utility.hpp>
#include <frd/type_traits.hpp>

namespace frd {

    template<typename From, typename To>
    concept convertible_to_non_slicing = (
        convertible_to<From, To>                           &&
        (
            (!pointer<decay<From>> || !pointer<decay<To>>) ||
            same_as_without_cv<
                remove_pointer<decay<From>>,
                remove_pointer<decay<From>>
            >
        )
    );

    template<typename T>
    concept movable = (
        object<T>               &&
        move_constructible<T>   &&
        assignable_from<T &, T> &&
        swappable<T>
    );

    template<typename T>
    concept copyable = (
        copy_constructible<T>           &&
        movable<T>                      &&
        assignable_from<T &, T &>       &&
        assignable_from<T &, const T &> &&
        assignable_from<T &, const T>
    );

    template<typename T>
    concept semiregular = copyable<T> && default_constructible<T>;

    template<typename T>
    concept regular = semiregular<T> && equality_comparable<T>;

}
