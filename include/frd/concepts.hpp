#pragma once

#include <frd/bits/concepts_base.hpp>

#include <frd/utility.hpp>

namespace frd {

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
