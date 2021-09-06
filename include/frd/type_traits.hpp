#pragma once

#include <frd/bits/type_traits_base.hpp>
#include <frd/bits/concepts_base.hpp>

namespace frd {

    template<typename T>
    struct _decay {
        using U = remove_reference<T>;

        using type = conditional<
            array_type<U>,
            remove_extent<U> *,

            conditional<
                function<U>,
                U *,

                remove_cv<U>
            >
        >;
    };

    template<typename T>
    using decay = typename _decay<T>::type;

}
