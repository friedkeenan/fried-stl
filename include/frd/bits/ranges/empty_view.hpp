#pragma once

#include <frd/bits/ranges/view_base.hpp>

#include <frd/arithmetic.hpp>

namespace frd {

    template<object Element>
    class empty_view : public view_interface<empty_view<Element>> {
        public:
            static constexpr Element *begin() noexcept {
                return nullptr;
            }

            static constexpr Element *end() noexcept {
                return nullptr;
            }

            static constexpr Element *data() noexcept {
                return nullptr;
            }

            static constexpr frd::size_t size() noexcept {
                return 0;
            }

            static constexpr bool empty() noexcept {
                return true;
            }
    };

    namespace views {

        template<object Element>
        constexpr inline empty_view<Element> empty{};

    }

}

namespace std::ranges {

    template<typename Element>
    constexpr inline bool enable_borrowed_range<frd::empty_view<Element>> = true;

}
