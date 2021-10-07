#pragma once

#include <cstddef>
#include <memory>

#include <frd/arithmetic.hpp>
#include <frd/memory.hpp>
#include <frd/utility.hpp>
#include <frd/array.hpp>
#include <frd/ranges.hpp>
#include <frd/type_traits.hpp>
#include <frd/concepts.hpp>

namespace frd {

    template<typename Element, frd::size_t Extent>
    class span;

    template<typename T>
    constexpr inline bool _span_specialization = false;

    template<typename Element, frd::size_t Extent>
    constexpr inline bool _span_specialization<span<Element, Extent>> = true;

    template<typename T>
    concept span_specialization = _span_specialization<remove_cvref<T>>;

    template<typename Element, frd::size_t Extent = dynamic_extent>
    class span : public view_tag {
        public:
            using element_type     = Element;
            using value_type       = remove_cv<Element>;
            using size_type        = frd::size_t;
            using difference_type  = frd::ptrdiff_t;
            using pointer          = Element *;
            using const_pointer    = const Element *;
            using reference        = Element &;
            using const_reference  = const Element &;
            using iterator         = Element *;
            using reverse_iterator = frd::reverse_iterator<iterator>;

            static constexpr auto extent = Extent;

            static constexpr bool HasDynamicExtent = (Extent == dynamic_extent);

            pointer _data = nullptr;

            [[no_unique_address]] maybe_present<!HasDynamicExtent, size_type> _size = {};

            constexpr span() noexcept requires (Extent == 0 || HasDynamicExtent) = default;

            template<contiguous_iterator It>
            requires (qualification_convertible_to<iter_reference<It>, Element>)
            explicit(!HasDynamicExtent)
            constexpr span(It it, const size_type count) noexcept : _data(frd::to_address(it)), _size(count) {
                if constexpr (!HasDynamicExtent) {
                    FRD_ASSERT(count == Extent, "Mismatched size for span with static extent!");
                }
            }

            template<contiguous_iterator It, sized_sentinel_for<It> S>
            requires (qualification_convertible_to<iter_reference<It>, Element> && !convertible_to<S, size_type>)
            explicit(!HasDynamicExtent)
            constexpr span(It it, S bound) noexcept : _data(frd::to_address(it)), _size(bound - it) {
                if constexpr (!HasDynamicExtent) {
                    FRD_ASSERT(bound - it == Extent, "Mismatched size for span with static extent!");
                }
            }

            template<qualification_convertible_to<Element> ElementOther, frd::size_t N>
            requires (HasDynamicExtent || N == Extent)
            constexpr span(ElementOther (&arr)[N]) noexcept : _data(frd::data(arr)), _size(N) { }

            template<qualification_convertible_to<Element> ElementOther, frd::size_t N>
            requires (HasDynamicExtent || N == Extent)
            constexpr span(frd::array<ElementOther, N> &arr) noexcept : _data(arr.data()), _size(N) { }

            template<typename ElementOther, frd::size_t N>
            requires (qualification_convertible_to<const ElementOther, Element> && (HasDynamicExtent || N == Extent))
            constexpr span(const frd::array<ElementOther, N> &arr) noexcept : _data(arr.data()), _size(N) { }

            template<typename R>
            requires (
                contiguous_range<R>                                       &&
                sized_range<R>                                            &&
                (borrowed_range<R> || const_type<Element>)                &&
                !span_specialization<R>                                   &&
                !array_specialization<R>                                  &&
                qualification_convertible_to<range_reference<R>, Element>
            )
            explicit(!HasDynamicExtent)
            constexpr span(R &&r)
            noexcept(
                noexcept(frd::data(r)) &&
                noexcept(frd::size(r))
            ) : _data(frd::data(r)), _size(frd::size(r)) {
                if constexpr (!HasDynamicExtent) {
                    FRD_ASSERT(frd::size(r) == Extent, "Mismatched size for span with static extent!");
                }
            }

            template<typename ElementOther>
            requires (qualification_convertible_to<const ElementOther, Element>)
            explicit(!HasDynamicExtent)
            constexpr span(const std::initializer_list<ElementOther> list) noexcept : _data(frd::data(list)), _size(frd::size(list)) {
                if constexpr (!HasDynamicExtent) {
                    FRD_ASSERT(frd::size(list) == Extent, "Mismatched size for span with static extent!");
                }
            }

            template<qualification_convertible_to<Element> ElementOther, size_type ExtentOther>
            requires (
                HasDynamicExtent              ||
                ExtentOther == dynamic_extent ||
                ExtentOther == Extent
            )
            explicit(!HasDynamicExtent && ExtentOther == dynamic_extent)
            constexpr span(const span<ElementOther, ExtentOther> &source) noexcept : _data(source.data()), _size(source.size()) {
                if constexpr (!HasDynamicExtent) {
                    FRD_ASSERT(source.size() == Extent, "Mismatched size for span with static extent!");
                }
            }

            constexpr span(const span &other) noexcept = default;

            constexpr span &operator =(const span &rhs) noexcept = default;

            template<frd::size_t Count>
            requires (Count <= Extent)
            constexpr span<Element, Count> first() const noexcept {
                if constexpr (HasDynamicExtent) {
                    FRD_ASSERT(Count <= this->size(), "Invalid subspan size!");
                }

                return span<Element, Count>(this->_data, this->size() - Count);
            }

            constexpr span<Element> first(const size_type count) const noexcept {
                FRD_ASSERT(count <= this->size(), "Invalid subspan size!");

                return span(this->_data, count);
            }

            template<frd::size_t Count>
            requires (Count <= Extent)
            constexpr span<Element, Count> last() const noexcept {
                if constexpr (HasDynamicExtent) {
                    FRD_ASSERT(Count <= this->size(), "Invalid subspan size!");
                }

                return span(this->_data + this->size() - Count, Count);
            }

            constexpr span<Element> last(const size_type count) const noexcept {
                FRD_ASSERT(count <= this->size(), "Invalid subspan size!");

                return span(this->_data + this->size() - count, count);
            }

            template<
                frd::size_t Offset,
                frd::size_t Count = dynamic_extent,

                frd::size_t SubExtent = []() {
                    if constexpr (Count != dynamic_extent) {
                        return Count;
                    } else if constexpr (!HasDynamicExtent) {
                        return Extent - Offset;
                    } else {
                        return dynamic_extent;
                    }
                }()
            >
            requires (Offset <= Extent && (Count == dynamic_extent || Count <= Extent - Offset))
            constexpr span<Element, SubExtent> subspan() const noexcept {
                if constexpr (HasDynamicExtent) {
                    FRD_ASSERT(Offset <= this->size() && Count <= this->size() - Offset, "Invalid subspan size!");
                }

                return span<Element, SubExtent>(
                    this->_data + Offset,

                    [this]() {
                        if constexpr (HasDynamicExtent) {
                            return this->size() - Count;
                        } else {
                            return Count;
                        }
                    }()
                );
            }

            constexpr span<Element> subspan(const size_type offset, const size_type count) const noexcept {
                FRD_ASSERT(offset <= this->size(), count <= this->size() - offset, "Invalid subspan size!");

                return span(this->_data + offset, count);
            }

            constexpr pointer data() const noexcept {
                return this->_data;
            }

            constexpr size_type size() const noexcept {
                if constexpr (!HasDynamicExtent) {
                    return this->_size;
                } else {
                    return Extent;
                }
            }

            constexpr size_type size_bytes() const noexcept {
                return this->size() * sizeof(Element);
            }

            constexpr bool empty() const noexcept {
                return this->size() == 0;
            }

            constexpr reference front() const noexcept {
                return *this->_data;
            }

            constexpr reference back() const noexcept {
                return *(this->_data + this->size());
            }

            constexpr iterator begin() const noexcept {
                return this->_data;
            }

            constexpr reverse_iterator rbegin() const noexcept {
                return reverse_iterator(this->end());
            }

            constexpr iterator end() const noexcept {
                return this->_data + this->size();
            }

            constexpr reverse_iterator rend() const noexcept {
                return reverse_iterator(this->begin());
            }

            constexpr reference operator [](const frd::size_t index) const noexcept {
                FRD_ASSERT(index < this->size(), "Invalid index for span!");

                return this->_data[index];
            }
    };

    template<typename It, typename EndOrSize>
    span(It, EndOrSize) -> span<remove_reference<iter_reference<It>>>;

    template<typename T, frd::size_t N>
    span(T (&)[N]) -> span<T, N>;

    template<typename T, frd::size_t N>
    span(frd::array<T, N> &) -> span<T, N>;

    template<typename T, frd::size_t N>
    span(const frd::array<T, N> &) -> span<const T, N>;

    template<typename R>
    span(R &&) -> span<remove_reference<range_reference<R>>>;

    template<typename T>
    span(std::initializer_list<T>) -> span<const T>;

    template<typename R>
    concept span_convertible = !span_specialization<R> && requires(R &&r) {
        ::frd::span(frd::forward<R>(r));
    };

    template<typename T, frd::size_t N>
    constexpr span<const T, N> as_const_span(const span<T, N> s) noexcept {
        return s;
    }

    template<span_convertible R>
    constexpr auto as_const_span(R &&r) noexcept(noexcept(span(frd::forward<R>(r)))) {
        return frd::as_const_span(span(frd::forward<R>(r)));
    }

    /* TODO: Is it worth it to have versions for 'frd::strict_byte'? */

    template<
        typename    T,
        frd::size_t N,

        frd::size_t ByteExtent = []() {
            if constexpr (N == dynamic_extent) {
                return dynamic_extent;
            } else {
                return N * sizeof(T);
            }
        }()
    >
    span<const std::byte, ByteExtent> as_bytes(const span<T, N> s) noexcept {
        return span<const std::byte, ByteExtent>(reinterpret_cast<const std::byte *>(s.data()), s.size_bytes());
    }

    template<
        typename    T,
        frd::size_t N,

        frd::size_t ByteExtent = []() {
            if constexpr (N == dynamic_extent) {
                return dynamic_extent;
            } else {
                return N * sizeof(T);
            }
        }()
    >
    requires (!const_type<T>)
    span<std::byte, ByteExtent> as_writable_bytes(const span<T, N> s) noexcept {
        return span<std::byte, ByteExtent>(reinterpret_cast<std::byte *>(s.data()), s.size_bytes());
    }

    template<span_convertible R>
    auto as_bytes(R &&r) noexcept(noexcept(as_bytes(span(frd::forward<R>(r))))) {
        return frd::as_bytes(span(frd::forward<R>(r)));
    }

    template<span_convertible R>
    auto as_writable_bytes(R &&r) noexcept(noexcept(as_writable_bytes(span(frd::forward<R>(r))))) {
        return frd::as_writable_bytes(span(frd::forward<R>(r)));
    }

    template<typename Object>
    frd::array<std::byte, sizeof(Object)> underlying_data_copy(Object &&obj) noexcept {
        frd::array<std::byte, sizeof(Object)> raw_data;

        frd::copy(frd::as_bytes(span<Object, 1>(std::addressof(obj))), raw_data.begin());

        return raw_data;
    }

    template<typename Object>
    auto underlying_data(Object &obj) noexcept {
        return frd::as_bytes(span<const Object, 1>(std::addressof(obj), 1));
    }

    template<typename Object>
    auto underlying_data(Object &&) = delete;

    template<typename Object>
    requires (!const_type<Object>)
    auto writable_underlying_data(Object &obj) noexcept {
        return frd::as_writable_bytes(span<const Object, 1>(std::addressof(obj), 1));
    }

    template<typename Object>
    requires (!const_type<Object>)
    auto writable_underlying_data(Object &&) = delete;

}

namespace std::ranges {

    template<typename Element, frd::size_t Extent>
    constexpr inline bool enable_borrowed_range<frd::span<Element, Extent>> = true;

}
