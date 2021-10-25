#pragma once

#include <frd/bits/ranges/iterators.hpp>
#include <frd/bits/ranges/access.hpp>
#include <frd/bits/memory_base.hpp>

#include <frd/utility.hpp>
#include <frd/concepts.hpp>

namespace frd {

    /* Can perform storage optimizations over 'optional<range_iterator<R>>'. */
    template<typename R>
    struct cached_iterator {
        using _iterator = range_iterator<R>;

        uninitialized<_iterator> _it_holder = {};
        bool _initialized = false;

        constexpr void _destruct() {
            if (this->_initialized) {
                this->_it_holder.elem.~_iterator();
            }
        }

        constexpr void _destruct() noexcept requires (trivially_destructible<_iterator>) { }

        constexpr cached_iterator() noexcept = default;

        constexpr ~cached_iterator() {
            this->_destruct();
        }

        constexpr ~cached_iterator() noexcept requires (trivially_destructible<_iterator>) = default;

        constexpr cached_iterator(const cached_iterator &other)
        noexcept(
            nothrow_constructible_from<_iterator, const _iterator &>
        )
        requires (
            copy_constructible<_iterator>
        ) : _initialized(other._initialized) {
            if (other._initialized) {
                this->_it_holder.elem = other._it_holder.elem;
            }
        }

        constexpr cached_iterator &operator =(const cached_iterator &rhs)
        requires (
            copy_constructible<_iterator>
        ) {
            FRD_CHECK_SELF(rhs);

            this->_destruct();

            this->_initialized = rhs._initialized;

            if (rhs._initialized) {
                this->_it_holder.elem = rhs._it_holder.elem;
            }

            return *this;
        }

        constexpr cached_iterator(cached_iterator &&other)
        noexcept(
            nothrow_constructible_from<_iterator, _iterator>
        )
        requires(
            move_constructible<_iterator>
        ) : _initialized(frd::exchange(other._initialized, false)) {
            if (other._initialized) {
                this->_it_holder.elem = frd::move(other._it_holder.elem);
            }
        }

        constexpr cached_iterator &operator =(cached_iterator &&rhs)
        requires (
            move_constructible<_iterator>
        ) {
            FRD_CHECK_SELF(rhs);

            this->_destruct();

            this->_initialized = frd::exchange(rhs._initialized, false);

            if (this->_initialized) {
                this->_it_holder.elem = frd::move(rhs._it_holder.elem);
            }
        }

        [[nodiscard]]
        constexpr bool has_value() const noexcept {
            return this->_initialized;
        }

        template<forwarder_for<R> RFwd>
        [[nodiscard]]
        constexpr _iterator get(RFwd &&r) const {
            frd::precondition(this->has_value(), "cached_iterator doesn't have a value!");

            frd::discard(r);

            return this->_it_holder.elem;
        }

        template<forwarder_for<R> RFwd, forwarder_for<_iterator> ItFwd>
        constexpr void set(RFwd &&r, ItFwd &&it) {
            /* TODO: Assert that we have no value? */

            frd::discard(r);

            this->_it_holder.elem = frd::forward<ItFwd>(it);
            this->_initialized    = true;
        }
    };

    template<typename R>
    requires (
        random_access_range<R>                                  &&
        sizeof(range_difference<R>) < sizeof(range_iterator<R>)
    )
    struct cached_iterator<R> {
        using _iterator   = range_iterator<R>;
        using _difference = range_difference<R>;

        _difference _offset = -1;

        constexpr cached_iterator() noexcept = default;
        constexpr ~cached_iterator() = default;

        constexpr explicit cached_iterator(const _difference offset) noexcept : _offset(offset) { }

        constexpr cached_iterator(const cached_iterator &) noexcept = default;

        constexpr cached_iterator &operator =(const cached_iterator &) noexcept = default;

        constexpr cached_iterator(cached_iterator &&other) noexcept : _offset(frd::exchange(other._offset, -1)) { }

        constexpr cached_iterator &operator =(cached_iterator &&rhs) noexcept {
            FRD_CHECK_SELF(rhs);

            this->_offset = frd::exchange(rhs._offset, -1);

            return *this;
        }

        [[nodiscard]]
        constexpr bool has_value() const noexcept {
            return this->_offset >= 0;
        }

        template<forwarder_for<R> RFwd>
        [[nodiscard]]
        constexpr _iterator get(RFwd &&r) const {
            frd::precondition(this->has_value(), "cached_iterator doesn't have a value!");

            return frd::begin(frd::forward<RFwd>(r)) + this->_offset;
        }


        template<forwarder_for<R> RFwd, forwarder_for<_iterator> ItFwd>
        constexpr void set(RFwd &&r, ItFwd &&it) {
            /* TODO: Assert that we have no value? */

            this->_offset = frd::forward<ItFwd>(it) - frd::begin(frd::forward<RFwd>(r));
        }
    };

}
