#include <cstdio>

#include <frd/defines.hpp>
#include <frd/arithmetic.hpp>
#include <frd/bit.hpp>
#include <frd/tuple.hpp>
#include <frd/memory.hpp>

static_assert(frd::tuple{1, 2} == frd::tuple{1, 2});

static_assert(sizeof(frd::size_t)    == 8);
static_assert(sizeof(frd::int8_t)    == 1);
static_assert(sizeof(frd::int16_t)   == 2);
static_assert(sizeof(frd::int32_t)   == 4);
static_assert(sizeof(frd::int64_t)   == 8);
static_assert(sizeof(frd::ptrdiff_t) == 8);

static_assert(frd::same_as<frd::ptrdiff_t, frd::int64_t>);

static_assert(frd::same_as<frd::uintptr_t, frd::uint64_t>);

static_assert(frd::unsigned_integral<frd::uint8_t>);

static_assert(frd::same_as<frd::impl::pointer_traits<frd::allocator<int>>::rebind<char>::value_type, char>);

static_assert(frd::same_as<frd::remove_signedness<const volatile int>, const volatile int>);

using shit = frd::impl::allocator_traits<frd::allocator<int>>;

consteval auto fuck() {
    const auto alloc = frd::allocator<int>{};

    const auto data = alloc.allocate(1);
    *data = 1;

    alloc.deallocate(data, 1);

    return true;
}

static_assert(fuck());

int main(int argc, char **argv) {
    UNUSED(argc, argv);

    const auto fuck = frd::tuple{1, 2};

    const auto &[x, y] = fuck;
    UNUSED(x, y);

    for (const auto &i : fuck) {
        std::printf("%d\n", i);
    }

    return 0;
}
