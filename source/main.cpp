#include <cstdio>

#include <frd/defines.hpp>
#include <frd/arithmetic.hpp>
#include <frd/bit.hpp>
#include <frd/tuple.hpp>
#include <frd/memory.hpp>
#include <frd/algorithm.hpp>
#include <frd/interval.hpp>
#include <frd/vector.hpp>

static_assert(frd::tuple{1, 2} == frd::tuple{1, 2});

static_assert(sizeof(frd::size_t)    == 8);
static_assert(sizeof(frd::int8_t)    == 1);
static_assert(sizeof(frd::int16_t)   == 2);
static_assert(sizeof(frd::int32_t)   == 4);
static_assert(sizeof(frd::int64_t)   == 8);
static_assert(sizeof(frd::ptrdiff_t) == 8);

static_assert(frd::same_as<frd::ptrdiff_t, frd::int64_t>);

static_assert(frd::same_as<frd::uintptr_t, frd::uint64_t>);

static_assert(frd::signed_integral<char>);

static_assert(frd::same_as<frd::unsafe::allocator_traits<frd::allocator<int>>::rebind_alloc<char>::value_type, char>);

static_assert(frd::same_as<frd::remove_signedness<const volatile unsigned int>, const volatile int>);

static_assert(frd::same_as<frd::make_signed<unsigned int>, signed int>);
static_assert(frd::same_as<frd::make_unsigned<const volatile int>, const volatile unsigned int>);

static_assert(frd::min(1, 2, 3, 0, 4, 4) == 0);

static_assert(frd::range<frd::vector<int>>);

static_assert(frd::same_as<frd::remove_cvref<const int &>, int>);

static_assert(frd::range<frd::interval<int>>);

consteval bool fuck() {
    struct S {
        bool value;

        constexpr S(bool value) : value(value) { }
    };

    auto v = frd::vector<S>();
    v.reserve(5);

    v.emplace_back(true);
    v.emplace_back(false);
    v.emplace_back(true);

    v.pop_back();

    v.resize(5, S(false));
    v.clear();

    return v.size() == 0 && v.capacity() == 5;
}

static_assert(fuck());

static_assert(frd::same_as<typename frd::unique_ptr<int>::element_type, int>);

int main(int argc, char **argv) {
    FRD_UNUSED(argc, argv);

    const auto fuck = frd::tuple{1, 2};

    const auto &[x, y] = fuck;
    FRD_UNUSED(x, y);

    for (const auto i : fuck) {
        std::printf("%d\n", i);
    }

    return 0;
}
