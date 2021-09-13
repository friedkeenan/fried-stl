#include <cstdio>

#include <frd/defines.hpp>
#include <frd/arithmetic.hpp>
#include <frd/bit.hpp>
#include <frd/tuple.hpp>
#include <frd/memory.hpp>
#include <frd/algorithm.hpp>
#include <frd/array.hpp>
#include <frd/vector.hpp>

using namespace frd::arithmetic_literals;

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
static_assert(frd::same_as<decltype(frd::interval(0_sz, 5)), frd::interval<frd::size_t, int>>);

static_assert(frd::range<frd::array<int, 2>>);
static_assert(frd::array{1, 2} == frd::array{1, 2});
static_assert(frd::nothrow_swappable<frd::array<int, 2>>);
static_assert(frd::to_array<char>({1, 2, 3}) == frd::array<char, 3>{1, 2, 3});

static_assert(frd::size(frd::interval(5)) == 5);

consteval bool fuck() {
    struct S {
        bool value = false;

        S() = delete;
        constexpr S(bool value) : value(value) { }
    };

    auto v = frd::vector<S>();
    v.emplace(v.end(), true);

    v.emplace_back(true);
    v.emplace_back(false);
    v.emplace_back(true);

    v.pop_back();

    v.resize(5, S(false));
    v.clear();

    v.emplace(v.begin(), true);

    return v[0].value && v.capacity() == 5;
}

static_assert(fuck());

static_assert(frd::same_as<typename frd::unique_ptr<int>::element_type, int>);

int main(int argc, char **argv) {
    FRD_UNUSED(argc, argv);

    const auto fuck = frd::array{1, 2};

    const auto &[x, y] = fuck;
    std::printf("%d %d\n", x, y);

    for (const auto i : frd::interval(5) | frd::views::iterators) {
        std::printf("%d\n", *i);
    }

    return 0;
}
