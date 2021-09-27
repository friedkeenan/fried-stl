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

static_assert(frd::interval(5)[0] == 0);

static_assert(frd::contiguous_range<frd::vector<int>>);
static_assert(frd::view<frd::interval<int>>);

static_assert(frd::sign(1) > 0);

static_assert(std::tuple_size<frd::tuple<int, int>>::value == 2);
static_assert(frd::same_as<frd::tuple_element<1, frd::tuple<int, int>>, int>);

static_assert(frd::common_range<frd::interval<int>>);

static_assert(frd::_adl::_adl_swap<frd::unique_ptr<int> &, frd::unique_ptr<int> &>);
static_assert(frd::nothrow_swappable<frd::unique_ptr<int> &>);

static_assert(!frd::contiguous_iterator<frd::reverse_iterator<int *>>);

consteval bool fuck() {
    struct S {
        bool value = false;

        S() = delete;
        constexpr S(bool value) : value(value) { }
    };

    auto v = frd::vector<S>();
    v.insert(v.end(), {S(true)});
    v.emplace(v.end(), true);

    v.emplace_back(true);
    v.emplace_back(false);
    v.emplace_back(true);

    v.pop_back();

    v.resize(5, S(false));
    v.clear();

    v.emplace(v.begin(), false);
    v.emplace(v.begin(), true);
    v.insert(v.end(), S(true));

    v.insert(v.end(), frd::array{S(false), S(true)});
    v.insert(v.end(), {S(false), S(true)});

    return v[0].value && !v[1].value && v[2].value && v.back().value && v.capacity() == 8;
}

static_assert(fuck());

static_assert(frd::same_as<typename frd::unique_ptr<int>::element_type, int>);

struct IntSentinel {
    constexpr bool operator ==(const int rhs) const noexcept {
        return rhs == 5;
    }
};

static_assert(frd::subrange(frd::interval(0, IntSentinel{}), 5).size() == 5);

consteval bool shit() {
    bool success = true;

    int compare = 4;
    for (const auto i : frd::interval(0, IntSentinel{}) | frd::views::reverse | frd::views::iterators) {
        success = (success && (*i == compare));

        compare--;
    }

    return success;
}

static_assert(shit());

consteval bool cuck() {
    const auto storage = frd::bit_storage(5);

    return storage.unpack<int>() == 5;
}

static_assert(cuck());

int main(int argc, char **argv) {
    FRD_UNUSED(argc, argv);

    constexpr auto fuck = frd::array{1, 2};

    std::printf("%d\n", frd::get<1>(fuck));

    const auto &[x, y] = fuck;
    std::printf("%d %d\n", x, y);

    static_assert(frd::common_range<decltype(frd::subrange(fuck))>);
    static_assert(frd::common_range<decltype(frd::subrange(fuck) | frd::views::iterators)>);

    static_assert(frd::reverse_iterator(fuck.end())[0] == 2);

    static_assert(frd::subrange(fuck).size() == 2);

    static_assert(frd::subrange(fuck.begin(), fuck.end()).size() == 2);

    for (const auto i : frd::interval(5) | frd::views::reverse | frd::views::iterators) {
        std::printf("%d\n", *i);
    }

    return 0;
}
