#include <cstdio>

#include <frd/defines.hpp>
#include <frd/arithmetic.hpp>
#include <frd/bit.hpp>
#include <frd/tuple.hpp>
#include <frd/memory.hpp>
#include <frd/algorithm.hpp>
#include <frd/array.hpp>
#include <frd/span.hpp>
#include <frd/vector.hpp>

using namespace frd::arithmetic_literals;

static_assert(frd::tuple{1, 2} == frd::tuple{1, 2});
static_assert(frd::pair{3, 4} == frd::pair{3, 4});
static_assert(frd::pair{5, 6} == frd::tuple{5, 6});

static_assert(frd::pair{7, 8}.first == frd::pair{8, 7}.second);

static_assert(sizeof(frd::size_t)    == 8);
static_assert(sizeof(frd::int8_t)    == 1);
static_assert(sizeof(frd::int16_t)   == 2);
static_assert(sizeof(frd::int32_t)   == 4);
static_assert(sizeof(frd::int64_t)   == 8);
static_assert(sizeof(frd::int128_t)  == 16);
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
static_assert(frd::contiguous_range<std::initializer_list<int>>);

FRD_PLATFORM_USES_EXTENSION static_assert(frd::integral<unsigned __int128>);

static_assert(sizeof(frd::int_for_bit_size<128>) == 16);

static_assert(frd::tuple_like<frd::tuple<int>>);
static_assert(frd::pair_like<frd::subrange<int *>>);
static_assert(frd::pair_like<frd::pair<int, int>>);

static_assert(frd::view<frd::span<int, 2>>);
static_assert(frd::same_as<frd::views::all_t<frd::span<int, 2>>, frd::span<int, 2>>);

static_assert(frd::decrementable<int>);

static_assert(
    frd::apply(
        frd::overloaded{
            [](const int                 ) { return 6;   },
            [](const double              ) { return 9;   },
            [](const frd::unique_ptr<int>) { return 420; },
        },

        frd::tuple{frd::make_unique<int>(5)}
    ) == 420
);

static_assert(frd::qualification_convertible_to<const int, const volatile int>);

static_assert(frd::contiguous_iterator<frd::counted_iterator<int *>>);
static_assert(frd::random_access_iterator<frd::counted_iterator<frd::reverse_iterator<int *>>>);

static_assert(frd::same_as<frd::make_index_sequence<5>, frd::index_sequence<0, 1, 2, 3, 4>>);

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

    const auto it = v.insert(v.end(), {S(false), S(true)}) + 1;
    it->value = false;

    v.insert(v.end(), 500, S(false));

    const auto erase_ret = v.erase(v.begin() + 1, v.end() - 1);

    return erase_ret == v.begin() + 1 && v[0].value && !v[1].value && v.size() == 2 && v.capacity() == 507;
}

static_assert(fuck());

struct IntSentinel {
    constexpr bool operator ==(const int rhs) const noexcept {
        return rhs == 5;
    }
};

static_assert(frd::subrange(frd::interval(0, IntSentinel{}), 5).size() == 5);

consteval bool shit() {
    bool success = true;

    int compare = 4;
    for (const auto i : frd::interval(0, IntSentinel{}) | frd::views::reverse | frd::views::iterators | frd::views::cycle<>(3) | frd::views::cycle<>(2)) {
        success = (success && (*i == compare));

        /*
            Add 4 instead of subtracting 1 to get a positive 'compare' because
            C++'s '%' operator doesn't only return positive numbers.
        */
        compare = (compare + 4) % 5;
    }

    return success;
}

static_assert(shit());

consteval bool cuck() {
    const auto storage = frd::bit_storage(5);

    return storage.unpack<int>() == 5;
}

static_assert(cuck());

static_assert(frd::vector<int>() == frd::vector<int>());

int main(int argc, char **argv) {
    frd::discard(argc, argv);

    static constexpr auto fuck = frd::array{1, 2};

    std::printf("%d\n", frd::get<1>(fuck));

    const auto [x, y] = fuck;
    std::printf("%d %d\n", x, y);

    static_assert(frd::common_range<decltype(frd::subrange(fuck))>);
    static_assert(frd::common_range<decltype(frd::subrange(fuck) | frd::views::iterators)>);

    static_assert(frd::reverse_iterator(fuck.end())[0] == 2);

    static_assert(frd::subrange(fuck).size() == 2);

    static_assert(frd::subrange(fuck.begin(), fuck.end()).size() == 2);

    constexpr auto s = frd::as_const_span(fuck);
    static_assert(frd::same_as<decltype(s), const frd::span<const int, 2>>);

    static_assert(frd::same_as<decltype(s | frd::views::cycle<3> | frd::views::cycle<2>), frd::cycle_view<frd::span<const int, 2>, 6>>);

    constexpr auto rng = s | frd::views::reverse | frd::views::cycle<3> | frd::views::cycle<2>;

    for (const auto [i, num] : frd::views::counted(rng.begin(), 5) | frd::views::enumerate) {
        std::printf("repeat[%ld]: %d\n", i, num);
    }

    constexpr auto zip = frd::views::zip(frd::interval(0, frd::unreachable_sentinel), frd::interval(3, 6) | frd::views::cycle<2>);

    for (const auto [a, b] : zip) {
        std::printf("zip %d %d\n", a, b);
    }

    frd::apply_for_each(
        frd::overloaded{
            [](int x, char c) { std::printf("int %d char %c\n", x, c); },

            [](char c, int x) { std::printf("char %c int %d\n", c, x); }
        },

        frd::pair{69, 'h'}, frd::pair{'a', 420}
    );

    std::printf(
        "%d\n",
        frd::get<1>(frd::pair{4, 6}.transform([](int x) {
            return x * 2;
        }))
    );

    const auto other = frd::tuple_convert<short, short>(frd::tuple{'h', 's'});
    std::printf("convert %d %d\n", frd::get<0>(other), frd::get<1>(other));

    for (auto i : frd::interval(0, frd::unreachable_sentinel) | frd::views::iterators) {
        if (*i == 6) {
            break;
        }

        i += 1;
        std::printf("loop %d\n", *i);
    }

    std::printf(
        "make_from_tuple %d\n",

        frd::make_from_tuple<int>(frd::tuple{666})
    );

    return 0;
}
