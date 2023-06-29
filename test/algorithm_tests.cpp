#include <cmath>
#include <cstdint>
#include <gtest/gtest.h>
#include <limits>
#include <numeric>
#include <tuple>
#include <type_traits>
#include <utility>

#include "nupp/algorithm.hpp"

using namespace nupp;

template <typename...> struct type_list{};

template <typename, typename> struct prepend;
template <typename T, typename... Ts>
struct prepend<T, type_list<Ts...>> { using result = type_list<T, Ts...>; };

// We only generate non-empty subsets

template <typename, typename> struct add_to_subsets;
template <typename T, typename... Ls>
struct add_to_subsets<T, type_list<Ls...>> {
  using result = type_list<Ls..., type_list<T>,
                           typename prepend<T, Ls>::result...>;
};

template <typename L> struct subsets { using result = type_list<L>; };
template <typename H, typename... Ts> requires(sizeof...(Ts) > 0)
struct subsets<type_list<H, Ts...>> {
  using result =
    add_to_subsets<H, typename subsets<type_list<Ts...>>::result>::result;
};

template <typename T, typename... Ts>
constexpr bool any = (std::same_as<T, Ts> || ...);

template <typename T, typename... Ts>
constexpr bool all = (std::same_as<T, Ts> && ...);

class AlgorithmTests: public ::testing::Test {
private:
  using int_args = type_list<pow2_t, uint32_t, uint64_t, pow2_t,
                             int32_t, int64_t, pow2_t>;

  using int_subsets = subsets<int_args>::result;
  using all_subsets =
    subsets<typename prepend<double, int_args>::result>::result;

  template <typename T>
  static T gen_element() noexcept {
    // To check for the border cases for integers, we will generate
    // the min/max value in some cases
    enum { minv, maxv, zerov, regv };
    const int choice = rand() % 4;

    if constexpr (std::same_as<T, pow2_t>) {
      if (choice == minv || choice == zerov) return pow2_t{ 1, pow2_t::exact };
      if (choice == maxv) return pow2_t{ uint64_t{1} << 63, pow2_t::exact };
      return pow2_t{ uint64_t(1) << (rand() % 8), pow2_t::exact };
    }
    else {
      if (choice == minv) return std::numeric_limits<T>::min();
      if (choice == zerov) return T(0);
      if (choice == maxv) return std::numeric_limits<T>::max();

      // Normal now
      if constexpr (std::is_floating_point_v<T>)
        return T(-100 + rand() % 201) / T(1 + rand() % 100);
      else if constexpr (std::is_signed_v<T>)
        return T(-42 + rand() % 85);
      else
        return rand() % 42;
    }
  }

protected:
  template <typename... Ts>
  static auto get_proper_common_t() noexcept {
    // Cleaner than std::conditional_t
    if constexpr (all<pow2_t, Ts...>) return pow2_t{};
    else if constexpr (any<double, Ts...>) return double{};
    else if constexpr (any<pow2_t, Ts...> || any<uint64_t, Ts...>)
      return uint64_t{};
    else if constexpr (any<int64_t, Ts...>) return int64_t{};
    else if constexpr (any<uint32_t, Ts...>) return uint32_t{};
    else return int32_t{};
  }

  template <typename... Ts>
  static auto get_proper_signed_common_t() noexcept {
    if constexpr (any<double, Ts...>) return double{};
    else if constexpr (any<int64_t, Ts...>) return int64_t{};
    else if constexpr (any<int32_t, Ts...>) return int32_t{};
    else return get_proper_common_t<Ts...>();
  }

  template <typename List, typename Func>
  static void invoke_cases(Func&& f) noexcept {
    [&f]<typename... Cases>(const type_list<Cases...>) {
      (..., std::apply(std::forward<Func>(f),
                       []<typename... Args>(const type_list<Args...>) {
                         return std::tuple{ gen_element<Args>()... };
                       }(Cases{})));
    }(List{});
  }

  template <typename Func>
  void apply_to_int_cases(Func&& f) noexcept {
    invoke_cases<int_subsets>(std::forward<Func>(f));
  }

  template <typename Func>
  static void apply_to_all_cases(Func&& f) noexcept {
    invoke_cases<all_subsets>(std::forward<Func>(f));
  }
};

template <template <typename...> typename F, typename... Ts>
type_list<F<Ts>...> map(type_list<Ts...>);

TEST_F(AlgorithmTests, absolute) {
  using types = type_list<int8_t, int16_t, int32_t, int64_t,
                          uint8_t, uint16_t, uint32_t, uint64_t,
                          float, double, long double, pow2_t>;
  using cases = decltype(map<type_list>(types{}));

  constexpr size_t Runs = 1000;
  for (size_t i = 0; i < Runs; ++i)
    this->invoke_cases<cases>([](const auto arg) {
      using arg_t = decltype(arg);
      const auto result = absolute(arg);

      if constexpr (std::signed_integral<arg_t>) {
        using exp_result_t = std::make_unsigned_t<arg_t>;
        EXPECT_TRUE((std::same_as<exp_result_t, decltype(result)>));

        // Careful with the min() value (on 2's complement) and
        // integer promotion
        if (arg < arg_t(0)) {
          EXPECT_EQ(result, exp_result_t(-exp_result_t(arg)));
        }
        else {
          EXPECT_EQ(result, exp_result_t(arg));
        }
      }
      else {
        EXPECT_TRUE((std::same_as<arg_t, decltype(result)>));
        if constexpr (std::floating_point<arg_t>) {
          EXPECT_EQ(result, std::abs(arg));
        }
        else {
          EXPECT_EQ(result, arg);
        }
      }
    });
}

TEST_F(AlgorithmTests, meta) {
  // A meta test for our own test cases generation
  constexpr static size_t MaxSize = 8;

  size_t sizes[MaxSize + 1] = {};
  this->apply_to_all_cases([&sizes](const auto... args) {
    constexpr size_t size = sizeof...(args);
    ASSERT_GT(size, 0);
    ASSERT_LE(size, MaxSize);

    ++sizes[0];
    ++sizes[size];
  });

  EXPECT_EQ(sizes[0], (1 << MaxSize) - 1);  // no empty
  uint64_t base_coeff = MaxSize;  // binomial coefficient
  for (size_t i = 1; i <= MaxSize; ++i) {
    EXPECT_EQ(sizes[i], base_coeff);
    base_coeff*= MaxSize - i;
    base_coeff/= i + 1;
  }
}

TEST_F(AlgorithmTests, minimum) {
  constexpr size_t Runs = 1000;
  const auto test = []<typename... Ts>(const Ts... args) {
    const auto result = minimum(args...);

    using R = decltype(result);
    ASSERT_TRUE((std::same_as<std::decay_t<R>,
                              decltype(get_proper_signed_common_t<Ts...>())>));

    if constexpr (std::signed_integral<R>) {
      /* We need to be careful with unsigned arguments since we cannot
       * always convert them to the signed result. Since this is a
       * minimum, we can simply ignore those that would overflow */
      constexpr auto lim = std::numeric_limits<R>::max();
      EXPECT_TRUE(((args > lim || result <= R(args)) && ...));
      EXPECT_TRUE(((args <= lim && result == R(args)) || ...));
    }
    else {
      // If the result is unsigned integral, that means all the
      // arguments are as well
      EXPECT_TRUE(((result <= R(args)) && ...));
      EXPECT_TRUE(((result == R(args)) || ...));
    }
  };

  for (size_t i = 0; i < Runs; ++i)
    this->apply_to_all_cases(test);
}

TEST_F(AlgorithmTests, maximum) {
  constexpr size_t Runs = 1000;

  const auto test = []<typename... Ts>(const Ts... args) {
    const auto result = maximum(args...);

    using R = decltype(result);
    ASSERT_TRUE((std::same_as<std::decay_t<R>,
                              decltype(get_proper_common_t<Ts...>())>));

    if constexpr (std::unsigned_integral<R>) {
      /* We need to be careful with signed arguments since we cannot
       * always convert them to unsigned. Luckily, the negative signed
       * arguments can simply be ignored: an unsigned maximum is
       * always at least 0 */
      EXPECT_TRUE(((args < decltype(args){} || result >= R(args)) && ...));
      EXPECT_TRUE(((args >= decltype(args){} && result == R(args)) || ...));
      return;
    }
    else {
      // Otherwise the conversion to common type (even if pow2_t) is
      // safe, so we can just do:
      EXPECT_TRUE(((result >= R(args)) && ...));
      EXPECT_TRUE(((result == R(args)) || ...));
    }
  };

  for (size_t i = 0; i < Runs; ++i)
    this->apply_to_all_cases(test);
}

auto rec_call_gcd(const auto head, const auto... tail) noexcept {
  if constexpr (sizeof...(tail) == 0) return head;
  else return std::gcd(head, rec_call_gcd(tail...));
};

template <typename T>
uint64_t abs_ext(const T arg) noexcept {
  if constexpr (std::same_as<T, pow2_t>) return arg.value;
  else return absolute(arg);
}

TEST_F(AlgorithmTests, gcd) {
  constexpr size_t Runs = 1000;

  const auto test = [](auto... args) {
    const auto result = gcd(args...);
    const auto expected = rec_call_gcd(abs_ext(args)...);
    EXPECT_EQ(result, expected);
  };

  for (size_t i = 0; i < Runs; ++i)
    this->apply_to_int_cases(test);
}

auto rec_call_lcm(const auto head, const auto... tail) noexcept {
  if constexpr (sizeof...(tail) == 0) return head;
  else return std::lcm(head, rec_call_lcm(tail...));
};

TEST_F(AlgorithmTests, lcm) {
  constexpr size_t Runs = 1000;

  const auto test = [](const auto... args) {
    // We will skip the cases where the product of abs of all
    // arguments is inexpressible as the unsigned common type, since
    // that may lead to UB with lcm
    const size_t max_log = (pow2_t{ abs_ext(args), pow2_t::ceil }.log2() + ...);
    using result_t = std::common_type_t<decltype(args)...>;
    if (max_log > sizeof_bit<result_t>) return;

    const auto result = lcm(args...);
    const auto expected = rec_call_lcm(abs_ext(args)...);
    EXPECT_EQ(result, expected);
  };

  for (size_t i = 0; i < Runs; ++i)
    this->apply_to_int_cases(test);
}
