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
  void apply_to_all_cases(Func&& f) noexcept {
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
  const auto test = [](const auto... args) {
    const auto result = minimum(args...);

    using R = decltype(result);
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

  const auto test = [](const auto... args) {
    const auto result = maximum(args...);

    using R = decltype(result);
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
