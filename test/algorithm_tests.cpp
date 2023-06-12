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

class AlgorithmTests: public ::testing::Test {
private:
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
