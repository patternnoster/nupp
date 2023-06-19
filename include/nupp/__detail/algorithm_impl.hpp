#pragma once
#include <algorithm>
#include <bit>
#include <cmath>
#include <concepts>
#include <type_traits>

#include "../pow2_t.hpp"

/**
 * @file
 * The implementation of the extended versions of some standard
 * numeric algorithms
 *
 * @author    patternnoster@github
 * @copyright 2023, under the MIT License (see /LICENSE for details)
 **/

namespace nupp::__detail {

template <typename T>
struct unsigned_for_t { using result = T; };

template <std::signed_integral T>
struct unsigned_for_t<T> { using result = std::make_unsigned_t<T>; };

template <typename T>
constexpr auto absolute(const T arg) noexcept {
  if constexpr (std::signed_integral<T>) {
    using result_t = std::make_unsigned_t<T>;
    /* This compiles to the exact same code the stdlibc++'s abs does,
     * but technically this doesn't cause UB when applied to the
     * minimum integer value (unlike abs, even in C++20 with its 2's
     * complement requirement!) */
    return result_t(arg >= 0 ? arg : -result_t(arg));
  }
  else if constexpr (std::floating_point<T>) return (std::abs)(arg);
  else return arg;
}

// I wonder why STL is missing this...
template <typename T>
concept arithmetic = std::is_arithmetic_v<T>;

enum class algorithms { minimum, maximum, gcd, lcm };

/**
 * @brief The unified functor type that implements given numerical
 *        algorithms on any number of arguments
 **/
template <algorithms _algo>
struct invoker_t {
  template <std::same_as<pow2_t>... Args>  // pow2_t special case
  constexpr pow2_t operator()(const Args...) const noexcept;

  template <arithmetic... Args>            // non-pow2_t case
  constexpr auto operator()(const Args...) const noexcept;

  template <typename... Args>              // mixed case
  constexpr auto operator()(const Args...) const noexcept;
};

template <algorithms _algo>
template <std::same_as<pow2_t>... Args>
constexpr pow2_t invoker_t<_algo>::operator()
  (const Args... args) const noexcept {
  constexpr size_t size = sizeof...(Args);
  static_assert(size > 0);

  if constexpr (size == 1) return (args, ...);
  else if constexpr (size == 2) {
    // The optimization is not worth it with only 2 arguments
    if constexpr (_algo == algorithms::minimum)
      return pow2_t{ (std::min)({args.value...}), pow2_t::exact };
    else
      return pow2_t{ (std::max)({args.value...}), pow2_t::exact };
  }
  else {
    // Okay, now it's worth it: more than 2 arguments
    const uint64_t or_all = (args.value | ...);
    if constexpr (_algo == algorithms::minimum)
      return pow2_t{ uint64_t(1) << std::countr_zero(or_all),
                     pow2_t::exact };
    else
      return pow2_t{ uint64_t(1) << 63 >> std::countl_zero(or_all),
                     pow2_t::exact };
  }
}

template <algorithms _algo>
template <arithmetic... Args>
constexpr auto invoker_t<_algo>::operator()(const Args...) const noexcept {
  return std::common_type_t<Args...>{};
}

template <algorithms _algo>
template <typename... Args>
constexpr auto invoker_t<_algo>::operator()(const Args...) const noexcept {
  return std::common_type_t<Args...>{};
};

template <algorithms _algo> constexpr invoker_t<_algo> invoker{};

} // namespace nupp::__detail
