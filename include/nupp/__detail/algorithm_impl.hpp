#pragma once
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
constexpr pow2_t invoker_t<_algo>::operator()(const Args...) const noexcept {
  return {};
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
