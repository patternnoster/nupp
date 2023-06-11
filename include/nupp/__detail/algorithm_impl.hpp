#pragma once
#include <cmath>
#include <concepts>
#include <type_traits>

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

} // namespace nupp::__detail
