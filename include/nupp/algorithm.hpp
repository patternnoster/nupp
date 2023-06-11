#pragma once
#include "pow2_t.hpp"

#include "__detail/algorithm_impl.hpp"

/**
 * @file
 * Extended versions of some standard numeric algorithms
 *
 * @author    patternnoster@github
 * @copyright 2023, under the MIT License (see /LICENSE for details)
 **/

namespace nupp {

/**
 * @brief The corresponding unsigned type for extended arithmetic
 *        values (for every type T that is not an std::signed_integral
 *        is same as T, otherwise same as std::make_unsigned_t<T>)
 **/
template <extended_arithmetic T>
using unsigned_for = __detail::unsigned_for_t<T>::result;

/**
 * @brief Returns the absolute value of the provided argument
 * @note  Unlike the std::abs function, does not cause undefined
 *        behaviour when called with the minimum value for signed
 *        integral types. Always returns the unsigned type
 **/
template <extended_arithmetic T>
constexpr unsigned_for<T> absolute(const T arg) noexcept {
  return __detail::absolute(arg);
}

/**
 * @brief The minimum function for any (non-zero) number of arithmetic
 *        arguments. Returns (a copy of) the result, converted to
 *        std::common_type_t<Args...>
 **/
template <extended_arithmetic... Args> requires(sizeof...(Args) > 0)
constexpr std::common_type_t<Args...> minimum(const Args...) noexcept;

/**
 * @brief The maximum function for any (non-zero) number of arithemitc
 *        arguments. Returns (a copy of) the result, converted to
 *        std::common_type_t<Args...>
 **/
template <extended_arithmetic... Args> requires(sizeof...(Args) > 0)
constexpr std::common_type_t<Args...> maximum(const Args...) noexcept;

/**
 * @brief The greatest common divisor function for any (non-zero)
 *        number of (absolute values of) the integer arguments
 **/
template <extended_integer... Args> requires(sizeof...(Args) > 0)
constexpr unsigned_for<std::common_type_t<Args...>> gcd(const Args...) noexcept;

/**
 * @brief The least common multiple function for any (non-zero)
 *        number of (absolute values of) the integer arguments
 **/
template <extended_integer... Args> requires(sizeof...(Args) > 0)
constexpr unsigned_for<std::common_type_t<Args...>>lcm(const Args...) noexcept;

} // namespace nupp
