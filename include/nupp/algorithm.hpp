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
using unsigned_for_t = __detail::unsigned_for<T>::type;

/**
 * @brief The common type of all the signed types in a given
 *        list. Equals their std::common_type_t, if none are signed
 **/
template <extended_arithmetic... Ts> requires(sizeof...(Ts) > 0)
using signed_common_type_t = __detail::signed_common_type<Ts...>::type;

/**
 * @brief Returns the absolute value of the provided argument
 * @note  Unlike the std::abs function, does not cause undefined
 *        behaviour when called with the minimum value for signed
 *        integral types. Always returns the unsigned type
 **/
template <extended_arithmetic T>
constexpr unsigned_for_t<T> absolute(const T arg) noexcept {
  return __detail::absolute(arg);
}

/**
 * @brief The minimum function for any (non-zero) number of arithmetic
 *        arguments. Returns (a copy of) the result, converted to
 *        signed_common_type_t<Args...> (i.e., std::common_type_t of
 *        all the signed argument types, or all argument types if none
 *        of them are signed)
 * @note  The algorithm works correctly when mixing signed and unsigned
 *        types, but narrowing conversions may be performed when
 *        mixing floating point and integral arguments
 **/
template <extended_arithmetic... Args> requires(sizeof...(Args) > 0)
constexpr signed_common_type_t<Args...> minimum(const Args... args) noexcept {
  return __detail::invoker<__detail::algorithms::minimum>(args...);
}

/**
 * @brief The maximum function for any (non-zero) number of arithemitc
 *        arguments. Returns (a copy of) the result, converted to
 *        std::common_type_t<Args...>
 * @note  The algorithm works correctly when mixing signed and unsigned
 *        types, but narrowing conversions may be performed when
 *        mixing floating point and integral arguments
 **/
template <extended_arithmetic... Args> requires(sizeof...(Args) > 0)
constexpr std::common_type_t<Args...> maximum(const Args... args) noexcept {
  return __detail::invoker<__detail::algorithms::maximum>(args...);
}

/**
 * @brief The greatest common divisor function for any (non-zero)
 *        number of (absolute values of) the integer arguments
 * @note  Unlike std::gcd, the behaviour is well-defined for minimum
 *        values of signed arguments. Always returns an unsigned type
 **/
template <extended_integer... Args> requires(sizeof...(Args) > 0)
constexpr unsigned_for_t<std::common_type_t<Args...>> gcd
  (const Args... args) noexcept {
  return __detail::invoker<__detail::algorithms::gcd>(args...);
}

/**
 * @brief The least common multiple function for any (non-zero)
 *        number of (absolute values of) the integer arguments
 * @note  As with std::lcm, the behaviour is undefined if the least
 *        common multiple is not representable as the unsigned common
 *        type (e.g., lcm(uint32_t(2^31), int32_t(3))). Unlike
 *        std::lcm, the behaviour is well-defined for minimum values
 *        of signed arguments. Always returns an unsigned type
 **/
template <extended_integer... Args> requires(sizeof...(Args) > 0)
constexpr unsigned_for_t<std::common_type_t<Args...>>lcm
(const Args... args) noexcept {
  return __detail::invoker<__detail::algorithms::lcm>(args...);
}

} // namespace nupp
