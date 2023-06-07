#pragma once
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

} // namespace nupp::__detail
