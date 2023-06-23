#pragma once
#include <algorithm>
#include <bit>
#include <cmath>
#include <concepts>
#include <type_traits>
#include <utility>

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
struct unsigned_for { using type = T; };

template <std::signed_integral T>
struct unsigned_for<T> { using type = std::make_unsigned_t<T>; };

template <typename... Ts> requires(sizeof...(Ts) > 0)
struct signed_common_type { using type = std::common_type_t<Ts...>; };

template <typename H, typename... Ts>
  requires(sizeof...(Ts) > 0
           && (std::is_signed_v<H> || ... || std::is_signed_v<Ts>))
struct signed_common_type<H, Ts...> {
  using common_tail_t = signed_common_type<Ts...>::type;
  using type =
    std::conditional_t<std::is_signed_v<H>,
                       std::conditional_t<(std::is_signed_v<Ts> || ...),
                                          std::common_type_t<H, common_tail_t>,
                                          H>, common_tail_t>;
};

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

// For the remaining two methods we're going to need some meta-helpers

template <typename S, size_t> struct append_idx;
template <size_t... _idxs, size_t _arg>
struct append_idx<std::index_sequence<_idxs...>, _arg> {
  using result = std::index_sequence<_idxs..., _arg>;
};

template <typename T = std::index_sequence<>,
          typename F = std::index_sequence<>>
struct partition_result {
  using true_indices = T;
  using false_indices = F;
};

template <typename R, size_t _idx>
using append_true =
  partition_result<typename append_idx<typename R::true_indices,
                                       _idx>::result,
                   typename R::false_indices>;

template <typename R, size_t _idx>
using append_false =
  partition_result<typename R::true_indices,
                   typename append_idx<typename R::false_indices,
                                       _idx>::result>;

template <template <typename> typename, typename R, typename...>
struct partition_t { using result = R; };

template <template <typename> typename P, typename R,
          typename H, typename... Ts>
struct partition_t<P, R, H, Ts...> {
  constexpr static size_t next_idx =
    R::true_indices::size() + R::false_indices::size();

  using head_result = std::conditional_t<P<R>::value,
                                         append_true<R, next_idx>,
                                         append_false<R, next_idx>>;

  using result = partition_t<P, head_result, Ts...>::result;
};

template <template <typename> typename P, typename... Ts>
using partition = partition_t<P, partition_result<>, Ts...>::result;

// Okay, now the real thing

template <algorithms _algo>
template <arithmetic... Args>
constexpr auto invoker_t<_algo>::operator()(const Args...) const noexcept {
  using signed_partition = partition<std::is_signed, Args...>;
  return std::common_type_t<Args...>{};
}

template <algorithms _algo>
template <typename... Args>
constexpr auto invoker_t<_algo>::operator()(const Args...) const noexcept {
  return std::common_type_t<Args...>{};
};

template <algorithms _algo> constexpr invoker_t<_algo> invoker{};

} // namespace nupp::__detail
