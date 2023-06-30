#pragma once
#include <algorithm>
#include <bit>
#include <cmath>
#include <concepts>
#include <numeric>
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

  template <arithmetic... Args>            // non-pow2_t case (min/max)
    requires(_algo == algorithms::minimum || _algo == algorithms::maximum)
  constexpr auto operator()(const Args...) const noexcept;

  template <integer T, integer... Args>    // non-pow2_t case (gcd/lcm)
    requires(_algo == algorithms::gcd || _algo == algorithms::lcm)
  constexpr inline auto operator()(const T, const Args...) const noexcept;

  template <typename... Args>              // mixed case
  constexpr auto operator()(const Args...) const noexcept;
};

template <algorithms _algo>
template <std::same_as<pow2_t>... Args>
constexpr pow2_t invoker_t<_algo>::operator()
  (const Args... args) const noexcept {
  constexpr size_t size = sizeof...(Args);
  static_assert(size > 0);

  constexpr bool need_minimum =  // for pow2s, gcd is min, lcm is max
    _algo == algorithms::minimum || _algo == algorithms::gcd;

  if constexpr (size == 1) return (args, ...);
  else if constexpr (size == 2) {
    // The optimization is not worth it with only 2 arguments
    if constexpr (need_minimum)
      return pow2_t{ (std::min)({args.value...}), pow2_t::exact };
    else
      return pow2_t{ (std::max)({args.value...}), pow2_t::exact };
  }
  else {
    // Okay, now it's worth it: more than 2 arguments
    const uint64_t or_all = (args.value | ...);
    if constexpr (need_minimum)
      return pow2_t{ uint64_t(1) << std::countr_zero(or_all),
                     pow2_t::exact };
    else
      return pow2_t{ uint64_t(1) << 63 >> std::countl_zero(or_all),
                     pow2_t::exact };
  }
}

template <algorithms _algo>
template <integer T, integer... Args>
  requires(_algo == algorithms::gcd || _algo == algorithms::lcm)
constexpr inline auto invoker_t<_algo>::operator()
  (const T head, const Args... tail) const noexcept {
  // For lcm and gcd we will simply call the STL functions
  if constexpr (sizeof...(Args) == 0)
    return absolute(head);
  else {
    using result_t = std::make_unsigned_t<std::common_type_t<T, Args...>>;
    result_t lhs = absolute(head);
    if constexpr (_algo == algorithms::gcd)
      return ((lhs = std::gcd(lhs, tail)), ...);
    else
      return ((lhs = std::lcm(lhs, tail)), ...);
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

  using head_result = std::conditional_t<P<H>::value,
                                         append_true<R, next_idx>,
                                         append_false<R, next_idx>>;

  using result = partition_t<P, head_result, Ts...>::result;
};

template <template <typename> typename P, typename... Ts>
using partition = partition_t<P, partition_result<>, Ts...>::result;

// Okay, now the real thing

template <algorithms _algo>
template <arithmetic... Args>
  requires(_algo == algorithms::minimum || _algo == algorithms::maximum)
constexpr auto invoker_t<_algo>::operator()(const Args... args) const noexcept {
  constexpr size_t size = sizeof...(Args);
  static_assert(size > 0);

  if constexpr (size == 1) return (args, ...);
  else {
    using common_t = std::common_type_t<Args...>;
    if constexpr ((std::is_signed_v<Args> && ...)
                  || (!std::is_signed_v<Args> && ...)) {
      // If all the arguments are signed or not at the same time, we
      // can simply convert them to the common type
      if constexpr (_algo == algorithms::minimum)
        return (std::min)({ common_t(args)... });
      else
        return (std::max)({ common_t(args)... });
    }
    else {
      /* Otherwise, problem: we cannot convert signed types to the
       * common type if it's unsigned, otherwise the result might be
       * incorrect. So deal with them separately.
       * As always, crisis is an opportunity: if the signed result is
       * negative, we can cut away a few steps... */
      using signed_partition = partition<std::is_signed, Args...>;

      const auto args_tuple = std::forward_as_tuple(args...);
      return [this, &args_tuple]<size_t... _signed, size_t... _non_signed>
        (std::index_sequence<_signed...>, std::index_sequence<_non_signed...>) {
        // First get the signed result (this calls a constexpr condition above)
        const auto signed_result = (*this)(std::get<_signed>(args_tuple)...);
        if constexpr (_algo == algorithms::minimum)
          if (signed_result <= 0) // No positive (unsigned) number can be less
            return signed_result;  // The type is signed_common_type_t

        // Now same for the unsigned one
        const auto unsigned_result =
          (*this)(std::get<_non_signed>(args_tuple)...);
        if constexpr (_algo == algorithms::maximum) {
          if (signed_result <= 0) // Same as above
            return common_t(unsigned_result);  // never narrowing for integral
        }

        // The signed_result is positive here, otherwise we would've
        // returned. Which means, conversion is safe:
        const auto result =
          (*this)(common_t(signed_result), common_t(unsigned_result));

        if constexpr (_algo == algorithms::minimum)
          // The conversion is safe, since the minimum cannot be
          // greater than the maximum signed value
          return decltype(signed_result)(result);
        else return result; // Already of common type
      }(typename signed_partition::true_indices{},
        typename signed_partition::false_indices{});
    }
  }
}

template <typename T>
using pow2_predicate = std::is_same<T, pow2_t>;

template <algorithms _algo>
template <typename... Args>
constexpr auto invoker_t<_algo>::operator()(const Args... args) const noexcept {
  // So we're mixing pow2s and non-pow2s here...
  static_assert(sizeof...(Args) > 0);
  using pow2_partition = partition<pow2_predicate, Args...>;

  const auto args_tuple = std::forward_as_tuple(args...);
  return [this, &args_tuple]<size_t... _pow2s,
                             size_t _non_pow2_head, size_t... _non_pow2s_tail>
    (std::index_sequence<_pow2s...>,
     std::index_sequence<_non_pow2_head, _non_pow2s_tail...>) {
    // First compute the pow2 version separately (this always
    // calls the overload above)
    const pow2_t pow2_result = (*this)(std::get<_pow2s>(args_tuple)...);

    /* For the minimum and maximum we simply use that value as one of
     * the arguments (as uint64_t), there is both no way and no reason
     * to do it any more efficiently */
    if constexpr (_algo == algorithms::minimum || _algo == algorithms::maximum)
      return (*this)(pow2_result.value, std::get<_non_pow2_head>(args_tuple),
                     std::get<_non_pow2s_tail>(args_tuple)...);
    else {
      /* In case of gcd/lcm however, it makes sense to optimize even a
       * single pow2 since bit shifts are so much faster than
       * division. First, get all the non-pow2s results */
      using result_t = std::make_unsigned_t<std::common_type_t<Args...>>;
      const result_t non_pow2_result =
        (*this)(result_t(absolute(std::get<_non_pow2_head>(args_tuple))),
                std::get<_non_pow2s_tail>(args_tuple)...);

      const int zero_diff =
        int(pow2_result.log2()) - std::countr_zero(non_pow2_result);

      if constexpr (_algo == algorithms::gcd)
        // Now the bit trick: gcd with a pow2 is a pow2 with the
        // minimum number of right zeros...
        return
          result_t{ zero_diff <= 0 ? pow2_result : pow2_result >> zero_diff };
      else
        // ...and the lcm of x and pow2 has the maximum number of
        // zeros on the right followed by all the x's other bits
        return
          zero_diff <= 0 ? non_pow2_result : non_pow2_result << zero_diff;
    }
  }(typename pow2_partition::true_indices{},
    typename pow2_partition::false_indices{});
};

template <algorithms _algo> constexpr invoker_t<_algo> invoker{};

} // namespace nupp::__detail
