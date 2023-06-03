#pragma once
#include <bit>
#include <concepts>
#include <cstdint>
#include <type_traits>

/**
 * @file
 * A structural type representing powers of two and the corresponding
 * concepts and tools
 *
 * @author    patternnoster@github
 * @copyright 2023, under the MIT License (see /LICENSE for details)
 **/

namespace nupp {

/**
 * @brief An std::integral type other than bool (useful for templates
 *        like std::make_unsigned)
 **/
template <typename T>
concept integer = std::integral<T> && !std::same_as<std::remove_cv_t<T>, bool>;

/**
 * @brief An std::unsigned_integral type other than bool
 **/
template <typename T>
concept unsigned_integer = integer<T> && std::unsigned_integral<T>;

struct pow2_t;

/**
 * @brief An integer type (standard integral except bool) or pow2_t
 *        (and the cv-qualified versions of those)
 **/
template <typename T>
concept extended_integer =
  integer<T> || std::same_as<std::remove_cv_t<T>, pow2_t>;

/**
 * @brief A standard arithmetic type or pow2_t (and the cv-qualified
 *        versions of those)
 **/
template <typename T>
concept extended_arithmetic =
  std::is_arithmetic_v<T> || std::same_as<std::remove_cv_t<T>, pow2_t>;

/**
 * @brief Checks if an integer is a power of 2
 **/
template <unsigned_integer T>
constexpr bool is_pow2(const T arg) noexcept {
  return std::has_single_bit(arg);
}

using std::size_t;

template <typename T>
constexpr size_t sizeof_bit = sizeof(T) * 8;

/**
 * @brief A structural type intended for numeric values that are
 *        powers of 2 between 1 and (including) 2^63
 **/
struct pow2_t {
  constexpr static struct floor_tag {} floor = {};
  constexpr static struct ceil_tag {} ceil = {};
  constexpr static struct exact_tag {} exact = {};

  /**
   * @brief Flooring constructor: if the argument is not itself a
   *        power of 2, it is rounded to the closest lower power of 2
   * @note  Signed types are accepted for convenience only. A negative
   *        or zero argument leads to undefined behaviour
   * @param arg where 0 < arg < 2^64
   **/
  template <integer T>
  constexpr pow2_t(const T arg, const floor_tag) noexcept
    : value(uint64_t(1) << (sizeof_bit<T> - 1)
            >> std::countl_zero(std::make_unsigned_t<T>(arg))) {}

  /**
   * @brief Ceiling constructor: if the argument is not itself a
   *        power of 2, it is rounded to the closest higher power of 2
   * @note  Signed types are accepted for convenience only. A negative
   *        or zero argument leads to undefined behaviour
   * @param arg where 0 < arg <= 2^63
   **/
  template <integer T>
  constexpr pow2_t(const T arg, const ceil_tag) noexcept
    : pow2_t((uint64_t(arg) << 1) - 1, floor) {}

  /**
   * @brief Exact constructor: the argument *must* be a power of 2,
   *        otherwise the behaviour is undefined
   * @param arg where 0 < arg <= 2^63
   **/
  template <integer T>
  constexpr pow2_t(const T arg, const exact_tag) noexcept: value(arg) {}

  /**
   * @brief By default, use the flooring constructor (and not the
   *        exact one, better safe than sorry)
   **/
  template <integer T>
  constexpr explicit pow2_t(const T arg) noexcept: pow2_t(arg, floor) {}

  /**
   * @brief The default constructor creates 2^0
   **/
  constexpr pow2_t() noexcept: value(1) {}

  constexpr pow2_t(const pow2_t&) = default;
  constexpr pow2_t(pow2_t&&) = default;

  constexpr pow2_t& operator=(const pow2_t& rhs) noexcept {
    /* Const on the value is nothing but a poor substitute for a
     * private access specifier (which we cannot use because we want
     * this type to be structural). So the cast is perfectly valid */
    const_cast<uint64_t&>(value) = rhs.value;
    return *this;
  }

  constexpr pow2_t& operator=(pow2_t&& rhs) noexcept {
    return *this = rhs;  // Nothing to win from move
  }

  constexpr auto log2() const noexcept {
    return uint8_t(std::countr_zero(value));
  }

  /**
   * @brief Returns a mask with all the bits from the first to the
   *        log2()-th set. Useful for modular arithmetic
   **/
  constexpr uint64_t get_mask() const noexcept {
    return value - 1;
  }

  constexpr auto operator<=>(const pow2_t rhs) const noexcept {
    return value <=> rhs.value;
  }

  constexpr pow2_t operator>>(const int shift) const noexcept {
    return { value >> shift, exact };
  }

  constexpr pow2_t operator<<(const int shift) const noexcept {
    return { value << shift, exact };
  }

  constexpr pow2_t operator*(const pow2_t& rhs) const noexcept {
    return *this << rhs.log2();
  }

  constexpr pow2_t operator/(const pow2_t& rhs) const noexcept {
    return *this >> rhs.log2();
  }

  constexpr uint64_t operator%(const pow2_t& rhs) const noexcept {
    return value & rhs.get_mask();
  }

  template <unsigned_integer T>
  friend constexpr T operator%(const T& lhs, const pow2_t& rhs) noexcept {
    return lhs & static_cast<T>(rhs.get_mask());
  }

  constexpr operator uint64_t() const noexcept {
    return value;
  }

  template <std::integral T>
  constexpr explicit operator T() const noexcept {
    return T(value);
  }

  const uint64_t value;  // This is public to keep the type structural, but
                         // const to prevent users from messing with it
};

} // namespace nupp

namespace std {

template <typename T> requires(std::is_arithmetic_v<T>)
struct common_type<nupp::pow2_t, T> {
  using type = std::common_type_t<uint64_t, T>;
};

template <typename T> requires(std::is_arithmetic_v<T>)
struct common_type<T, nupp::pow2_t> {
  using type = std::common_type_t<T, uint64_t>;
};

} // namespace std
