#pragma once
#include <concepts>
#include <cstdint>
#include <type_traits>

namespace nupp {

/**
 * @brief An std::integral type other than bool (useful for templates
 *        like std::make_unsigned)
 **/
template <typename T>
concept integer = std::integral<T> && !std::same_as<std::remove_cv_t<T>, bool>;

using std::size_t;

template <typename T>
constexpr size_t sizeof_bit = sizeof(T) * 8;

/**
 * @brief A structural type intended for numeric values that are
 *        powers of 2 between 1 and (including) 2^63
 **/
struct pow2_t {
  /**
   * @brief The default constructor creates 2^0
   **/
  constexpr pow2_t() noexcept: value(1) {}

  /**
   * @brief Floors the given argument to the closest lower power of 2
   * @note  Signed types are accepted for convenience only. A negative
   *        or zero argument leads to undefined behaviour
   * @param arg where 0 < arg < 2^64
   **/
  template <integer T>
  explicit constexpr pow2_t(const T arg) noexcept
    : value(uint64_t(1) << (sizeof_bit<T> - 1)
            >> std::countl_zero(std::make_unsigned_t<T>(arg))) {}

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

  constexpr auto operator<=>(const pow2_t rhs) const noexcept {
    return value <=> rhs.value;
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
