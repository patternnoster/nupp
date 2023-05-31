#pragma once
#include <compare>
#include <iterator>

#include "pow2_t.hpp"

/**
 * @file
 * An iterator for bit masks in the std::ranges style
 *
 * @author    patternnoster@github
 * @copyright 2023, under the MIT License (see /LICENSE for details)
 **/

namespace nupp {

/**
 * @brief An iterator that goes through set bits in a mask starting
 *        from the least significant
 * @note  The std::default_sentinel is std::sized_sentinel_for this
 *        iterator
 * @note  This iterator is dereferenceable if and only if it doesn't
 *        compare equal to std::default_sentinel. The default
 *        constructed mask_iterator always does (and can be used to
 *        denote range end as well)
 **/
template <unsigned_integer T = size_t>
class mask_iterator {
public:
  using value_type = T;
  using difference_type = std::make_signed_t<T>;

  constexpr mask_iterator() noexcept = default;
  constexpr mask_iterator(const T mask) noexcept: mask_(mask) {}

  constexpr bool operator==(const mask_iterator& other) const noexcept {
    return mask_ == other.mask_;
  }

  constexpr bool operator==(const std::default_sentinel_t) const noexcept {
    return !mask_;
  }

  constexpr pow2_t operator*() const noexcept {
    return { T(1) << std::countr_zero(mask_), pow2_t::exact };
  }

  constexpr mask_iterator& operator++() noexcept {
    mask_^= this->operator*();
    /* This approach is better than the regular &=(mask_-1), as it
     * allows a good compiler to optimize out one instruction in
     * cycles with dereferencing (which is normally the case) */
    return *this;
  }

  constexpr mask_iterator operator++(int) noexcept {
    mask_iterator result{mask_};
    ++*this;
    return result;
  }

  constexpr auto count() const noexcept {
    return difference_type(std::popcount(mask_));
  }

  constexpr std::strong_ordering operator<=>
    (const mask_iterator& other) const noexcept {
    return other.count() <=> count();
  }

  constexpr difference_type operator-
    (const mask_iterator& other) const noexcept {
    return other.count() - count();
  }

  friend constexpr difference_type operator-
    (const std::default_sentinel_t, const mask_iterator& rhs) noexcept {
    return rhs.count();
  }

  friend constexpr difference_type operator-
    (const mask_iterator& lhs, const std::default_sentinel_t) noexcept {
    return -lhs.count();
  }

private:
  T mask_ = 0;
};

template <integer T>
mask_iterator(T) -> mask_iterator<std::make_unsigned_t<T>>;

} // namespace nupp
