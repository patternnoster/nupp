#include <concepts>
#include <cstdint>
#include <gtest/gtest.h>
#include <iterator>
#include <ranges>

#include "nupp/mask_iterator.hpp"

using namespace nupp;
namespace ranges = std::ranges;

TEST(MaskIteratorTests, basic_ranges) {
  const uint64_t m1 = 1 | 64 | 4096;
  const ranges::subrange sr1{ mask_iterator{m1},
                              std::default_sentinel };

  EXPECT_EQ(ranges::size(sr1), 3);
  uint64_t rm1 = 0;
  for (auto p : sr1) {
    EXPECT_TRUE((std::same_as<decltype(p), pow2_t>));
    rm1|= p;
  }

  EXPECT_EQ(rm1, m1);

  const ranges::subrange sr2{ mask_iterator(-1),
                              std::default_sentinel };
  EXPECT_EQ(ranges::size(sr2), sizeof_bit<int>);
  int rm2 = 0;
  for (auto p : sr2) rm2|= p;

  EXPECT_EQ(rm2, -1);

  const ranges::subrange sr3{ mask_iterator{uint32_t(0)},
                              mask_iterator<uint32_t>{} };
  EXPECT_TRUE(ranges::empty(sr3));
  EXPECT_EQ(std::begin(sr3), std::default_sentinel);
}

TEST(MaskIteratorTests, randomized) {
  constexpr size_t RandomTries = 1000;

  constexpr static auto test_range = []<typename I, typename S>
    (const auto mask, const I begin, const S end) {
    EXPECT_TRUE((std::input_or_output_iterator<I>));
    EXPECT_TRUE((std::sized_sentinel_for<S, I>));

    const auto size = end - begin;
    EXPECT_GE(size, 0);
    EXPECT_EQ((begin - end), -size);

    if (size == 0) { EXPECT_EQ(begin, end); }
    if (begin == end) { EXPECT_EQ(size, 0); }

    constexpr bool is_iterator_end = std::input_or_output_iterator<S>;

    size_t result = 0;
    size_t cnt = 0;
    for (auto it = begin; it != end; ++it) {
      ++cnt;
      result+= (*it).value;

      EXPECT_GE(it, begin);
      if constexpr (is_iterator_end) {
        EXPECT_LT(it, end);
        EXPECT_GT(end, it);
      }

      EXPECT_NE(it, std::default_sentinel);
      EXPECT_EQ(((it - begin) + (end - it)), size);
    }

    EXPECT_EQ(size, cnt);

    if constexpr (is_iterator_end) {
      if (end != std::default_sentinel) { EXPECT_NE(*end, 0); }
      EXPECT_EQ((begin.count() - end.count()), size);
    }
    else
      EXPECT_EQ(begin.count(), size) << mask;

    EXPECT_EQ(result, mask);
  };

  constexpr static auto test_mask = []<typename T>(const T mask) {
    mask_iterator begin{mask};
    decltype(begin) end{};

    test_range(mask, begin, end);
    test_range(mask, begin, std::default_sentinel);
  };

  constexpr auto test_typed_mask = [](const auto mask) {
    test_mask(uint64_t(mask));
    test_mask(uint32_t(mask));
    test_mask(uint16_t(mask));
    test_mask(uint8_t(mask));
  };

  test_typed_mask(0);
  test_typed_mask(uint64_t(-1));

  for (size_t i = 0; i < RandomTries; ++i) {
    test_typed_mask(i);
    test_typed_mask(rand());
  }
}
