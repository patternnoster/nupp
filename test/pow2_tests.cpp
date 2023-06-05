#include <cstdint>
#include <gtest/gtest.h>

#include "nupp/pow2_t.hpp"

using namespace nupp;

template <pow2_t _val>
struct pow2_test_struct {
  uint64_t get() const noexcept {
    return _val;
  }
};

TEST(Pow2Tests, construction) {
  // Make sure the type is structural
  const pow2_test_struct<pow2_t{1024}> t;
  EXPECT_EQ(t.get(), 1024);

  // Check construction
  const auto test_constr = [](const auto pow2) {
    if (pow2 > 1) {
      EXPECT_EQ((pow2_t{pow2 - 1, pow2_t::floor}), pow2 >> 1);
      EXPECT_EQ((pow2_t{pow2 + 1, pow2_t::floor}), pow2);
    }

    if (pow2 > 2) {
      EXPECT_EQ((pow2_t{pow2 - 1, pow2_t::ceil}), pow2);
    }

    EXPECT_EQ((pow2_t{pow2, pow2_t::exact}), pow2);

    if (static_cast<uint64_t>(pow2) < (uint64_t(1) << 63)) {
      EXPECT_EQ((pow2_t{pow2 + 1, pow2_t::ceil}),
                uint64_t(pow2) << 1);
    }
  };

  for (size_t i = 0; i < 64; ++i) {
    test_constr(uint64_t(1) << i);
    if (i < 63) test_constr(int64_t(1) << i);
    if (i < 32) test_constr(uint32_t(1) << i);
    if (i < 31) test_constr(int32_t(1) << i);
    if (i < 16) test_constr(uint16_t(1) << i);
    if (i < 15) test_constr(int16_t(1) << i);
    if (i < 8) test_constr(uint8_t(1) << i);
    if (i < 7) test_constr(int8_t(1) << i);
  }

  // Check assignment too
  pow2_t x{1}, y{16};
  x = y;
  y = pow2_t{128};
  EXPECT_EQ(x.value, 16);
  EXPECT_EQ(y.value, 128);
  EXPECT_EQ(std::max(x, y).value, y.value);
}

TEST(Pow2Tests, arithmetic) {
  // Check log2
  for (int i = 0; i < 64; ++i) {
    const auto val = uint64_t(1) << i;
    EXPECT_TRUE(is_pow2(val));
    EXPECT_EQ(pow2_t{val}.log2(), i);
  }

  // Check arithmetic
  const auto test_mod = [](const auto lhs, const pow2_t& rhs) {
    EXPECT_EQ((lhs % rhs), (lhs % rhs.value));
    EXPECT_EQ(((lhs - 1) % rhs), ((lhs - 1) % rhs.value));
    EXPECT_EQ(((lhs + 1) % rhs), ((lhs + 1) % rhs.value));
  };

  for (int i = 0; i < 64; ++i) {
    for (int j = 0; j < 64; ++j) {
      const pow2_t x{uint64_t(1) << i};
      const pow2_t y{uint64_t(1) << j};

      if ((i + j) < 64) {
        EXPECT_EQ((x << j).log2(), (i + j));
        EXPECT_EQ((x * y).log2(), (i + j));
      }

      if (i >= j) {
        EXPECT_EQ((x >> j).log2(), (i - j));
        EXPECT_EQ((x / y).log2(), (i - j));
        EXPECT_EQ((x % y), 0);
      }
      else {
        EXPECT_EQ((x % y), x.value);
      }

      test_mod(x.value, y);
      if (i < 8) test_mod(uint8_t(x.value), y);
      if (i < 16) test_mod(uint16_t(x.value), y);
      if (i < 32) test_mod(uint32_t(x.value), y);
    }
  }
}

TEST(Pow2Tests, concepts) {
  EXPECT_TRUE(integer<volatile int>);
  EXPECT_TRUE(integer<const char16_t>);
  EXPECT_FALSE(integer<pow2_t>);

  EXPECT_TRUE(extended_integer<int8_t>);
  EXPECT_TRUE(extended_integer<const pow2_t>);

  EXPECT_FALSE(integer<bool>);
  EXPECT_FALSE(unsigned_integer<bool>);
  EXPECT_FALSE(extended_integer<bool>);

  EXPECT_TRUE(extended_arithmetic<bool>);
  EXPECT_TRUE(extended_arithmetic<size_t>);
  EXPECT_TRUE(extended_arithmetic<double>);
  EXPECT_TRUE(extended_arithmetic<pow2_t>);

  EXPECT_TRUE((std::same_as<std::common_type_t<pow2_t, int>, uint64_t>));
  EXPECT_TRUE((std::same_as<std::common_type_t<pow2_t, double>, double>));
  EXPECT_TRUE((std::same_as<std::common_type_t<pow2_t, pow2_t>, pow2_t>));
  EXPECT_TRUE((std::same_as<std::common_type_t<float, pow2_t>, float>));
  EXPECT_TRUE((std::same_as<std::common_type_t<uint32_t, pow2_t>, uint64_t>));
}
