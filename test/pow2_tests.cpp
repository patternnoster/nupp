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

  // Check construction and assignment
  pow2_t x{1}, y{16};
  x = y;
  y = pow2_t{128};
  EXPECT_EQ(x.value, 16);
  EXPECT_EQ(y.value, 128);
  EXPECT_EQ(std::max(x, y).value, y.value);
}
