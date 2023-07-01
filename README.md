## The nupp library

A collection of C++20 numeric tools and templates. Still work in progress.

### Installation

The library is header-only. To install the headers along with the CMake configuration file (for `find_package`) one can use the standard CMake procedure:
```sh
cmake $SOURCE_PATH
cmake --install . --prefix=$INSTALL_PATH
```

### Example

A working examples of some of the tools provided by the library so far:
```c++
#include <nupp/algorithm.hpp>
#include <nupp/mask_iterator.hpp>
#include <nupp/pow2_t.hpp>

nupp::pow2_t p1{16};
nupp::pow2_t p2{42, nupp::pow2_t::ceil}; // equals 64
auto l2 = (p2 / p1).log2();              // equals 2, shift used

const int mask = p1 | p2;
std::ranges::subrange range{nupp::mask_iterator(mask),
                            std::default_sentinel};
for (nupp::pow2_t bit : range) {
  /* iterates through the 2 set bits... */
}

nupp::minimum(-1, p1, uint64_t(42), p2, int32_t(-17)); // returns int32_t(-17)
nupp::maximum(p2, -120, uint64_t(500), 42);            // returns uint64_t(500)
```

### Running the tests

Use the `NUPP_BUILD_TESTS=ON` CMake option to build the library tests on your system (requires the googletest submodule). One can run the following commands (in the source directory) to build and run the tests:
```sh
mkdir build && cd build
cmake .. -DNUPP_BUILD_TESTS=ON
cmake --build .
ctest
```
