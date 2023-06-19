cmake_minimum_required(VERSION 3.23)

add_executable(nupp_tests
  algorithm_tests.cpp
  mask_iterator_tests.cpp
  pow2_tests.cpp
)
target_link_libraries(nupp_tests PRIVATE nupp gtest_main)

# Show all warnings because we're pedantic (and also all and extra)
target_compile_options(nupp_tests PRIVATE
  $<IF:$<BOOL:${MSVC}>, /W3, -Wall -Wpedantic -Wextra>
  $<$<CXX_COMPILER_ID:MSVC>:/bigobj>)

include(GoogleTest)
gtest_discover_tests(nupp_tests)
