cmake_minimum_required(VERSION 3.23)

project(nupp LANGUAGES CXX)

option(NUPP_BUILD_TESTS "Build tests for this library" OFF)

set(INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_library(nupp INTERFACE)
target_compile_features(nupp INTERFACE cxx_std_20)
target_include_directories(nupp INTERFACE
  $<BUILD_INTERFACE:${INCLUDE_DIR}>)

if(NUPP_BUILD_TESTS)
  enable_testing()
  add_subdirectory(extern)
  add_subdirectory(test)
endif()
