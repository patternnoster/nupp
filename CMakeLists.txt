cmake_minimum_required(VERSION 3.23)

project(nupp LANGUAGES CXX)

option(NUPP_BUILD_TESTS "Build tests for this library" OFF)

include(GNUInstallDirs)
set(INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_library(nupp INTERFACE)
target_compile_features(nupp INTERFACE cxx_std_20)
target_include_directories(nupp INTERFACE
  $<BUILD_INTERFACE:${INCLUDE_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

if(NUPP_BUILD_TESTS)
  enable_testing()
  add_subdirectory(extern)
  add_subdirectory(test)
endif()

install(TARGETS nupp EXPORT nupp)
install(DIRECTORY ${INCLUDE_DIR}/nupp DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
install(EXPORT nupp
  FILE nuppConfig.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/nupp)
