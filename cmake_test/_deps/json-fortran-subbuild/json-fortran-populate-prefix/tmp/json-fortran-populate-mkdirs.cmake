# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file LICENSE.rst or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION ${CMAKE_VERSION}) # this file comes with cmake

# If CMAKE_DISABLE_SOURCE_CHANGES is set to true and the source directory is an
# existing directory in our source tree, calling file(MAKE_DIRECTORY) on it
# would cause a fatal error, even though it would be a no-op.
if(NOT EXISTS "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-src")
  file(MAKE_DIRECTORY "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-src")
endif()
file(MAKE_DIRECTORY
  "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build"
  "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-subbuild/json-fortran-populate-prefix"
  "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-subbuild/json-fortran-populate-prefix/tmp"
  "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-subbuild/json-fortran-populate-prefix/src/json-fortran-populate-stamp"
  "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-subbuild/json-fortran-populate-prefix/src"
  "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-subbuild/json-fortran-populate-prefix/src/json-fortran-populate-stamp"
)

set(configSubDirs )
foreach(subDir IN LISTS configSubDirs)
    file(MAKE_DIRECTORY "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-subbuild/json-fortran-populate-prefix/src/json-fortran-populate-stamp/${subDir}")
endforeach()
if(cfgdir)
  file(MAKE_DIRECTORY "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-subbuild/json-fortran-populate-prefix/src/json-fortran-populate-stamp${cfgdir}") # cfgdir has leading slash
endif()
