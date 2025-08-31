# Adapted from http://www.cmake.org/Wiki/CMake_FAQ#Can_I_do_.22make_uninstall.22_with_CMake.3F May 1, 2014

if(NOT EXISTS "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/install_manifest.txt")
  message(FATAL_ERROR "Cannot find install manifest: /home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/install_manifest.txt")
endif(NOT EXISTS "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/install_manifest.txt")

file(READ "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/install_manifest.txt" files)
string(REGEX REPLACE "\n" ";" files "${files}")
foreach(file ${files})
  message(STATUS "Uninstalling $ENV{DESTDIR}${file}")
  if(IS_SYMLINK "$ENV{DESTDIR}${file}" OR EXISTS "$ENV{DESTDIR}${file}")
    exec_program(
      "/usr/bin/cmake" ARGS "-E remove \"$ENV{DESTDIR}${file}\""
      OUTPUT_VARIABLE rm_out
      RETURN_VALUE rm_retval
      )
    if(NOT "${rm_retval}" STREQUAL 0)
      message(FATAL_ERROR "Problem when removing $ENV{DESTDIR}${file}")
    endif(NOT "${rm_retval}" STREQUAL 0)
  else(IS_SYMLINK "$ENV{DESTDIR}${file}" OR EXISTS "$ENV{DESTDIR}${file}")
    message(STATUS "File $ENV{DESTDIR}${file} does not exist.")
  endif(IS_SYMLINK "$ENV{DESTDIR}${file}" OR EXISTS "$ENV{DESTDIR}${file}")
endforeach(file)
