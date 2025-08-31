# Install script for directory: /home/ert/code/fortfront/cmake_test/_deps/json-fortran-src

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "0")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set path to fallback-tool for dependency-resolution.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  list(APPEND CMAKE_ABSOLUTE_DESTINATION_FILES
   "/usr/local/jsonfortran-gnu-8.3.0/doc/")
  if(CMAKE_WARN_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(WARNING "ABSOLUTE path INSTALL DESTINATION : ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  endif()
  if(CMAKE_ERROR_ON_ABSOLUTE_INSTALL_DESTINATION)
    message(FATAL_ERROR "ABSOLUTE path INSTALL DESTINATION forbidden (by caller): ${CMAKE_ABSOLUTE_DESTINATION_FILES}")
  endif()
  file(INSTALL DESTINATION "/usr/local/jsonfortran-gnu-8.3.0/doc" TYPE DIRECTORY FILES "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/doc/")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  foreach(file
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib/libjsonfortran.so.8.3.0"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib/libjsonfortran.so.8.3"
      )
    if(EXISTS "${file}" AND
       NOT IS_SYMLINK "${file}")
      file(RPATH_CHECK
           FILE "${file}"
           RPATH "")
    endif()
  endforeach()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib" TYPE SHARED_LIBRARY FILES
    "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/lib/libjsonfortran.so.8.3.0"
    "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/lib/libjsonfortran.so.8.3"
    )
  foreach(file
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib/libjsonfortran.so.8.3.0"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib/libjsonfortran.so.8.3"
      )
    if(EXISTS "${file}" AND
       NOT IS_SYMLINK "${file}")
      if(CMAKE_INSTALL_DO_STRIP)
        execute_process(COMMAND "/usr/bin/strip" "${file}")
      endif()
    endif()
  endforeach()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib" TYPE SHARED_LIBRARY FILES "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/lib/libjsonfortran.so")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib" TYPE STATIC_LIBRARY FILES "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/lib/libjsonfortran.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(GLOB_RECURSE MODULE_FILES "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/include/*.mod")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(GLOB_RECURSE SUBMOD_FILES "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/include/*.smod")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL ${MODULE_FILES} DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL ${SUBMOD_FILES} DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake/jsonfortran-gnu-targets.cmake")
    file(DIFFERENT _cmake_export_file_changed FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake/jsonfortran-gnu-targets.cmake"
         "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/CMakeFiles/Export/12f567646e65c05937922d4fe92037a8/jsonfortran-gnu-targets.cmake")
    if(_cmake_export_file_changed)
      file(GLOB _cmake_old_config_files "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake/jsonfortran-gnu-targets-*.cmake")
      if(_cmake_old_config_files)
        string(REPLACE ";" ", " _cmake_old_config_files_text "${_cmake_old_config_files}")
        message(STATUS "Old export file \"$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake/jsonfortran-gnu-targets.cmake\" will be replaced.  Removing files [${_cmake_old_config_files_text}].")
        unset(_cmake_old_config_files_text)
        file(REMOVE ${_cmake_old_config_files})
      endif()
      unset(_cmake_old_config_files)
    endif()
    unset(_cmake_export_file_changed)
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake" TYPE FILE FILES "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/CMakeFiles/Export/12f567646e65c05937922d4fe92037a8/jsonfortran-gnu-targets.cmake")
  if(CMAKE_INSTALL_CONFIG_NAME MATCHES "^()$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake" TYPE FILE FILES "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/CMakeFiles/Export/12f567646e65c05937922d4fe92037a8/jsonfortran-gnu-targets-noconfig.cmake")
  endif()
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/cmake" TYPE FILE FILES
    "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/pkg/jsonfortran-gnu-config.cmake"
    "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/jsonfortran-gnu-config-version.cmake"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/jsonfortran-gnu-8.3.0/lib/pkgconfig" TYPE FILE FILES "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/json-fortran.pc")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
if(CMAKE_INSTALL_LOCAL_ONLY)
  file(WRITE "/home/ert/code/fortfront/cmake_test/_deps/json-fortran-build/install_local_manifest.txt"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
endif()
