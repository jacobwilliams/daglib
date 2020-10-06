cmake_minimum_required(VERSION 3.15)

project(DAG
  VERSION "1.0.0"
  LANGUAGES Fortran
  DESCRIPTION "Directed Acyclic Graph Library"
)

file    (READ ${CMAKE_CURRENT_LIST_DIR}/media/text-art.txt LOGO)
string  (JOIN "\n " GREETING "${LOGO}" "Version: ${DAG_VERSION}\n Build type: ${CMAKE_BUILD_TYPE}\n")
message (STATUS "\n${GREETING}")

if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
  message(FATAL_ERROR
    "\n"
    "Error: PROJECT_SOURCE_DIR matches PROJECT_BINARY_DIR: ${PROJECT_BINARY_DIR}\n"
    "This project does not support identical build and source paths.\n"
    "Please delete ${PROJECT_SOURCE_DIR}/CMakeCache.txt and ${PROJECT_SOURCE_DIR}/CMakeFiles/"
    "and then build in a different directory.  This may be accomplished in a bash shell by executing\n"
    "  rm -r CMakeCache.txt CMakeFiles/\n"
    "  mkdir build\n"
    "  cd build\n"
    "  export CC=gcc FC=gfortran\n"
    "  cmake .. \n"
    "or by substituting the appropriate syntax for shells other than bash."
  )
endif()

set (ALLOWED_BUILD_TYPES "Debug" "Release")
get_property(multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(multi_config)
  set (CMAKE_CONFIGURATION_TYPES "${ALLOWED_BUILD_TYPES}" CACHE STRING "list of supported configuration types" FORCE)
else()
  set (CMAKE_BUILD_TYPE "Debug" CACHE STRING "Select which configuration to build.")
  set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${ALLOWED_BUILD_TYPES})
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -std=f2018")
  get_filename_component(barename ${CMAKE_Fortran_COMPILER} NAME)
  if("${barename}" MATCHES gfortran)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcoarray=single")
  endif()
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include")

find_package(GFTL REQUIRED)
find_package(GFTL_SHARED REQUIRED VERSION 1.0.4)
find_package(YAFYAML REQUIRED)

include(CheckFortranSourceCompiles)

check_fortran_source_compiles("
  program main
  integer :: i
  i = 0
  error stop i
  end program
"
  HAVE_NON_CONSTANT_ERROR_STOP
  SRC_EXT ".f90")
if(HAVE_NON_CONSTANT_ERROR_STOP)
  add_definitions(-DHAVE_NON_CONSTANT_ERROR_STOP)
endif()

check_fortran_source_compiles("
  program main
  contains
  pure function foo() result(res)
    error stop 'Error stop is supported in pure functions (F2018)'
  end function
  end program
"
  HAVE_ERROR_STOP_IN_PURE
  SRC_EXT ".f90"
  )
if(HAVE_ERROR_STOP_IN_PURE)
  add_definitions(-DHAVE_ERROR_STOP_IN_PURE)
endif()

foreach(directory src tests)
  add_subdirectory("${directory}")
endforeach()

enable_testing()

function(add_caf_test name type num_images)
  set(test_dir "${PROJECT_BINARY_DIR}/tests/${type}/${name}")
  add_test( NAME "${name}"
    COMMAND cafrun -n "${num_images}" "${test_dir}/test-${name}"
    WORKING_DIRECTORY "${test_dir}"
  )
  set_tests_properties(${name}
     PROPERTIES PASS_REGULAR_EXPRESSION "Test passed."
  )
endfunction()

foreach(test_name
  jacob-example
  multiple-independents
)
  add_caf_test("${test_name}" "integration" 2)
endforeach()

foreach(test_name
  input-output
)
  add_caf_test("${test_name}" "unit" 2)
endforeach()
