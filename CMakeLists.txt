cmake_minimum_required(VERSION 3.15)

set (DAGLIB_VERSION "1.0")
set (ALLOWED_BUILD_TYPES "Debug" "Release")

get_property(multi_config GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
if(multi_config)
  set (CMAKE_CONFIGURATION_TYPES "${ALLOWED_BUILD_TYPES}" CACHE STRING "list of supported configuration types" FORCE)
else()
  set (CMAKE_BUILD_TYPE "Debug" CACHE STRING "Select which configuration to build.")
  set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS ${ALLOWED_BUILD_TYPES})
endif()

file    (READ ${CMAKE_CURRENT_LIST_DIR}/media/text-art.txt LOGO)
string  (JOIN "\n " GREETING "${LOGO}" "Version: ${DAGLIB_VERSION}\n Build type: ${CMAKE_BUILD_TYPE}\n")
message (STATUS "\n${GREETING}")

project(daglib
  VERSION "${DAGLIB_VERSION}"
  LANGUAGES Fortran
  DESCRIPTION "Directed Acyclic Graph Library"
)

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

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -std=f2018")
  get_filename_component(barename ${CMAKE_Fortran_COMPILER} NAME)
  if("${barename}" MATCHES gfortran)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcoarray=single")
  endif()
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/mod")

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

include(ExternalProject)
set(installDir ${CMAKE_CURRENT_BINARY_DIR}/install)

ExternalProject_Add(gftl-shared
 SOURCE_DIR ${PROJECT_SOURCE_DIR}/src/git-submodules/gftl-shared
 INSTALL_DIR ${installDir}
 CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
            -DCMAKE_PREFIX_PATH:PATH=<INSTALL_DIR>
)

ExternalProject_Add(gftl
 SOURCE_DIR ${PROJECT_SOURCE_DIR}/src/git-submodules/gftl
 INSTALL_DIR ${installDir}
 CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
            -DCMAKE_PREFIX_PATH:PATH=<INSTALL_DIR>
)

ExternalProject_Add(yaFyaml
 SOURCE_DIR ${PROJECT_SOURCE_DIR}/src/git-submodules/yaFyaml
 INSTALL_DIR ${installDir}
 CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
            -DCMAKE_PREFIX_PATH:PATH=<INSTALL_DIR>
)

ExternalProject_Add_StepDependencies(yaFyaml configure gftl gftl-shared)
