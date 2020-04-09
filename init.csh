#!/bin/csh
#
if ( $1 == "GCC" ) then
    if ( `caf -v |& head -1 | awk '{print substr($0,1,4)}'` == "caf:" ) then
	CAF_COMPILER
	CMAKE
    endif
    setenv FC  `which gfortran`
    setenv CC  `which gcc`
    setenv CXX `which g++`
    set compiler=gcc
else if ( $1 == "Intel" ) then
    if ( `ifort -v |& head -1 | awk '{print substr($0,1,6)}'` == "ifort:" ) then
	INTEL_COMPILER
    endif
    if ( `cmake --version |& head -1 | awk '{print substr($0,14,2)}'` == "2" ) then
	CMAKE
    endif
    setenv FC  `which ifort`
    setenv CC  `which icc`
    setenv CXX `which icpc`
    set compiler=intel
else
    echo "Either need to use -GCC- or -Intel- as the first argument"
    exit
endif
#
if ( -e daglib/build-${compiler} ) then
    rm -rf daglib/build-${compiler}
endif
#
mkdir -p daglib/build-${compiler}
cdn daglib/build-${compiler}
#
if ( $?2 ) then
    cmake --verbose --log-level=VERBOSE ../..
else
    cmake ../..
endif
#
make
#
ctest
