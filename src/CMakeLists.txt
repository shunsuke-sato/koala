#cmake_minimum_required(VERSION 2.8)
cmake_minimum_required(VERSION 3.0)

project(KOALA Fortran)

set(CMAKE_Fortran_COMPILER      "mpif90")
set(CMAKE_Fortran_FLAGS   "-O0 ")

set(TARGET_NAME   "koala")
set(MOD_LIB        module)
set(IO_LIB         io)
set(COMMON_LIB     common)
set(GS_LIB         gs)
set(RT_LIB         rt)

set(KOALA_SOURCES   main.f90)
file(GLOB MOD_SRC ${MOD_LIB}/*.f90)
file(GLOB IO_SRC ${IO_LIB}/*.f90)
file(GLOB COMMON_SRC ${COMMON_LIB}/*.f90)
file(GLOB GS_SRC ${GS_LIB}/*.f90)
file(GLOB RT_SRC ${RT_LIB}/*.f90)
list(APPEND KOALA_SOURCES 
            ${MOD_SRC}
            ${IO_SRC}
            ${COMMON_SRC}
            ${GS_SRC}
            ${RT_SRC}
)

add_executable(${TARGET_NAME} ${KOALA_SOURCES})
