cmake_minimum_required(VERSION 2.8)

project(KOALA Fortran)



set(CMAKE_Fortran_COMPILER      "mpif90")
set(CMAKE_Fortran_FLAGS   "-O0") # -lfftw3_mpi -lfftw3 -lm")
set(PATH_FFTW_LIB   /home/sato/lib/fftw/bin/lib)
set(PATH_FFTW_INC   /home/sato/lib/fftw/bin/include)

include_directories(${PATH_FFTW_LIB})
include_directories(${PATH_FFTW_INC})
link_directories(${PATH_FFTW_LIB})


set(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR})

set(TARGET_NAME   "koala")
set(MOD_LIB        module)
set(IO_LIB         io)
set(COMMON_LIB     common)
set(GS_LIB         gs)
set(RT_LIB         rt)

set(KOALA_SOURCES   src/main.f90)
file(GLOB MOD_SRC src/${MOD_LIB}/*.f90)
file(GLOB IO_SRC src/${IO_LIB}/*.f90)
file(GLOB COMMON_SRC src/${COMMON_LIB}/*.f90)
file(GLOB GS_SRC src/${GS_LIB}/*.f90)
file(GLOB RT_SRC src/${RT_LIB}/*.f90)
list(APPEND KOALA_SOURCES 
            ${MOD_SRC}
            ${IO_SRC}
            ${COMMON_SRC}
            ${GS_SRC}
            ${RT_SRC}
)

add_executable(${TARGET_NAME} ${KOALA_SOURCES})
target_link_libraries(${TARGET_NAME}  fftw3_mpi fftw3)