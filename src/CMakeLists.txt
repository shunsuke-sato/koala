cmake_minimum_required(VERSION 2.8)

project(KOALA Fortran)


set(CMAKE_Fortran_COMPILER      "mpiifort")
set(CMAKE_Fortran_FLAGS   "-O2 -qopenmp")

set (LIBRARY_OUTPUT_PATH           ${PROJECT_BINARY_DIR})
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR})

set(TARGET_NAME   "koala")
set(MOD_LIB        module)


add_subdirectory(${MOD_LIB}) 

add_executable(${TARGET_NAME} main.f90)
add_dependencies(${TARGET_NAME} ${MOD_LIB})

target_link_libraries(${TARGET_NAME} ${MOD_LIB})
