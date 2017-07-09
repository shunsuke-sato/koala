cmake_minimum_required(VERSION 2.8)

project(KOALA Fortran)


set(CMAKE_Fortran_COMPILER      "mpif90")
set(CMAKE_Fortran_FLAGS   "-O0")

set (LIBRARY_OUTPUT_PATH           ${PROJECT_BINARY_DIR})
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR})

set(TARGET_NAME   "koala")
set(MOD_LIB        module)
set(IO_LIB         inputoutput)


add_subdirectory(${MOD_LIB}) 
add_subdirectory(${IO_LIB}) 

add_executable(${TARGET_NAME} main.f90)
add_dependencies(${TARGET_NAME} ${MOD_LIB})
add_dependencies(${IO_LIB} ${MOD_LIB})
set(LIBRARIES
    ${MOD_LIB}
    ${IO_LIB}
    )

target_link_libraries(${TARGET_NAME} ${LIBRARIES})
