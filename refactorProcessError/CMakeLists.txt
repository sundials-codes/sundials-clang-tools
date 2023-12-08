cmake_minimum_required(VERSION 3.10)
project(refactor-process-error)

# specify the C++ standard
set(CMAKE_CXX_STANDARD 17)

# find LLVM and Clang
find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

# include LLVM and Clang headers
include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})

# add LLVM and Clang definitions
add_definitions(${LLVM_DEFINITIONS} ${CLANG_DEFINITIONS})

# specify the source files
set(SOURCE_FILES refactorProcessError.cpp)

# add the executable
add_executable(refactor-process-error ${SOURCE_FILES})

# link LLVM and Clang libraries
llvm_map_components_to_libnames(LLVM_LIBS support core irreader)
target_link_libraries(refactor-process-error ${LLVM_LIBS} clangTooling clangASTMatchers)
