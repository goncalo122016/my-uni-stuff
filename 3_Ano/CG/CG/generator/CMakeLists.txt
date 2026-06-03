include_directories(${CMAKE_SOURCE_DIR}/common)

add_executable(generator src/main.cpp ../common/Vec.hpp
        src/Generator.cpp
        src/Generator.hpp
        ../common/Mat.hpp
        ../common/Mat.cpp)
