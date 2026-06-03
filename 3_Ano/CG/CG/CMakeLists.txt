cmake_minimum_required(VERSION 3.22)

project(engine_system LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)

# Add vcpkg packages path
set(VCPKG_ROOT "$ENV{HOME}/vcpkg")
set(VCPKG_TRIPLET "x64-linux")
set(CMAKE_PREFIX_PATH "${VCPKG_ROOT}/packages/imgui_${VCPKG_TRIPLET}"
                      "${VCPKG_ROOT}/packages/glfw3_${VCPKG_TRIPLET}"
                      "${VCPKG_ROOT}/packages/glew_${VCPKG_TRIPLET}"
                      "${VCPKG_ROOT}/packages/tinyxml2_${VCPKG_TRIPLET}")

# Add include paths from vcpkg
include_directories(
    "${VCPKG_ROOT}/packages/imgui_${VCPKG_TRIPLET}/include"
    "${VCPKG_ROOT}/packages/glfw3_${VCPKG_TRIPLET}/include"
    "${VCPKG_ROOT}/packages/glew_${VCPKG_TRIPLET}/include"
    "${VCPKG_ROOT}/packages/tinyxml2_${VCPKG_TRIPLET}/include"
)

# Add lib paths from vcpkg
link_directories(
    "${VCPKG_ROOT}/packages/imgui_${VCPKG_TRIPLET}/lib"
    "${VCPKG_ROOT}/packages/glfw3_${VCPKG_TRIPLET}/lib"
    "${VCPKG_ROOT}/packages/glew_${VCPKG_TRIPLET}/lib"
    "${VCPKG_ROOT}/packages/tinyxml2_${VCPKG_TRIPLET}/lib"
)

add_subdirectory(engine)
add_subdirectory(generator)
