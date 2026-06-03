include_directories(${CMAKE_SOURCE_DIR}/common)

file(GLOB_RECURSE ENGINE_SOURCES src/*.cpp src/*.hpp)
file(GLOB COMMON_SOURCES ${CMAKE_SOURCE_DIR}/common/*.cpp ${CMAKE_SOURCE_DIR}/common/*.hpp)

add_executable(system
    ${ENGINE_SOURCES}
    ${COMMON_SOURCES}
)

find_package(OpenGL REQUIRED)
target_link_libraries(system PUBLIC ${OPENGL_LIBRARIES})
target_include_directories(system PUBLIC ${OPENGL_INCLUDE_DIR})

find_package(glfw3 CONFIG REQUIRED)
target_link_libraries(system PRIVATE glfw)

find_package(imgui CONFIG REQUIRED)
target_link_libraries(system PRIVATE imgui::imgui)

find_package(tinyxml2 CONFIG REQUIRED)
target_link_libraries(system PRIVATE tinyxml2::tinyxml2)

find_package(GLEW REQUIRED)
target_link_libraries(system PRIVATE GLEW::GLEW)

find_package(ZLIB REQUIRED)
target_link_libraries(system PRIVATE ZLIB::ZLIB)
