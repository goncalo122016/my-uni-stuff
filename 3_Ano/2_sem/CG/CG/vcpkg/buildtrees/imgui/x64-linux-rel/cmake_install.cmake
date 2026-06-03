# Install script for directory: /home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/src/v1.90.2-4442117b09.clean

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/packages/imgui_x64-linux")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "OFF")
endif()

# Set path to fallback-tool for dependency-resolution.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/x64-linux-rel/libimgui.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES
    "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/src/v1.90.2-4442117b09.clean/imgui.h"
    "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/src/v1.90.2-4442117b09.clean/imconfig.h"
    "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/src/v1.90.2-4442117b09.clean/imgui_internal.h"
    "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/src/v1.90.2-4442117b09.clean/imstb_textedit.h"
    "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/src/v1.90.2-4442117b09.clean/imstb_rectpack.h"
    "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/src/v1.90.2-4442117b09.clean/imstb_truetype.h"
    "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/src/v1.90.2-4442117b09.clean/misc/cpp/imgui_stdlib.h"
    )
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/src/v1.90.2-4442117b09.clean/backends/imgui_impl_glfw.h")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/src/v1.90.2-4442117b09.clean/backends/imgui_impl_opengl2.h")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/imgui" TYPE FILE FILES "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/x64-linux-rel/imgui-config.cmake")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/imgui/imgui-targets.cmake")
    file(DIFFERENT _cmake_export_file_changed FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/imgui/imgui-targets.cmake"
         "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/x64-linux-rel/CMakeFiles/Export/791eb3786aa5a66c7063a763bc360501/imgui-targets.cmake")
    if(_cmake_export_file_changed)
      file(GLOB _cmake_old_config_files "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/imgui/imgui-targets-*.cmake")
      if(_cmake_old_config_files)
        string(REPLACE ";" ", " _cmake_old_config_files_text "${_cmake_old_config_files}")
        message(STATUS "Old export file \"$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/imgui/imgui-targets.cmake\" will be replaced.  Removing files [${_cmake_old_config_files_text}].")
        unset(_cmake_old_config_files_text)
        file(REMOVE ${_cmake_old_config_files})
      endif()
      unset(_cmake_old_config_files)
    endif()
    unset(_cmake_export_file_changed)
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/imgui" TYPE FILE FILES "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/x64-linux-rel/CMakeFiles/Export/791eb3786aa5a66c7063a763bc360501/imgui-targets.cmake")
  if(CMAKE_INSTALL_CONFIG_NAME MATCHES "^([Rr][Ee][Ll][Ee][Aa][Ss][Ee])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/imgui" TYPE FILE FILES "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/x64-linux-rel/CMakeFiles/Export/791eb3786aa5a66c7063a763bc360501/imgui-targets-release.cmake")
  endif()
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
if(CMAKE_INSTALL_LOCAL_ONLY)
  file(WRITE "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/x64-linux-rel/install_local_manifest.txt"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
endif()
if(CMAKE_INSTALL_COMPONENT)
  if(CMAKE_INSTALL_COMPONENT MATCHES "^[a-zA-Z0-9_.+-]+$")
    set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
  else()
    string(MD5 CMAKE_INST_COMP_HASH "${CMAKE_INSTALL_COMPONENT}")
    set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INST_COMP_HASH}.txt")
    unset(CMAKE_INST_COMP_HASH)
  endif()
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  file(WRITE "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/imgui/x64-linux-rel/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
endif()
