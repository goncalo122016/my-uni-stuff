
if (NOT EXISTS "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/glfw3/x64-linux-dbg/install_manifest.txt")
  message(FATAL_ERROR "Cannot find install manifest: \"/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/glfw3/x64-linux-dbg/install_manifest.txt\"")
endif()

file(READ "/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/buildtrees/glfw3/x64-linux-dbg/install_manifest.txt" files)
string(REGEX REPLACE "\n" ";" files "${files}")

foreach (file ${files})
  message(STATUS "Uninstalling \"$ENV{DESTDIR}${file}\"")
  if (EXISTS "$ENV{DESTDIR}${file}")
    exec_program("/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/downloads/tools/cmake-3.31.10-linux/cmake-3.31.10-linux-x86_64/bin/cmake" ARGS "-E remove \"$ENV{DESTDIR}${file}\""
                 OUTPUT_VARIABLE rm_out
                 RETURN_VALUE rm_retval)
    if (NOT "${rm_retval}" STREQUAL 0)
      MESSAGE(FATAL_ERROR "Problem when removing \"$ENV{DESTDIR}${file}\"")
    endif()
  elseif (IS_SYMLINK "$ENV{DESTDIR}${file}")
    EXEC_PROGRAM("/home/goncalo/UMinho/3_Ano/CG/CG/vcpkg/downloads/tools/cmake-3.31.10-linux/cmake-3.31.10-linux-x86_64/bin/cmake" ARGS "-E remove \"$ENV{DESTDIR}${file}\""
                 OUTPUT_VARIABLE rm_out
                 RETURN_VALUE rm_retval)
    if (NOT "${rm_retval}" STREQUAL 0)
      message(FATAL_ERROR "Problem when removing symlink \"$ENV{DESTDIR}${file}\"")
    endif()
  else()
    message(STATUS "File \"$ENV{DESTDIR}${file}\" does not exist.")
  endif()
endforeach()

