# Custom toolchain for Arch Linux
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR x86_64)

# Specify the C and C++ compilers
set(CMAKE_C_COMPILER /usr/bin/gcc)
set(CMAKE_CXX_COMPILER /usr/bin/g++)

# Get the GCC version and architecture
execute_process(COMMAND gcc -dumpmachine OUTPUT_VARIABLE GCC_MACHINE OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND gcc -dumpversion OUTPUT_VARIABLE GCC_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE)

# Set compiler flags
set(CMAKE_C_FLAGS "-shared-libgcc -pthread" CACHE STRING "C flags" FORCE)
set(CMAKE_CXX_FLAGS "-shared-libgcc -pthread" CACHE STRING "C++ flags" FORCE)

# Linker flags
set(CMAKE_SHARED_LINKER_FLAGS "-shared-libgcc -pthread -L/usr/lib -L/usr/lib/${GCC_MACHINE}")
set(CMAKE_EXE_LINKER_FLAGS "-shared-libgcc -pthread -L/usr/lib -L/usr/lib/${GCC_MACHINE}")

# Mark compilers as working
set(CMAKE_C_COMPILER_WORKS TRUE CACHE BOOL "")
set(CMAKE_CXX_COMPILER_WORKS TRUE CACHE BOOL "")

# Compiler identification
set(CMAKE_C_COMPILER_ID GNU)
set(CMAKE_CXX_COMPILER_ID GNU)
set(CMAKE_C_COMPILER_VERSION 15.2)
set(CMAKE_CXX_COMPILER_VERSION 15.2)

# Thread support
set(Threads_FOUND TRUE CACHE BOOL "")
set(CMAKE_HAVE_LIBC_PTHREAD ON)
set(CMAKE_THREAD_LIBS_INIT "-pthread")
set(CMAKE_USE_PTHREADS_INIT ON)

# Set explicit include directories for C and C++
set(CMAKE_C_STANDARD_INCLUDE_DIRECTORIES "/usr/include")
set(CMAKE_CXX_STANDARD_INCLUDE_DIRECTORIES "/usr/include;/usr/include/c++/15.2.1;/usr/include/${GCC_MACHINE}/c++/15.2.1")

# Add prefix for system includes
include_directories(SYSTEM /usr/include)
