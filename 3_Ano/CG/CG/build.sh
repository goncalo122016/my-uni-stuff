#!/bin/bash

# Build script for engine_system
# This script handles the build process with proper flags for Arch Linux

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
BUILD_TYPE="Release"
CLEAN_BUILD=false
PARALLEL_JOBS=$(nproc)

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -d|--debug)
            BUILD_TYPE="Debug"
            shift
            ;;
        -c|--clean)
            CLEAN_BUILD=true
            shift
            ;;
        -j|--jobs)
            PARALLEL_JOBS="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [options]"
            echo "Options:"
            echo "  -d, --debug       Build with debug symbols"
            echo "  -c, --clean       Clean build directory before building"
            echo "  -j, --jobs N      Number of parallel jobs (default: $(nproc))"
            echo "  -h, --help        Show this help message"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo -e "${YELLOW}Building engine_system...${NC}"
echo "Build type: $BUILD_TYPE"
echo "Parallel jobs: $PARALLEL_JOBS"

# Always clean CMakeCache.txt to avoid stale configuration
if [ -f "build/CMakeCache.txt" ]; then
    echo -e "${YELLOW}Cleaning stale CMake cache...${NC}"
    rm -f build/CMakeCache.txt
    rm -rf build/CMakeFiles
fi

# Clean build directory if requested
if [ "$CLEAN_BUILD" = true ]; then
    echo -e "${YELLOW}Cleaning build directory...${NC}"
    rm -rf build
fi

# Create build directory if it doesn't exist
mkdir -p build
cd build

# Configure with CMake
echo -e "${YELLOW}Configuring with CMake...${NC}"
cmake \
    -DCMAKE_CXX_FLAGS="-static-libgcc" \
    -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
    -DCMAKE_TOOLCHAIN_FILE="../vcpkg/scripts/buildsystems/vcpkg.cmake" \
    -DVCPKG_TARGET_TRIPLET=x64-linux \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    ..

# Exportar para o root do projeto para o IDE detetar
ln -sf compile_commands.json ../compile_commands.json

# Build with make
echo -e "${YELLOW}Building...${NC}"
make -j"$PARALLEL_JOBS"

echo -e "${GREEN}Build completed successfully!${NC}"
echo "Binaries located at:"
echo "  - engine/system (main executable)"
echo "  - generator/generator (generator utility)"

# Return to original directory
cd ..
