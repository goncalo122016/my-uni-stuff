#ifndef GENERATOR_H
#define GENERATOR_H
#include <vector>

#include "Vec.hpp"

namespace generator
{
    std::vector<Vertex> GeneratePlane(float length, size_t divisions);
    std::vector<Vertex> GenerateSphere(float radius, size_t slices, size_t stacks);
    std::vector<Vertex> GenerateCone(float radius, float height, size_t slices, size_t stacks);
    std::vector<Vertex> GenerateBox(float length, size_t divisions);
    std::vector<Vertex> GenerateCylinder(float radius, float height, size_t slices, size_t stacks);
    std::vector<Vertex> GenerateTorus(float radius, float tubeRadius, size_t slices, size_t stacks);
    std::vector<Vertex> GenerateIcosphere(float radius, size_t subdivisions);
    std::vector<Vertex> GeneratePatch(const char* filename, int tessellation);
}

#endif // GENERATOR_H
