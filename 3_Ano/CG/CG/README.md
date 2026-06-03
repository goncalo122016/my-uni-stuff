# CG Engine System

A 3D graphics engine system built with OpenGL for rendering 3D scenes and generating geometric models.

## Prerequisites

- [vcpkg](https://vcpkg.io/en/getting-started) - Package manager for C++
- [CMake](https://cmake.org/download/) (version 3.25 or higher)
- C++20 compatible compiler
- OpenGL development libraries

### Installing vcpkg

If you don't have vcpkg installed:

```bash
git clone https://github.com/microsoft/vcpkg.git
cd vcpkg
./bootstrap-vcpkg.sh  # Linux/macOS
# or
.\bootstrap-vcpkg.bat  # Windows
```

Then run `vcpkg integrate install` to get the toolchain file path.

## Compiling

### Linux

#### Command Line

```bash
cd engine_system

# Configure and build
cmake -B build -S . -DCMAKE_TOOLCHAIN_FILE=[path to vcpkg]/scripts/buildsystems/vcpkg.cmake
cmake --build build

# Or use the one-liner (replace with your vcpkg path):
rm -rf build && mkdir build && cd build && \
cmake -DCMAKE_TOOLCHAIN_FILE=/home/afonso/vcpkg/scripts/buildsystems/vcpkg.cmake .. && \
make
```

**Note:** Replace `[path to vcpkg]` with your actual vcpkg installation path. Run `vcpkg integrate install` to find it.

#### With no IDE

```bash
cd engine_system
cmake -B build -S . -DCMAKE_TOOLCHAIN_FILE=[path to vcpkg]/scripts/buildsystems/vcpkg.cmake
cmake --build build
```

You can find the vcpkg path by running `vcpkg integrate install` and looking at the output.

## Running

**Important:** Always run the executables from the project root directory, not from the build directory.

### Engine (System)

The main rendering engine that loads and displays 3D scenes.

```bash
# From the project root
./build/engine/system assets/scenes/box.xml

# Windows
.\build\engine\Debug\system.exe assets\scenes\box.xml
```

#### Engine Features

- **Camera Controls**: Click and drag with left mouse button to rotate camera
- **Mouse Wheel**: Zoom in/out
- **UI Controls** (ImGui):
  - World settings (camera position, FOV, near/far planes)
  - Rendering settings:
    - VSync toggle
    - Cull Faces toggle
    - Wireframe mode toggle
    - Render Axis toggle
    - **Fullscreen toggle** - Switch between windowed and fullscreen mode
  - Environment information (GPU, OpenGL version, etc.)
  - FPS counter

### Generator

Tool for generating geometric 3D models and saving them to files.

```bash
# From the project root
./build/generator/generator <command> <arguments>

# Windows
.\build\generator\Debug\generator.exe <command> <arguments>
```

#### Generator Commands

Generate various 3D geometric shapes:

```bash
# Generate a plane
./build/generator/generator plane <length> <divisions> <output_file>

# Generate a box
./build/generator/generator box <length> <divisions> <output_file>

# Generate a sphere
./build/generator/generator sphere <radius> <slices> <stacks> <output_file>

# Generate a cone
./build/generator/generator cone <radius> <height> <slices> <stacks> <output_file>

# Generate a cylinder
./build/generator/generator cylinder <radius> <height> <slices> <stacks> <output_file>

# Generate a torus
./build/generator/generator torus <radius> <tubeRadius> <slices> <stacks> <output_file>

# Generate an icosphere
./build/generator/generator icosphere <radius> <subdivisions> <output_file>
```

#### Shape Generation Examples

**Basic Shapes:**
```bash
# Plane (5x5 units, 10 subdivisions)
./build/generator/generator plane 5.0 10 plane.3d

# Box/Cube (2x2x2 units, 8 subdivisions per face)
./build/generator/generator box 2.0 8 box.3d

# Sphere (radius 1, 20 slices, 20 stacks)
./build/generator/generator sphere 1.0 20 20 sphere.3d

# Cone (radius 1, height 2, 20 slices, 10 stacks)
./build/generator/generator cone 1.0 2.0 20 10 cone.3d
```

**Advanced Shapes:**
```bash
# Cylinder (radius 1, height 3, 24 slices, 12 stacks)
./build/generator/generator cylinder 1.0 3.0 24 12 cylinder.3d

# Torus/Donut (major radius 1.5, tube radius 0.4, 30x30 subdivisions)
./build/generator/generator torus 1.5 0.4 30 30 torus.3d

# Icosphere (radius 1, 3 subdivision levels)
./build/generator/generator icosphere 1.0 3 icosphere.3d
```

**High-Quality Shapes:**
```bash
# Smooth sphere with more detail
./build/generator/generator sphere 1.0 40 40 sphere_hq.3d

# Detailed torus
./build/generator/generator torus 2.0 0.5 60 60 torus_hq.3d

# High-detail icosphere
./build/generator/generator icosphere 1.0 4 icosphere_hq.3d
```

**Low-Poly Shapes:**
```bash
# Stylized low-poly sphere
./build/generator/generator sphere 1.0 8 8 sphere_lowpoly.3d

# Hexagonal cylinder
./build/generator/generator cylinder 1.0 2.0 6 1 cylinder_lowpoly.3d

# Basic icosphere
./build/generator/generator icosphere 1.0 1 icosphere_lowpoly.3d
```

**Generate All Default Shapes:**
```bash
# Create all basic shapes with standard quality
./build/generator/generator plane 5.0 10 plane.3d
./build/generator/generator box 2.0 8 box.3d
./build/generator/generator sphere 1.0 20 20 sphere.3d
./build/generator/generator cone 1.0 2.0 20 10 cone.3d
./build/generator/generator cylinder 1.0 3.0 24 12 cylinder.3d
./build/generator/generator torus 1.5 0.4 30 30 torus.3d
./build/generator/generator icosphere 1.0 3 icosphere.3d
```

**Notes:**
- All generated files are saved to `assets/models/` directory
- Higher subdivision values create smoother shapes but increase vertex count
- Icosphere subdivision levels: each level quadruples the triangle count
  - Level 1: 20 triangles
  - Level 2: 80 triangles
  - Level 3: 320 triangles
  - Level 4: 1,280 triangles
- See `docs/SHAPE_GENERATOR.md` for detailed documentation

## Project Structure

```
engine_system/
├── assets/
│   ├── models/     # 3D model files (.3d format)
│   └── scenes/     # Scene configuration files (.xml format)
├── build/          # Build output directory
├── common/         # Shared code (Vec, Mat classes)
├── engine/         # Main rendering engine
│   └── src/
│       ├── Engine.cpp/h    # Main engine class
│       ├── Model.cpp/h     # 3D model loading and rendering
│       ├── World.cpp/h     # Scene/world management
│       └── main.cpp        # Entry point
├── generator/      # 3D model generator tool
│   └── src/
│       ├── Generator.cpp/h # Shape generation logic
│       └── main.cpp        # Entry point
└── CMakeLists.txt
```

## Scene File Format

Scenes are defined using XML files. The format is simple and flexible with sensible defaults.

### Full Example

```xml
<world>
    <window width="800" height="600" />
    <camera>
        <position x="5" y="3" z="5" />
        <lookAt x="0" y="0" z="0" />
        <up x="0" y="1" z="0" />
        <projection fov="60" near="1" far="1000" />
    </camera>
    <models>
        <model file="assets/models/sphere.3d" />
        <model file="assets/models/plane.3d" />
    </models>
</world>
```

### Minimal Example

Most elements are optional! This is a valid scene file:

```xml
<world>
    <models>
        <model file="assets/models/sphere.3d" />
    </models>
</world>
```

### Element Structure

- **`<world>`** (required): Root element containing the entire scene
- **`<window>`** (optional): Window dimensions
  - `width` (default: 800): Window width in pixels
  - `height` (default: 600): Window height in pixels
- **`<camera>`** (optional): Camera configuration
  - **`<position>`**: Camera position in 3D space
    - `x`, `y`, `z` (default: 5, 5, 5)
  - **`<lookAt>`**: Point the camera is looking at
    - `x`, `y`, `z` (default: 0, 0, 0)
  - **`<up>`**: Up vector for camera orientation
    - `x`, `y`, `z` (default: 0, 1, 0)
  - **`<projection>`**: Projection settings
    - `fov` (default: 60): Field of view in degrees
    - `near` (default: 1): Near clipping plane
    - `far` (default: 1000): Far clipping plane
- **`<models>`** (optional): List of 3D models to render
  - **`<model>`**: Individual model with `file` attribute pointing to .3d file
    - `file` (required): Path to the 3D model file

### Key Features

✅ **Simple Structure**: No complex hierarchies or nesting  
✅ **Sensible Defaults**: Most elements are optional with reasonable default values  
✅ **Flexible**: Add only what you need to override  
✅ **Error Tolerant**: Missing models are skipped with warnings instead of failing  

**Note:** All models in the scene are rendered as a flat list without hierarchical grouping.

## Dependencies

The following dependencies are automatically installed via vcpkg:

- **glfw3** - Window and input management
- **imgui** - Immediate mode GUI (with GLFW and OpenGL2 bindings)
- **OpenGL** - Graphics API
- **GLEW** - OpenGL Extension Wrangler Library
- **tinyxml2** - XML parsing for scene files

## Controls

### Camera

- **Left Mouse Button + Drag**: Rotate camera around the scene
- **Mouse Wheel**: Zoom in/out

### UI

- Access all settings through the ImGui interface on the left side of the window
- Toggle fullscreen mode for immersive viewing
- Adjust camera parameters in real-time
- Monitor performance with built-in FPS counter

## Available Scene Examples

The project includes several example scenes demonstrating different features:

```bash
# Basic scenes with single shapes
./build/engine/system assets/scenes/box.xml
./build/engine/system assets/scenes/sphere_scene.xml
./build/engine/system assets/scenes/cone_scene.xml
./build/engine/system assets/scenes/plane.xml

# Multiple objects
./build/engine/system assets/scenes/sphere_and_plane.xml

# Hierarchical groups (new features)
./build/engine/system assets/scenes/minimal_group.xml
./build/engine/system assets/scenes/simple_group_test.xml
./build/engine/system assets/scenes/group_example.xml

# Complex examples
./build/engine/system assets/scenes/solar_system.xml
./build/engine/system assets/scenes/robot.xml
./build/engine/system assets/scenes/all_shapes.xml
```

## Performance Optimizations

This engine implements a layered set of CPU and GPU optimizations that significantly reduce per-frame work. The sections below explain each one, what problem it solves, and how it works.

---

### 1. Vertex Deduplication at Load Time

**Problem:** Model files (`.3d`, `.obj`) store one vertex entry per triangle corner, meaning the same position/normal/UV tuple appears many times for shared mesh edges. Uploading the raw list wastes GPU memory and bandwidth.

**Solution:** During parsing, every vertex is hashed using a custom FNV-inspired hash over its 8 floats (position + normal + UV). A `std::unordered_map` maps unique vertices to indices, and the result is a compact vertex buffer with an index buffer (`glDrawElements`). This typically reduces vertex count by ~50–60% on smooth meshes.

**Where:** `Model::LoadFrom3dFormatStream` / `LoadFromObjStream` in `engine/src/model/Model.cpp`

---

### 2. Two-Level Geometry Cache (CPU + GPU)

**Problem:** In scenes like the solar system, the same `sphere.3d` file is referenced by dozens of model nodes in the XML. Without caching, each node would re-read the file from disk and re-upload geometry to the GPU.

**Solution:** Two static caches work together:

- **`s_cpu_geo_cache`** (`unordered_map<path, CPUGeo>`): On the *first* load of a file, the parsed vertex/index vectors are *moved* (not copied) into this cache. Subsequent loads return a lightweight shell `Model` with no CPU vectors — just the AABB and vertex/index counts.
- **`s_geo_cache`** (`unordered_map<path, CachedGeo>`): On the first `InitGPU()` for a file, the geometry is uploaded and the VBO handles stored here. Every other `Model` sharing the same file path reuses those VBO IDs directly.

After all models are GPU-initialised at startup (`initAllGPU()`), `ClearModelCPUCache()` frees the CPU-side cache entirely, reclaiming RAM that is no longer needed.

**Effect:** N planets using the same sphere occupy 1× GPU memory and trigger 1 disk read, regardless of N.

**Where:** `LoadModelFromFile`, `Model::InitGPU`, `ClearModelCPUCache` in `Model.cpp`

---

### 3. Texture Cache

**Problem:** Multiple models pointing to the same texture file would each upload an independent copy to the GPU.

**Solution:** `s_tex_cache` (`unordered_map<path, GLuint>`) maps texture paths to GPU texture IDs. `loadTexture()` checks the cache before calling `stbi_load`. A cache hit just copies the existing `GLuint` and marks the model with `m_using_shared_tex = true` so the destructor does not delete the shared texture.

**Where:** `Model::loadTexture` in `Model.cpp`

---

### 4. AABB Cached at Load Time

**Problem:** Axis-aligned bounding boxes for frustum culling require iterating all vertices. Computing them at render time every frame is O(vertices) per model per frame.

**Solution:** The AABB is extended incrementally *during* vertex parsing (`m_cached_aabb.Extend(pos)` for each vertex) and stored in `m_cached_aabb`. At runtime, `Model::GetAABB()` is O(1) — a single struct copy.

**Where:** `Model.cpp` loaders; `Model::GetAABB()` in `Model.hpp`

---

### 5. Eager GPU Initialisation (No First-Frame Hitches)

**Problem:** With lazy GPU init (upload on first render), the first frame that draws a model would stall while uploading geometry, causing a visible hitch.

**Solution:** `Engine::Init()` calls `GetRootGroup().initAllGPU()` which walks the entire scene tree and calls `Model::InitGPU()` on every model before the render loop starts. All VBOs and textures are on the GPU before the first frame.

**Where:** `Group::initAllGPU`, `Engine::Init` in `Engine.cpp`

---

### 6. Hierarchical Frustum Culling with Plane Masking

**Problem:** Standard frustum culling tests a bounding box against all 6 frustum planes for every node. Deep hierarchies waste time: if an ancestor is already known to be fully inside a plane, descendants should not re-test that plane.

**Solution:** Each plane is represented by a bit in a `uint8_t` mask. `Frustum::HasInsideWithMask(aabb, inMask, outMask)` only tests the planes whose bits are set in `inMask`. When an AABB is found to be fully inside a plane (not just intersecting), that bit is cleared in `outMask` so descendants inherit a narrower test set. Entire subtrees are culled immediately when any plane test fails.

```
Root (mask=0x3F, test all 6 planes)
 └─ Planet group (fully inside left/right → outMask=0x0C, test only top/bottom/near/far)
     └─ Moon group (inherits 0x0C, skips left/right test entirely)
```

**Where:** `Frustum::HasInsideWithMask` in `Frustum.hpp`; `Group::render` in `Group.cpp`

---

### 7. Local Transform Matrix Cache

**Problem:** The render pass called `applyTransforms()` which re-evaluated every Catmull-Rom spline and trigonometric rotation every frame during rendering. This also happened independently in `updateWorldPositions()`, doubling the computation.

**Solution:** `updateWorldPositions()` accumulates all transforms — including Catmull-Rom position, derivative-based alignment rotation, and timed rotations — into a single `m_local_matrix[16]` per group. The render pass then calls `glMultMatrixf(m_local_matrix)` instead of re-evaluating anything. The expensive spline and `sinf`/`cosf` computation happens exactly once per frame, not twice.

**Where:** `Group::updateWorldPositions`, `Group::render` in `Group.cpp`

---

### 8. Orbit Curve VBOs (Lazy-Init)

**Problem:** `renderCurves()` previously sampled 100 Catmull-Rom points per orbit path using `getGlobalCatmullRomPoint()` and emitted them via `glVertex3f` inside a `glBegin/glEnd` block — every frame, for every visible curve. For a solar system with many orbits, this was thousands of spline evaluations and immediate-mode draw calls per frame.

**Solution:** The first time a curve is drawn, its 100 sampled positions are computed and uploaded into a `GL_ARRAY_BUFFER` (stored in `Transform::curve_vbo`). Every subsequent frame just does:
```
glBindBuffer → glVertexPointer → glDrawArrays(GL_LINE_LOOP)
```
No CPU computation after the first frame.

**Where:** `Group::renderCurves` in `Group.cpp`; `curve_vbo` / `curve_vbo_count` in `Group.hpp`

---

### 9. Orbit Circle VBOs (Lazy-Init)

**Problem:** `renderOrbitCircle()` recomputed 128 `cosf`/`sinf` pairs and submitted them via `glBegin/glEnd` every frame for each group with a timed rotation.

**Solution:** Same lazy-init VBO pattern as curves. On the first draw, the 128-vertex circle is computed and stored in `m_orbit_circle_vbo`. Every subsequent frame issues a single `glDrawArrays(GL_LINE_LOOP, 0, 128)`.

**Where:** `Group::renderOrbitCircle` in `Group.cpp`; `m_orbit_circle_vbo` in `Group.hpp`

---

### 10. Per-Frame GPU Render-State Caches

**Problem:** Every `Model::Render()` call unconditionally called `glBindTexture`, `glMaterialfv` (×5), `glColor4f`, `glBindBuffer` (×4), and `glVertexPointer/NormalPointer/TexCoordPointer` (×3) — regardless of whether the state actually changed from the previous model.

**Solution:** Four independent caches avoid redundant GPU state calls:

| Cache | State guarded | Skip condition |
|-------|--------------|----------------|
| `s_bound_texture` | `glBindTexture` | Texture ID unchanged |
| `s_last_mat` + `s_mat_valid` | `glMaterialfv` × 5 + `glColor4f` | `memcmp` on `Material` struct is zero |
| `s_bound_vbo_pos/norm/tex` | `glBindBuffer` + `glVertexPointer` × 3 | VBO ID unchanged |
| `s_bound_vbo_idx` | `glBindBuffer(GL_ELEMENT_ARRAY_BUFFER)` | Index VBO ID unchanged |

In the solar system, all planets share the same `sphere.3d` VBOs. After the first planet renders, every subsequent planet skips 6 `glBindBuffer` + 3 `gl*Pointer` calls — a saving of 9 GPU state changes per planet per frame.

`Model::InvalidateVBOCache()` resets all four caches when code outside `Model::Render()` mutates GL state (orbit curve/circle draws) or after a skybox restores saved attributes.

**Where:** Static variables and `Model::Render` / `Model::InvalidateVBOCache` in `Model.cpp`

---

### 11. Single VBO Unbind Per Frame (ImGui Compatibility)

**Problem:** `glBindBuffer(GL_ARRAY_BUFFER, 0)` was called after every model draw to avoid corrupting client-pointer state. The ImGui OpenGL2 backend uses `glVertexPointer` with a **CPU pointer**, not a VBO offset — if `GL_ARRAY_BUFFER` is non-zero when ImGui calls `glVertexPointer(ptr)`, the driver treats `ptr` as a byte offset into the bound VBO, producing garbage or no GUI.

**Solution:** Remove the per-model unbind (N calls saved per frame). Instead, unbind both `GL_ARRAY_BUFFER` and `GL_ELEMENT_ARRAY_BUFFER` exactly **once per frame**, between the 3D scene render and the ImGui render call. This preserves the VBO cache benefit throughout the 3D pass and guarantees clean state for ImGui.

**Where:** `Engine::Render` in `Engine.cpp` (after `GetRootGroup().render()`, before `postRenderImGui()`)

---

### 12. Pause-Aware World-Position Skip

**Problem:** `updateWorldPositions()` walks the entire scene tree every frame, evaluating splines and building world matrices and AABBs — even when the animation is paused and nothing has moved.

**Solution:** `m_positions_valid` tracks whether the cached world positions are still current. When animation is paused, positions are computed once on the first paused frame and then skipped on all subsequent paused frames. The flag is automatically reset when animation resumes, forcing a recompute on the first live frame.

**Where:** `Engine::Render` and `m_positions_valid` in `Engine.cpp` / `Engine.hpp`

---

### 13. Frame-Rate Cap (240 FPS)

**Problem:** With VSync disabled, the render loop runs at maximum GPU speed (potentially thousands of FPS on a fast machine), burning CPU and GPU resources for no perceptual benefit.

**Solution:** After each frame, `Engine::Run()` measures elapsed time and sleeps any remaining budget to maintain a 240 FPS ceiling. When VSync is on, the display driver's buffer swap already provides throttling and the sleep is skipped.

**Where:** `Engine::Run` in `Engine.cpp`

---

### 14. Persistent OpenGL Client States

**Problem:** Enabling and disabling `GL_VERTEX_ARRAY`, `GL_NORMAL_ARRAY`, and `GL_TEXTURE_COORD_ARRAY` around every draw call adds driver overhead.

**Solution:** All three client states are enabled once in `Engine::Init()` and never disabled during the 3D render pass. The ImGui backend disables them at the end of its draw call; they are restored at the start of the next `Engine::Render()` frame before any 3D rendering begins.

**Where:** `Engine::Init` and `Engine::Render` in `Engine.cpp`

---

### Summary

| Category | Optimization | Per-Frame Saving |
|----------|-------------|-----------------|
| Memory | Geometry & texture caches | VRAM: 1× per unique asset regardless of references |
| Memory | CPU cache cleared after GPU upload | RAM freed after startup |
| CPU | Vertex deduplication | ~50% fewer vertices processed |
| CPU | AABB cached at load time | O(vertices) → O(1) per model |
| CPU | Local matrix cache | Catmull-Rom + trig computed once, not twice |
| CPU | Orbit curve VBOs | 100 spline evals/curve → 0 (after first frame) |
| CPU | Orbit circle VBOs | 128 trig calls/circle → 0 (after first frame) |
| CPU | Pause-aware position skip | Entire scene tree walk skipped when paused |
| GPU | Hierarchical frustum culling | Up to 6 plane tests → 0–2 for deep children |
| GPU | Texture binding cache | `glBindTexture` only on change |
| GPU | Material + color cache | 6 `glMaterial*` calls only on change |
| GPU | VBO binding cache | 9 `glBindBuffer`/`gl*Pointer` calls → 0 for shared geometry |
| GPU | Single unbind per frame | N `glBindBuffer(0)` → 2 per frame |
| System | 240 FPS cap | Prevents runaway CPU/GPU burn |

---

## Troubleshooting

### Build Issues

1. Make sure vcpkg is properly installed and integrated
2. Verify CMake version is 3.25 or higher: `cmake --version`
3. Check that OpenGL drivers are up to date

### Runtime Issues

1. Always run executables from the project root directory
2. Ensure scene XML files and 3D model files exist in the correct paths
3. Check console output for detailed error messages

## License

This project is part of a Computer Graphics course assignment.
