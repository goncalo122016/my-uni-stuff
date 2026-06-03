#ifndef ENGINE_SYSTEM_ENVIRONMENT_H
#define ENGINE_SYSTEM_ENVIRONMENT_H

#include <string>

class EngineSystemEnvironment
{
public:
    std::string glew_version = "unknown";
    std::string glfw_version = "unknown";
    std::string imgui_version = "unknown";
    std::string opengl_version = "unknown";
    std::string gpu_renderer = "unknown";

    EngineSystemEnvironment() = default;
};

#endif // ENGINE_SYSTEM_ENVIRONMENT_H