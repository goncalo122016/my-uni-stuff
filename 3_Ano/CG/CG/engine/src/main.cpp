#include "core/Engine.hpp"
#include "scene/Scene.hpp"

#include <iostream>

#include "utils/Utils.hpp"

const std::vector<std::string> SCENES_PATHS_TO_SEARCH = {"assets/scenes/", "./"};

int main(const int argc, char *argv[])
{
    if (argc < 2)
    {
        std::cout << "Usage: engine <scene.xml>" << std::endl;
        return 1;
    }

    const char *file_path = argv[1];
    const auto o_path = utils::FindFile(SCENES_PATHS_TO_SEARCH, file_path);
    if (!o_path)
    {
        std::cerr << "Scene file not found: '" << file_path << "'" << std::endl;
        std::cerr << "Searched in: ";
        for (const auto &p : SCENES_PATHS_TO_SEARCH)
            std::cerr << "'" << p << "' ";
        std::cerr << std::endl;
        std::cerr << "Usage: engine <scene.xml>" << std::endl;
        return 1;
    }

    const auto path_string = o_path.value().string();

    Scene scene(path_string);
    if (!scene.LoadFromXml(path_string))
        return 1;

    Engine engine(scene);
    if (!engine.Init())
        return 1;

    engine.Run();

    engine.Shutdown();

    return 0;
}
