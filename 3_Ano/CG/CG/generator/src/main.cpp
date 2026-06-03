#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>

#include "Generator.hpp"

constexpr std::string_view default_folder = "assets/models/";

class Command
{
public:
    std::string name;
    std::string args;
    size_t arg_count;
};

const auto commands = {
    Command{"plane",    "<length> <divisions>",                 2},
    Command{"sphere",   "<radius> <slices> <stacks>",           3},
    Command{"cone",     "<radius> <height> <slices> <stacks>",  4},
    Command{"box",      "<length> <divisions>",                 2},
    Command{"cylinder", "<radius> <height> <slices> <stacks>",  4},
    Command{"torus",    "<radius> <tubeRadius> <slices> <stacks>", 4},
    Command{"icosphere","<radius> <subdivisions>",              2},
    Command{"patch",    "<input file> <tessellation>",          2},
};

const Command *getCommand(const char *name)
{
    for (const auto &cmd : commands)
        if (cmd.name == name) return &cmd;
    return nullptr;
}

void printHelp()
{
    std::cout << "Usage: generator <command> <args> <output file>\nCommands:\n";
    for (const auto &[name, args, _] : commands)
        std::cout << "\tgenerator " << name << ' ' << args << " <output file>\n";
}

void printUsage(const Command &cmd)
{
    std::cout << "Usage: generator " << cmd.name << ' ' << cmd.args << " <output file>\n";
}

std::vector<Vertex> runGenerator(const Command &cmd, char *args[])
{
    if (cmd.name == "plane")
        return generator::GeneratePlane(std::stof(args[0]), std::stoi(args[1]));
    if (cmd.name == "sphere")
        return generator::GenerateSphere(std::stof(args[0]), std::stoi(args[1]), std::stoi(args[2]));
    if (cmd.name == "cone")
        return generator::GenerateCone(std::stof(args[0]), std::stof(args[1]), std::stoi(args[2]), std::stoi(args[3]));
    if (cmd.name == "box")
        return generator::GenerateBox(std::stof(args[0]), std::stoi(args[1]));
    if (cmd.name == "cylinder")
        return generator::GenerateCylinder(std::stof(args[0]), std::stof(args[1]), std::stoi(args[2]), std::stoi(args[3]));
    if (cmd.name == "torus")
        return generator::GenerateTorus(std::stof(args[0]), std::stof(args[1]), std::stoi(args[2]), std::stoi(args[3]));
    if (cmd.name == "icosphere")
        return generator::GenerateIcosphere(std::stof(args[0]), std::stoi(args[1]));
    if (cmd.name == "patch")
        return generator::GeneratePatch(args[0], std::stoi(args[1]));
    throw std::runtime_error("Unknown command");
}

int main(const int argc, char *argv[])
{
    if (argc < 2) { printHelp(); return 1; }

    const char *command = argv[1];
    const auto *cmd = getCommand(command);
    if (!cmd) {
        std::cout << "Unknown command: " << command << "\n";
        printHelp(); return 1;
    }
    if (argc - 3 != (int)cmd->arg_count) {
        std::cout << "Invalid number of arguments for command " << cmd->name << "\n";
        printUsage(*cmd); return 1;
    }

    std::vector<Vertex> vertices;
    try {
        vertices = runGenerator(*cmd, argv + 2);
    } catch (...) {
        std::cout << "Invalid arguments for command " << cmd->name << "\n";
        printUsage(*cmd); return 1;
    }

    const auto save_file = argv[argc - 1];
    std::filesystem::path path = default_folder;
    path.append(save_file);
    if (!std::filesystem::exists(path.parent_path()))
        std::filesystem::create_directories(path.parent_path());

    std::ofstream file(path, std::ios::trunc);
    if (!file.is_open()) {
        std::cout << "Failed to open file " << save_file << "\n";
        return 1;
    }

    file << vertices.size() << "\n";
    for (const auto &v : vertices)
    {
        file << v.position.x << " " << v.position.y << " " << v.position.z << " "
             << v.normal.x   << " " << v.normal.y   << " " << v.normal.z   << " "
             << v.u          << " " << v.v           << "\n";
    }

    return 0;
}
