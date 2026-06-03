#include "Scene.hpp"

#include <iostream>
#include "tinyxml2.h"
#include "Group.hpp"

bool Scene::LoadFromXml(const std::string &file_path)
{
    tinyxml2::XMLDocument doc;
    if (doc.LoadFile(file_path.c_str()) != tinyxml2::XML_SUCCESS) {
        std::cerr << "Error: Could not load XML file '" << file_path << "'\n";
        return false;
    }

    auto world = doc.FirstChildElement("world");
    if (!world) {
        std::cerr << "Error: Missing <world> element\n";
        return false;
    }

    // Window
    if (auto win = world->FirstChildElement("window")) {
        win->QueryIntAttribute("width",  &m_window.width());
        win->QueryIntAttribute("height", &m_window.height());
    }

    // Background color
    if (auto bg = world->FirstChildElement("background")) {
        bg->QueryFloatAttribute("r", &m_background_color[0]);
        bg->QueryFloatAttribute("g", &m_background_color[1]);
        bg->QueryFloatAttribute("b", &m_background_color[2]);
        bg->QueryFloatAttribute("a", &m_background_color[3]);
    }

    // Camera
    if (auto cam = world->FirstChildElement("camera")) {
        if (auto pos = cam->FirstChildElement("position")) {
            pos->QueryFloatAttribute("x", &m_camera.position().x);
            pos->QueryFloatAttribute("y", &m_camera.position().y);
            pos->QueryFloatAttribute("z", &m_camera.position().z);
        }
        if (auto la = cam->FirstChildElement("lookAt")) {
            la->QueryFloatAttribute("x", &m_camera.looking_at().x);
            la->QueryFloatAttribute("y", &m_camera.looking_at().y);
            la->QueryFloatAttribute("z", &m_camera.looking_at().z);
        }
        if (auto up = cam->FirstChildElement("up")) {
            up->QueryFloatAttribute("x", &m_camera.up().x);
            up->QueryFloatAttribute("y", &m_camera.up().y);
            up->QueryFloatAttribute("z", &m_camera.up().z);
        }
        if (auto proj = cam->FirstChildElement("projection")) {
            proj->QueryFloatAttribute("fov",  &m_camera.fov());
            proj->QueryFloatAttribute("near", &m_camera.near());
            proj->QueryFloatAttribute("far",  &m_camera.far());
        }
    }

    // Lights
    // Parse 3 types of lights: DirectionalLight, PointLight, and Spotlight
    // Handles both camelCase (posX) and lowercase (posx) attribute names
    if (auto lights_elem = world->FirstChildElement("lights")) {
        // Helper: try both name variants (camelCase and lowercase)
        auto queryF = [](tinyxml2::XMLElement* e, const char* nameA, const char* nameB, float &out) {
            if (e->QueryFloatAttribute(nameA, &out) != tinyxml2::XML_SUCCESS)
                e->QueryFloatAttribute(nameB, &out);
        };

        for (auto le = lights_elem->FirstChildElement("light"); le;
             le = le->NextSiblingElement("light"))
        {
            const char *type_str = le->Attribute("type");
            if (!type_str) continue;

            std::string type = type_str;

            if (type == "directional") {
                DirectionalLight dl;
                dl.dir = {0.0f, 1.0f, 0.0f};
                queryF(le, "dirX", "dirx", dl.dir.x);
                queryF(le, "dirY", "diry", dl.dir.y);
                queryF(le, "dirZ", "dirz", dl.dir.z);
                m_lights.push_back(dl);
            }
            else if (type == "point") {
                PointLight pl;
                pl.pos = {0.0f, 0.0f, 0.0f};
                queryF(le, "posX", "posx", pl.pos.x);
                queryF(le, "posY", "posy", pl.pos.y);
                queryF(le, "posZ", "posz", pl.pos.z);
                m_lights.push_back(pl);
            }
            else if (type == "spot" || type == "spotlight") {
                Spotlight sl;
                sl.pos    = {0.0f, 0.0f, 0.0f};
                sl.dir    = {0.0f, -1.0f, 0.0f};
                sl.cutoff = 45.0f;
                queryF(le, "posX", "posx", sl.pos.x);
                queryF(le, "posY", "posy", sl.pos.y);
                queryF(le, "posZ", "posz", sl.pos.z);
                queryF(le, "dirX", "dirx", sl.dir.x);
                queryF(le, "dirY", "diry", sl.dir.y);
                queryF(le, "dirZ", "dirz", sl.dir.z);
                le->QueryFloatAttribute("cutoff", &sl.cutoff);
                m_lights.push_back(sl);
            }
        }
    }

    // Legacy flat model list
    if (auto models = world->FirstChildElement("models")) {
        for (auto me = models->FirstChildElement("model"); me;
             me = me->NextSiblingElement("model"))
        {
            const char *fp = me->Attribute("file");
            if (!fp) continue;
            auto model = LoadModelFromFile(fp);
            if (model) m_root_group.addModel(std::move(model.value()));
        }
    }

    // Group hierarchy
    if (auto group = world->FirstChildElement("group"))
        m_root_group = initializeGroupFromXML(group);

    m_default_camera = m_camera;
    return true;
}
