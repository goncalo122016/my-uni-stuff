#ifndef WINDOW_H
#define WINDOW_H

class Window
{
private:
    int m_width = 800;
    int m_height = 600;

public:
    Window() = default;
    Window(int width, int height) : m_width(width), m_height(height) {}

    int GetWidth() const { return m_width; }
    int GetHeight() const { return m_height; }
    void SetWidth(int width) { m_width = width; }
    void SetHeight(int height) { m_height = height; }
    
    // For backward compatibility with direct access
    int& width() { return m_width; }
    int& height() { return m_height; }
};

#endif // WINDOW_H