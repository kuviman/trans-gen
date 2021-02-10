#ifndef __MODEL_COLORED_VERTEX_HPP__
#define __MODEL_COLORED_VERTEX_HPP__

#include "Color.hpp"
#include "Stream.hpp"
#include "Vec2Float.hpp"
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>

namespace model::debug_interface {

// Vertex for debug rendering
class ColoredVertex {
public:
    // Position in world coordinates (if none, screen position (0, 0) is used)
    std::optional<Vec2Float> worldPos;
    // Additional offset in screen coordinates
    Vec2Float screenOffset;
    // Color to use
    Color color;

    ColoredVertex(std::optional<Vec2Float> worldPos, Vec2Float screenOffset, Color color);

    // Read ColoredVertex from input stream
    static ColoredVertex readFrom(InputStream& stream);

    // Write ColoredVertex to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of ColoredVertex
    std::string toString() const;
};

}

#endif