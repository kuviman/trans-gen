#include "ColoredVertex.hpp"

namespace model::debug_interface {

ColoredVertex::ColoredVertex(std::optional<Vec2Float> worldPos, Vec2Float screenOffset, Color color) : worldPos(worldPos), screenOffset(screenOffset), color(color) { }

// Read ColoredVertex from input stream
ColoredVertex ColoredVertex::readFrom(InputStream& stream) {
    std::optional<Vec2Float> worldPos = std::optional<Vec2Float>();
    if (stream.readBool()) {
        worldPos = Vec2Float::readFrom(stream);
    }
    Vec2Float screenOffset = Vec2Float::readFrom(stream);
    Color color = Color::readFrom(stream);
    return ColoredVertex(worldPos, screenOffset, color);
}

// Write ColoredVertex to output stream
void ColoredVertex::writeTo(OutputStream& stream) const {
    if (worldPos) {
        stream.write(true);
        const Vec2Float& worldPosValue = *worldPos;
        worldPosValue.writeTo(stream);
    } else {
        stream.write(false);
    }
    screenOffset.writeTo(stream);
    color.writeTo(stream);
}

// Get string representation of ColoredVertex
std::string ColoredVertex::toString() const {
    std::stringstream ss;
    ss << "ColoredVertex { ";
    ss << "worldPos: ";
    if (worldPos) {
        const Vec2Float& worldPosValue = *worldPos;
        ss << worldPosValue.toString();
    } else {
        ss << "none";
    }
    ss << ", ";
    ss << "screenOffset: ";
    ss << screenOffset.toString();
    ss << ", ";
    ss << "color: ";
    ss << color.toString();
    ss << " }";
    return ss.str();
}

}