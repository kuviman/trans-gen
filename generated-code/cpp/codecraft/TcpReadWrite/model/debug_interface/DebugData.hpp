#ifndef __MODEL_DEBUG_DATA_HPP__
#define __MODEL_DEBUG_DATA_HPP__

#include "Color.hpp"
#include "Stream.hpp"
#include "Vec2Float.hpp"
#include "model/debug_interface/ColoredVertex.hpp"
#include "model/debug_interface/PrimitiveType.hpp"
#include <memory>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace model::debug_interface {

// Debug data can be drawn in the app
class DebugData {
public:
    // Log some text
    class Log;
    // Draw primitives
    class Primitives;
    // Draw text
    class PlacedText;

    // Read DebugData from input stream
    static std::shared_ptr<DebugData> readFrom(InputStream& stream);

    // Write DebugData to output stream
    virtual void writeTo(OutputStream& stream) const = 0;

    // Get string representation of DebugData
    virtual std::string toString() const = 0;
};

// Log some text
class DebugData::Log : public DebugData {
public:
    static const int TAG = 0;

    // Text to show
    std::string text;

    Log(std::string text);

    // Read Log from input stream
    static Log readFrom(InputStream& stream);

    // Write Log to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Log
    std::string toString() const;

    bool operator ==(const Log& other) const;
};

// Draw primitives
class DebugData::Primitives : public DebugData {
public:
    static const int TAG = 1;

    // Vertices
    std::vector<model::debug_interface::ColoredVertex> vertices;
    // Primitive type
    model::debug_interface::PrimitiveType primitiveType;

    Primitives(std::vector<model::debug_interface::ColoredVertex> vertices, model::debug_interface::PrimitiveType primitiveType);

    // Read Primitives from input stream
    static Primitives readFrom(InputStream& stream);

    // Write Primitives to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Primitives
    std::string toString() const;
};

// Draw text
class DebugData::PlacedText : public DebugData {
public:
    static const int TAG = 2;

    // Vertex to determine text position and color
    model::debug_interface::ColoredVertex vertex;
    // Text
    std::string text;
    // Text alignment (0 means left, 0.5 means center, 1 means right)
    float alignment;
    // Font size in pixels
    float size;

    PlacedText(model::debug_interface::ColoredVertex vertex, std::string text, float alignment, float size);

    // Read PlacedText from input stream
    static PlacedText readFrom(InputStream& stream);

    // Write PlacedText to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of PlacedText
    std::string toString() const;
};

}

#endif