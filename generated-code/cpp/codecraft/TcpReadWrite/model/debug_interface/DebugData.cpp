#include "DebugData.hpp"
#include <stdexcept>

namespace model::debug_interface {

DebugData::Log::Log(std::string text) : text(text) { }

// Read Log from input stream
DebugData::Log DebugData::Log::readFrom(InputStream& stream) {
    std::string text = stream.readString();
    return DebugData::Log(text);
}

// Write Log to output stream
void DebugData::Log::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write(text);
}

// Get string representation of Log
std::string DebugData::Log::toString() const {
    std::stringstream ss;
    ss << "DebugData::Log { ";
    ss << "text: ";
    ss << '"' << text << '"';
    ss << " }";
    return ss.str();
}

bool DebugData::Log::operator ==(const DebugData::Log& other) const {
    return text == other.text;
}

DebugData::Primitives::Primitives(std::vector<model::debug_interface::ColoredVertex> vertices, model::debug_interface::PrimitiveType primitiveType) : vertices(vertices), primitiveType(primitiveType) { }

// Read Primitives from input stream
DebugData::Primitives DebugData::Primitives::readFrom(InputStream& stream) {
    std::vector<model::debug_interface::ColoredVertex> vertices = std::vector<model::debug_interface::ColoredVertex>();
    size_t verticesSize = stream.readInt();
    vertices.reserve(verticesSize);
    for (size_t verticesIndex = 0; verticesIndex < verticesSize; verticesIndex++) {
        model::debug_interface::ColoredVertex verticesElement = model::debug_interface::ColoredVertex::readFrom(stream);
        vertices.emplace_back(verticesElement);
    }
    model::debug_interface::PrimitiveType primitiveType = readPrimitiveType(stream);
    return DebugData::Primitives(vertices, primitiveType);
}

// Write Primitives to output stream
void DebugData::Primitives::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    stream.write((int)(vertices.size()));
    for (const model::debug_interface::ColoredVertex& verticesElement : vertices) {
        verticesElement.writeTo(stream);
    }
    stream.write((int)(primitiveType));
}

// Get string representation of Primitives
std::string DebugData::Primitives::toString() const {
    std::stringstream ss;
    ss << "DebugData::Primitives { ";
    ss << "vertices: ";
    ss << "[ ";
    for (size_t verticesIndex = 0; verticesIndex < vertices.size(); verticesIndex++) {
        const model::debug_interface::ColoredVertex& verticesElement = vertices[verticesIndex];
        if (verticesIndex != 0) {
            ss << ", ";
        }
        ss << verticesElement.toString();
    }
    ss << " ]";
    ss << ", ";
    ss << "primitiveType: ";
    ss << primitiveTypeToString(primitiveType);
    ss << " }";
    return ss.str();
}

DebugData::PlacedText::PlacedText(model::debug_interface::ColoredVertex vertex, std::string text, float alignment, float size) : vertex(vertex), text(text), alignment(alignment), size(size) { }

// Read PlacedText from input stream
DebugData::PlacedText DebugData::PlacedText::readFrom(InputStream& stream) {
    model::debug_interface::ColoredVertex vertex = model::debug_interface::ColoredVertex::readFrom(stream);
    std::string text = stream.readString();
    float alignment = stream.readFloat();
    float size = stream.readFloat();
    return DebugData::PlacedText(vertex, text, alignment, size);
}

// Write PlacedText to output stream
void DebugData::PlacedText::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    vertex.writeTo(stream);
    stream.write(text);
    stream.write(alignment);
    stream.write(size);
}

// Get string representation of PlacedText
std::string DebugData::PlacedText::toString() const {
    std::stringstream ss;
    ss << "DebugData::PlacedText { ";
    ss << "vertex: ";
    ss << vertex.toString();
    ss << ", ";
    ss << "text: ";
    ss << '"' << text << '"';
    ss << ", ";
    ss << "alignment: ";
    ss << alignment;
    ss << ", ";
    ss << "size: ";
    ss << size;
    ss << " }";
    return ss.str();
}

// Read DebugData from input stream
std::shared_ptr<DebugData> DebugData::readFrom(InputStream& stream) {
    switch (stream.readInt()) {
    case 0:
        return std::shared_ptr<DebugData::Log>(new DebugData::Log(DebugData::Log::readFrom(stream)));
    case 1:
        return std::shared_ptr<DebugData::Primitives>(new DebugData::Primitives(DebugData::Primitives::readFrom(stream)));
    case 2:
        return std::shared_ptr<DebugData::PlacedText>(new DebugData::PlacedText(DebugData::PlacedText::readFrom(stream)));
    default:
        throw std::runtime_error("Unexpected tag value");
    }
}

}