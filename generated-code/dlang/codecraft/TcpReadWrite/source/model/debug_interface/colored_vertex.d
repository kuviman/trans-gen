module model.debug_interface.colored_vertex;

import stream;
import std.conv;
import std.typecons : Nullable;
import color;
import vec2_float;

/// Vertex for debug rendering
struct ColoredVertex {
    /// Position in world coordinates (if none, screen position (0, 0) is used)
    Nullable!(Vec2Float) worldPos;
    /// Additional offset in screen coordinates
    Vec2Float screenOffset;
    /// Color to use
    Color color;

    this(Nullable!(Vec2Float) worldPos, Vec2Float screenOffset, Color color) {
        this.worldPos = worldPos;
        this.screenOffset = screenOffset;
        this.color = color;
    }

    /// Read ColoredVertex from reader
    static ColoredVertex readFrom(Stream reader) {
        Nullable!(Vec2Float) worldPos;
        if (reader.readBool()) {
            worldPos = Vec2Float.readFrom(reader);
        } else {
            worldPos.nullify();
        }
        Vec2Float screenOffset;
        screenOffset = Vec2Float.readFrom(reader);
        Color color;
        color = Color.readFrom(reader);
        return ColoredVertex(worldPos, screenOffset, color);
    }

    /// Write ColoredVertex to writer
    void writeTo(Stream writer) const {
        if (worldPos.isNull()) {
            writer.write(false);
        } else {
            writer.write(true);
            worldPos.get.writeTo(writer);
        }
        screenOffset.writeTo(writer);
        color.writeTo(writer);
    }
}