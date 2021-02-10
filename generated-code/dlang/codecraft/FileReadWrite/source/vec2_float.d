module vec2_float;

import stream;
import std.conv;
import std.typecons : Nullable;


/// 2 dimensional vector.
struct Vec2Float {
    /// `x` coordinate of the vector
    float x;
    /// `y` coordinate of the vector
    float y;

    this(float x, float y) {
        this.x = x;
        this.y = y;
    }

    /// Read Vec2Float from reader
    static Vec2Float readFrom(Stream reader) {
        float x;
        x = reader.readFloat();
        float y;
        y = reader.readFloat();
        return Vec2Float(x, y);
    }

    /// Write Vec2Float to writer
    void writeTo(Stream writer) const {
        writer.write(x);
        writer.write(y);
    }
}