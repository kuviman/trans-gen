module color;

import stream;
import std.conv;
import std.typecons : Nullable;


/// RGBA Color
struct Color {
    /// Red component
    float r;
    /// Green component
    float g;
    /// Blue component
    float b;
    /// Alpha (opacity) component
    float a;

    this(float r, float g, float b, float a) {
        this.r = r;
        this.g = g;
        this.b = b;
        this.a = a;
    }

    /// Read Color from reader
    static Color readFrom(Stream reader) {
        float r;
        r = reader.readFloat();
        float g;
        g = reader.readFloat();
        float b;
        b = reader.readFloat();
        float a;
        a = reader.readFloat();
        return Color(r, g, b, a);
    }

    /// Write Color to writer
    void writeTo(Stream writer) const {
        writer.write(r);
        writer.write(g);
        writer.write(b);
        writer.write(a);
    }
}