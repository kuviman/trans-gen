import model;
import stream;
import std.conv;
import std.typecons : Nullable;

/// 2 dimensional vector.
struct Vec2Int {
    /// `x` coordinate of the vector
    int x;
    /// `y` coordinate of the vector
    int y;

    this(int x, int y) {
        this.x = x;
        this.y = y;
    }

    /// Read Vec2Int from input stream
    static Vec2Int readFrom(Stream reader) {
        int x;
        x = reader.readInt();
        int y;
        y = reader.readInt();
        return Vec2Int(x, y);
    }

    /// Write Vec2Int to output stream
    void writeTo(Stream writer) const {
        writer.write(x);
        writer.write(y);
    }
}