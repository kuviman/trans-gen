import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Vec2Int {
    int x;
    int y;

    this(int x, int y) {
        this.x = x;
        this.y = y;
    }

    static Vec2Int readFrom(Stream reader) {
        int x;
        x = reader.readInt();
        int y;
        y = reader.readInt();
        return Vec2Int(x, y);
    }

    void writeTo(Stream writer) const {
        writer.write(x);
        writer.write(y);
    }
}