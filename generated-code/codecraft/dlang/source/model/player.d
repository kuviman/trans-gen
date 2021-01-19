import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct Player {
    int id;
    int score;
    int resource;

    this(int id, int score, int resource) {
        this.id = id;
        this.score = score;
        this.resource = resource;
    }

    static Player readFrom(Stream reader) {
        int id;
        id = reader.readInt();
        int score;
        score = reader.readInt();
        int resource;
        resource = reader.readInt();
        return Player(id, score, resource);
    }

    void writeTo(Stream writer) const {
        writer.write(id);
        writer.write(score);
        writer.write(resource);
    }
}