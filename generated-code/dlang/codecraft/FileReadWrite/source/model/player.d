module model.player;

import stream;
import std.conv;
import std.typecons : Nullable;


/// Player (strategy, client)
struct Player {
    /// Player's ID
    int id;
    /// Current score
    int score;
    /// Current amount of resource
    int resource;

    this(int id, int score, int resource) {
        this.id = id;
        this.score = score;
        this.resource = resource;
    }

    /// Read Player from reader
    static Player readFrom(Stream reader) {
        int id;
        id = reader.readInt();
        int score;
        score = reader.readInt();
        int resource;
        resource = reader.readInt();
        return Player(id, score, resource);
    }

    /// Write Player to writer
    void writeTo(Stream writer) const {
        writer.write(id);
        writer.write(score);
        writer.write(resource);
    }
}