module model.move_action;

import stream;
import std.conv;
import std.typecons : Nullable;
import vec2_int;

/// Move action
struct MoveAction {
    /// Target position
    Vec2Int target;
    /// Whether to try find closest position, if path to target is not found
    bool findClosestPosition;
    /// Whether to destroy other entities on the way
    bool breakThrough;

    this(Vec2Int target, bool findClosestPosition, bool breakThrough) {
        this.target = target;
        this.findClosestPosition = findClosestPosition;
        this.breakThrough = breakThrough;
    }

    /// Read MoveAction from reader
    static MoveAction readFrom(Stream reader) {
        Vec2Int target;
        target = Vec2Int.readFrom(reader);
        bool findClosestPosition;
        findClosestPosition = reader.readBool();
        bool breakThrough;
        breakThrough = reader.readBool();
        return MoveAction(target, findClosestPosition, breakThrough);
    }

    /// Write MoveAction to writer
    void writeTo(Stream writer) const {
        target.writeTo(writer);
        writer.write(findClosestPosition);
        writer.write(breakThrough);
    }
}