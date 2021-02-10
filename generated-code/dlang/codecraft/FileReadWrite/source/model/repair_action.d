module model.repair_action;

import stream;
import std.conv;
import std.typecons : Nullable;


/// Repair action
struct RepairAction {
    /// Target entity's ID
    int target;

    this(int target) {
        this.target = target;
    }

    /// Read RepairAction from reader
    static RepairAction readFrom(Stream reader) {
        int target;
        target = reader.readInt();
        return RepairAction(target);
    }

    /// Write RepairAction to writer
    void writeTo(Stream writer) const {
        writer.write(target);
    }
}