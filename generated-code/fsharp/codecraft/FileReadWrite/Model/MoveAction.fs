#nowarn "0058"

namespace TransGenTest.Model

open TransGenTest

/// Move action
type MoveAction = {
    /// Target position
    Target: Vec2Int;
    /// Whether to try find closest position, if path to target is not found
    FindClosestPosition: bool;
    /// Whether to destroy other entities on the way
    BreakThrough: bool;
} with

    /// Write MoveAction to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        this.Target.writeTo writer
        writer.Write this.FindClosestPosition
        writer.Write this.BreakThrough

    /// Read MoveAction from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Target = Vec2Int.readFrom reader;
        FindClosestPosition = reader.ReadBoolean()
        BreakThrough = reader.ReadBoolean()
    }