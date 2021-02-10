#nowarn "0058"

namespace TransGenTest.Model

open TransGenTest

/// Build action
type BuildAction = {
    /// Type of an entity to build
    EntityType: Model.EntityType;
    /// Desired position of new entity
    Position: Vec2Int;
} with

    /// Write BuildAction to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write (int this.EntityType)
        this.Position.writeTo writer

    /// Read BuildAction from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        EntityType = reader.ReadInt32() |> enum
        Position = Vec2Int.readFrom reader;
    }