#nowarn "0058"

namespace TransGenTest.Model

open TransGenTest

/// Game entity
type Entity = {
    /// Entity's ID. Unique for each entity
    Id: int;
    /// Entity's owner player ID, if owned by a player
    PlayerId: option<int>;
    /// Entity's type
    EntityType: Model.EntityType;
    /// Entity's position (corner with minimal coordinates)
    Position: Vec2Int;
    /// Current health
    Health: int;
    /// If entity is active, it can perform actions
    Active: bool;
} with

    /// Write Entity to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.Id
        match this.PlayerId with
            | Some value ->
                writer.Write true
                writer.Write value
            | None -> writer.Write false
        writer.Write (int this.EntityType)
        this.Position.writeTo writer
        writer.Write this.Health
        writer.Write this.Active

    /// Read Entity from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Id = reader.ReadInt32()
        PlayerId = match reader.ReadBoolean() with
                       | true -> Some(reader.ReadInt32())
                       | false -> None
        EntityType = reader.ReadInt32() |> enum
        Position = Vec2Int.readFrom reader;
        Health = reader.ReadInt32()
        Active = reader.ReadBoolean()
    }