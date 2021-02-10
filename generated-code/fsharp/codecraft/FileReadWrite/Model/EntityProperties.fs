#nowarn "0058"

namespace TransGenTest.Model

open TransGenTest

/// Entity properties
type EntityProperties = {
    /// Size. Entity has a form of a square with side of this length
    Size: int;
    /// Score for building this entity
    BuildScore: int;
    /// Score for destroying this entity
    DestroyScore: int;
    /// Whether this entity can move
    CanMove: bool;
    /// Number of population points this entity provides, if active
    PopulationProvide: int;
    /// Number of population points this entity uses
    PopulationUse: int;
    /// Maximum health points
    MaxHealth: int;
    /// Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
    InitialCost: int;
    /// If fog of war is enabled, maximum distance at which other entities are considered visible
    SightRange: int;
    /// Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
    ResourcePerHealth: int;
    /// Build properties, if entity can build
    Build: option<Model.BuildProperties>;
    /// Attack properties, if entity can attack
    Attack: option<Model.AttackProperties>;
    /// Repair properties, if entity can repair
    Repair: option<Model.RepairProperties>;
} with

    /// Write EntityProperties to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.Size
        writer.Write this.BuildScore
        writer.Write this.DestroyScore
        writer.Write this.CanMove
        writer.Write this.PopulationProvide
        writer.Write this.PopulationUse
        writer.Write this.MaxHealth
        writer.Write this.InitialCost
        writer.Write this.SightRange
        writer.Write this.ResourcePerHealth
        match this.Build with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
        match this.Attack with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
        match this.Repair with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false

    /// Read EntityProperties from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Size = reader.ReadInt32()
        BuildScore = reader.ReadInt32()
        DestroyScore = reader.ReadInt32()
        CanMove = reader.ReadBoolean()
        PopulationProvide = reader.ReadInt32()
        PopulationUse = reader.ReadInt32()
        MaxHealth = reader.ReadInt32()
        InitialCost = reader.ReadInt32()
        SightRange = reader.ReadInt32()
        ResourcePerHealth = reader.ReadInt32()
        Build = match reader.ReadBoolean() with
                    | true -> Some(Model.BuildProperties.readFrom reader;)
                    | false -> None
        Attack = match reader.ReadBoolean() with
                     | true -> Some(Model.AttackProperties.readFrom reader;)
                     | false -> None
        Repair = match reader.ReadBoolean() with
                     | true -> Some(Model.RepairProperties.readFrom reader;)
                     | false -> None
    }