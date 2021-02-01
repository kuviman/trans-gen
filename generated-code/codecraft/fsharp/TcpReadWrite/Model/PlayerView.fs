#nowarn "0058"

namespace TransGenTest.Model

/// Information available to the player
type PlayerView = {
    /// Your player's ID
    MyId: int;
    /// Size of the map
    MapSize: int;
    /// Whether fog of war is enabled
    FogOfWar: bool;
    /// Entity properties for each entity type
    EntityProperties: Map<EntityType, EntityProperties>;
    /// Max tick count for the game
    MaxTickCount: int;
    /// Max pathfind nodes when performing pathfinding in the game simulator
    MaxPathfindNodes: int;
    /// Current tick
    CurrentTick: int;
    /// List of players
    Players: Player[];
    /// List of entities
    Entities: Entity[];
} with

    /// Write PlayerView to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.MyId
        writer.Write this.MapSize
        writer.Write this.FogOfWar
        writer.Write this.EntityProperties.Count
        this.EntityProperties |> Map.iter (fun key value ->
            writer.Write (int key)
            value.writeTo writer )
        writer.Write this.MaxTickCount
        writer.Write this.MaxPathfindNodes
        writer.Write this.CurrentTick
        writer.Write this.Players.Length
        this.Players |> Array.iter (fun value ->
            value.writeTo writer )
        writer.Write this.Entities.Length
        this.Entities |> Array.iter (fun value ->
            value.writeTo writer )

    /// Read PlayerView from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        MyId = reader.ReadInt32()
        MapSize = reader.ReadInt32()
        FogOfWar = reader.ReadBoolean()
        EntityProperties = [for _ in 1 .. reader.ReadInt32() do
                               let key = reader.ReadInt32() |> enum
                               let value = EntityProperties.readFrom reader;
                               yield (key, value) ] |> Map.ofList
        MaxTickCount = reader.ReadInt32()
        MaxPathfindNodes = reader.ReadInt32()
        CurrentTick = reader.ReadInt32()
        Players = [|for _ in 1 .. reader.ReadInt32() do
                      yield Player.readFrom reader; |]
        Entities = [|for _ in 1 .. reader.ReadInt32() do
                       yield Entity.readFrom reader; |]
    }