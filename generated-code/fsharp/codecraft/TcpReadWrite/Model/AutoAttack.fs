#nowarn "0058"

namespace TransGenTest.Model

open TransGenTest

/// Auto attack options
type AutoAttack = {
    /// Maximum distance to pathfind
    PathfindRange: int;
    /// List of target entity types to try to attack. If empty, all types but resource are considered
    ValidTargets: Model.EntityType[];
} with

    /// Write AutoAttack to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.PathfindRange
        writer.Write this.ValidTargets.Length
        this.ValidTargets |> Array.iter (fun value ->
            writer.Write (int value) )

    /// Read AutoAttack from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        PathfindRange = reader.ReadInt32()
        ValidTargets = [|for _ in 1 .. reader.ReadInt32() do
                           yield reader.ReadInt32() |> enum |]
    }