#nowarn "0058"

namespace TransGenTest.Model

/// Entity's repair properties
type RepairProperties = {
    /// Valid target entity types
    ValidTargets: EntityType[];
    /// Health restored in one tick
    Power: int;
} with

    /// Write RepairProperties to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.ValidTargets.Length
        this.ValidTargets |> Array.iter (fun value ->
            writer.Write (int value) )
        writer.Write this.Power

    /// Read RepairProperties from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        ValidTargets = [|for _ in 1 .. reader.ReadInt32() do
                           yield reader.ReadInt32() |> enum |]
        Power = reader.ReadInt32()
    }