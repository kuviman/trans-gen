#nowarn "0058"

namespace TransGenTest.Model

/// Entity's build properties
type BuildProperties = {
    /// Valid new entity types
    Options: EntityType[];
    /// Initial health of new entity. If absent, it will have full health
    InitHealth: option<int>;
} with

    /// Write BuildProperties to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.Options.Length
        this.Options |> Array.iter (fun value ->
            writer.Write (int value) )
        match this.InitHealth with
            | Some value ->
                writer.Write true
                writer.Write value
            | None -> writer.Write false

    /// Read BuildProperties from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Options = [|for _ in 1 .. reader.ReadInt32() do
                      yield reader.ReadInt32() |> enum |]
        InitHealth = match reader.ReadBoolean() with
                         | true -> Some(reader.ReadInt32())
                         | false -> None
    }