#nowarn "0058"

namespace TransGenTest.Model

open TransGenTest

/// Player's action
type Action = {
    /// New actions for entities. If entity does not get new action, if will continue to perform previously set one
    EntityActions: Map<int, Model.EntityAction>;
} with

    /// Write Action to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.EntityActions.Count
        this.EntityActions |> Map.iter (fun key value ->
            writer.Write key
            value.writeTo writer )

    /// Read Action from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        EntityActions = [for _ in 1 .. reader.ReadInt32() do
                            let key = reader.ReadInt32()
                            let value = Model.EntityAction.readFrom reader;
                            yield (key, value) ] |> Map.ofList
    }