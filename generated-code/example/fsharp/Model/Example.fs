#nowarn "0058"

namespace TransGenTest.Model

type Example = {
    OneOf: OneOf;
    HashMap: Map<Enumeration, int>;
    OptionalInt: option<int>;
    OptionalBool: option<bool>;
    OptionalOneOf: option<OneOf>;
    OptionalStruct: option<Structure>;
    OptionalEnum: option<Enumeration>;
} with

    member this.writeTo(writer: System.IO.BinaryWriter) =
        this.OneOf.writeTo writer
        writer.Write this.HashMap.Count
        this.HashMap |> Map.iter (fun key value ->
            writer.Write (int key)
            writer.Write value )
        match this.OptionalInt with
            | Some value ->
                writer.Write true
                writer.Write value
            | None -> writer.Write false
        match this.OptionalBool with
            | Some value ->
                writer.Write true
                writer.Write value
            | None -> writer.Write false
        match this.OptionalOneOf with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
        match this.OptionalStruct with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
        match this.OptionalEnum with
            | Some value ->
                writer.Write true
                writer.Write (int value)
            | None -> writer.Write false

    static member readFrom(reader: System.IO.BinaryReader) = {
        OneOf = OneOf.readFrom reader;
        HashMap = [for _ in 1 .. reader.ReadInt32() do
                      let key = reader.ReadInt32() |> enum
                      let value = reader.ReadInt32()
                      yield (key, value) ] |> Map.ofList
        OptionalInt = match reader.ReadBoolean() with
                          | true -> Some(reader.ReadInt32())
                          | false -> None
        OptionalBool = match reader.ReadBoolean() with
                           | true -> Some(reader.ReadBoolean())
                           | false -> None
        OptionalOneOf = match reader.ReadBoolean() with
                            | true -> Some(OneOf.readFrom reader;)
                            | false -> None
        OptionalStruct = match reader.ReadBoolean() with
                             | true -> Some(Structure.readFrom reader;)
                             | false -> None
        OptionalEnum = match reader.ReadBoolean() with
                           | true -> Some(reader.ReadInt32() |> enum)
                           | false -> None
    }