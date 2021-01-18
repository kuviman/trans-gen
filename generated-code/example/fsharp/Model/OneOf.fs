#nowarn "0058"
namespace TransGenTest.Model

type OneOfOptionOne = {
    VecInt: int[];
    LongInt: int64;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 0
        writer.Write this.VecInt.Length
        this.VecInt |> Array.iter (fun value ->
            writer.Write value
        )
        writer.Write this.LongInt
    static member readFrom(reader: System.IO.BinaryReader) = {
        VecInt = [|for _ in 1 .. reader.ReadInt32() do
            yield reader.ReadInt32()
        |]
        LongInt = reader.ReadInt64()
    }

type OneOfOptionTwo = {
    Value: int;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 1
        writer.Write this.Value
    static member readFrom(reader: System.IO.BinaryReader) = {
        Value = reader.ReadInt32()
    }
type OneOf = 
    | OptionOne of OneOfOptionOne
    | OptionTwo of OneOfOptionTwo
    with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this with
            | OptionOne value -> value.writeTo writer
            | OptionTwo value -> value.writeTo writer
    static member readFrom(reader: System.IO.BinaryReader) =
        match reader.ReadInt32() with
            | 0 -> OptionOne (OneOfOptionOne.readFrom reader)
            | 1 -> OptionTwo (OneOfOptionTwo.readFrom reader)
            | x -> failwith (sprintf "Unexpected tag %d" x)
