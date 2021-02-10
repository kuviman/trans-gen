#nowarn "0058"

namespace TransGenTest

/// First option
type OneOfOptionOne = {
    /// List of integers
    VecInt: int[];
    /// Long integer
    LongInt: int64;
} with

    /// Write OptionOne to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 0
        writer.Write this.VecInt.Length
        this.VecInt |> Array.iter (fun value ->
            writer.Write value )
        writer.Write this.LongInt

    /// Read OptionOne from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        VecInt = [|for _ in 1 .. reader.ReadInt32() do
                     yield reader.ReadInt32() |]
        LongInt = reader.ReadInt64()
    }

/// Second option
type OneOfOptionTwo = {
    /// usize
    Value: int;
} with

    /// Write OptionTwo to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 1
        writer.Write this.Value

    /// Read OptionTwo from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Value = reader.ReadInt32()
    }

/// Oneof example
type OneOf =
    /// First option
    | OptionOne of OneOfOptionOne
    /// Second option
    | OptionTwo of OneOfOptionTwo
    with

    /// Write OneOf to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this with
            | OptionOne value -> value.writeTo writer
            | OptionTwo value -> value.writeTo writer

    /// Read OneOf from reader
    static member readFrom(reader: System.IO.BinaryReader) =
        match reader.ReadInt32() with
            | 0 -> OptionOne (OneOfOptionOne.readFrom reader)
            | 1 -> OptionTwo (OneOfOptionTwo.readFrom reader)
            | x -> failwith (sprintf "Unexpected tag %d" x)