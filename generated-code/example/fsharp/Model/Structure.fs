#nowarn "0058"

namespace TransGenTest.Model

/// Example structure
type Structure = {
    /// Text
    Text: string;
    /// 32-bit float
    FloatNumber: single;
    /// 64-bit float
    DoubleNumber: double;
} with

    /// Write Structure to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        let textData : byte[] = System.Text.Encoding.UTF8.GetBytes this.Text
        writer.Write textData.Length
        writer.Write textData
        writer.Write this.FloatNumber
        writer.Write this.DoubleNumber

    /// Read Structure from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Text = reader.ReadInt32() |> reader.ReadBytes |> System.Text.Encoding.UTF8.GetString
        FloatNumber = reader.ReadSingle()
        DoubleNumber = reader.ReadDouble()
    }