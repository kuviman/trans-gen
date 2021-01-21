#nowarn "0058"

namespace TransGenTest.Model

type Structure = {
    Text: string;
    FloatNumber: single;
    DoubleNumber: double;
} with

    member this.writeTo(writer: System.IO.BinaryWriter) =
        let textData : byte[] = System.Text.Encoding.UTF8.GetBytes this.Text
        writer.Write textData.Length
        writer.Write textData
        writer.Write this.FloatNumber
        writer.Write this.DoubleNumber

    static member readFrom(reader: System.IO.BinaryReader) = {
        Text = reader.ReadInt32() |> reader.ReadBytes |> System.Text.Encoding.UTF8.GetString
        FloatNumber = reader.ReadSingle()
        DoubleNumber = reader.ReadDouble()
    }