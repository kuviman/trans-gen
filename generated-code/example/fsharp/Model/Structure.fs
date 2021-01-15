#nowarn "0058"
namespace TransGenTest.Model
type Structure = {
    OneOfOne: OneOf;
    OneOfTwo: OneOf;
    HashMap: Map<Enumeration, int>;
    Text: string;
    RealNumber: double;
    } with
    member this.writeTo(writer: System.IO.BinaryWriter) =
        this.OneOfOne.writeTo writer
        this.OneOfTwo.writeTo writer
        writer.Write this.HashMap.Count
        this.HashMap |> Map.iter (fun key value ->
            writer.Write (int key)
            writer.Write value
        )
        let TextData : byte[] = System.Text.Encoding.UTF8.GetBytes this.Text
        writer.Write TextData.Length
        writer.Write TextData
        writer.Write this.RealNumber
    static member readFrom(reader: System.IO.BinaryReader) = {
        OneOfOne = OneOf.readFrom reader
        OneOfTwo = OneOf.readFrom reader
        HashMap = [for _ in 1 .. reader.ReadInt32() do
            let key = reader.ReadInt32() |> enum
            let value = reader.ReadInt32()
            yield (key, value)
            ] |> Map.ofList
        Text = reader.ReadInt32() |> reader.ReadBytes |> System.Text.Encoding.UTF8.GetString
        RealNumber = reader.ReadDouble()
    }
