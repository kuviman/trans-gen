#nowarn "0058"

namespace TransGenTest.Model.DebugInterface

open TransGenTest

/// Log some text
type DebugDataLog = {
    /// Text to show
    Text: string;
} with

    /// Write Log to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 0
        let textData : byte[] = System.Text.Encoding.UTF8.GetBytes this.Text
        writer.Write textData.Length
        writer.Write textData

    /// Read Log from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Text = reader.ReadInt32() |> reader.ReadBytes |> System.Text.Encoding.UTF8.GetString
    }

/// Draw primitives
type DebugDataPrimitives = {
    /// Vertices
    Vertices: Model.DebugInterface.ColoredVertex[];
    /// Primitive type
    PrimitiveType: Model.DebugInterface.PrimitiveType;
} with

    /// Write Primitives to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 1
        writer.Write this.Vertices.Length
        this.Vertices |> Array.iter (fun value ->
            value.writeTo writer )
        writer.Write (int this.PrimitiveType)

    /// Read Primitives from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Vertices = [|for _ in 1 .. reader.ReadInt32() do
                       yield Model.DebugInterface.ColoredVertex.readFrom reader; |]
        PrimitiveType = reader.ReadInt32() |> enum
    }

/// Draw text
type DebugDataPlacedText = {
    /// Vertex to determine text position and color
    Vertex: Model.DebugInterface.ColoredVertex;
    /// Text
    Text: string;
    /// Text alignment (0 means left, 0.5 means center, 1 means right)
    Alignment: single;
    /// Font size in pixels
    Size: single;
} with

    /// Write PlacedText to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 2
        this.Vertex.writeTo writer
        let textData : byte[] = System.Text.Encoding.UTF8.GetBytes this.Text
        writer.Write textData.Length
        writer.Write textData
        writer.Write this.Alignment
        writer.Write this.Size

    /// Read PlacedText from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Vertex = Model.DebugInterface.ColoredVertex.readFrom reader;
        Text = reader.ReadInt32() |> reader.ReadBytes |> System.Text.Encoding.UTF8.GetString
        Alignment = reader.ReadSingle()
        Size = reader.ReadSingle()
    }

/// Debug data can be drawn in the app
type DebugData =
    /// Log some text
    | Log of DebugDataLog
    /// Draw primitives
    | Primitives of DebugDataPrimitives
    /// Draw text
    | PlacedText of DebugDataPlacedText
    with

    /// Write DebugData to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this with
            | Log value -> value.writeTo writer
            | Primitives value -> value.writeTo writer
            | PlacedText value -> value.writeTo writer

    /// Read DebugData from reader
    static member readFrom(reader: System.IO.BinaryReader) =
        match reader.ReadInt32() with
            | 0 -> Log (DebugDataLog.readFrom reader)
            | 1 -> Primitives (DebugDataPrimitives.readFrom reader)
            | 2 -> PlacedText (DebugDataPlacedText.readFrom reader)
            | x -> failwith (sprintf "Unexpected tag %d" x)