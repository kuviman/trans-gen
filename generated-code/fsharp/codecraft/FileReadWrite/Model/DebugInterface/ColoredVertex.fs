#nowarn "0058"

namespace TransGenTest.Model.DebugInterface

open TransGenTest

/// Vertex for debug rendering
type ColoredVertex = {
    /// Position in world coordinates (if none, screen position (0, 0) is used)
    WorldPos: option<Vec2Single>;
    /// Additional offset in screen coordinates
    ScreenOffset: Vec2Single;
    /// Color to use
    Color: Color;
} with

    /// Write ColoredVertex to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this.WorldPos with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
        this.ScreenOffset.writeTo writer
        this.Color.writeTo writer

    /// Read ColoredVertex from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        WorldPos = match reader.ReadBoolean() with
                       | true -> Some(Vec2Single.readFrom reader;)
                       | false -> None
        ScreenOffset = Vec2Single.readFrom reader;
        Color = Color.readFrom reader;
    }