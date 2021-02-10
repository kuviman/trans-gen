#nowarn "0058"

namespace TransGenTest

/// RGBA Color
type Color = {
    /// Red component
    R: single;
    /// Green component
    G: single;
    /// Blue component
    B: single;
    /// Alpha (opacity) component
    A: single;
} with

    /// Write Color to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.R
        writer.Write this.G
        writer.Write this.B
        writer.Write this.A

    /// Read Color from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        R = reader.ReadSingle()
        G = reader.ReadSingle()
        B = reader.ReadSingle()
        A = reader.ReadSingle()
    }