#nowarn "0058"

namespace TransGenTest

/// 2 dimensional vector.
type Vec2Single = {
    /// `x` coordinate of the vector
    X: single;
    /// `y` coordinate of the vector
    Y: single;
} with

    /// Write Vec2Single to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.X
        writer.Write this.Y

    /// Read Vec2Single from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        X = reader.ReadSingle()
        Y = reader.ReadSingle()
    }