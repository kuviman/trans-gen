#nowarn "0058"

namespace TransGenTest.Model

/// 2 dimensional vector.
type Vec2Int = {
    /// `x` coordinate of the vector
    X: int;
    /// `y` coordinate of the vector
    Y: int;
} with

    /// Write Vec2Int to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.X
        writer.Write this.Y

    /// Read Vec2Int from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        X = reader.ReadInt32()
        Y = reader.ReadInt32()
    }