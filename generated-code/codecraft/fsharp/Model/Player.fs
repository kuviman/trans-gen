#nowarn "0058"

namespace TransGenTest.Model

/// Player (strategy, client)
type Player = {
    /// Player's ID
    Id: int;
    /// Current score
    Score: int;
    /// Current amount of resource
    Resource: int;
} with

    /// Write Player to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.Id
        writer.Write this.Score
        writer.Write this.Resource

    /// Read Player from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Id = reader.ReadInt32()
        Score = reader.ReadInt32()
        Resource = reader.ReadInt32()
    }