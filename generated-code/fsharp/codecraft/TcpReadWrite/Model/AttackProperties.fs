#nowarn "0058"

namespace TransGenTest.Model

open TransGenTest

/// Entity's attack properties
type AttackProperties = {
    /// Maximum attack range
    AttackRange: int;
    /// Damage dealt in one tick
    Damage: int;
    /// If true, dealing damage will collect resource from target
    CollectResource: bool;
} with

    /// Write AttackProperties to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.AttackRange
        writer.Write this.Damage
        writer.Write this.CollectResource

    /// Read AttackProperties from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        AttackRange = reader.ReadInt32()
        Damage = reader.ReadInt32()
        CollectResource = reader.ReadBoolean()
    }