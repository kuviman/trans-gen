#nowarn "0058"

namespace TransGenTest.Model

open TransGenTest

/// Repair action
type RepairAction = {
    /// Target entity's ID
    Target: int;
} with

    /// Write RepairAction to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write this.Target

    /// Read RepairAction from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Target = reader.ReadInt32()
    }