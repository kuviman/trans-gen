#nowarn "0058"

namespace TransGenTest.Model

open TransGenTest

/// Attack action
type AttackAction = {
    /// If specified, target entity's ID
    Target: option<int>;
    /// If specified, configures auto attacking
    AutoAttack: option<Model.AutoAttack>;
} with

    /// Write AttackAction to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this.Target with
            | Some value ->
                writer.Write true
                writer.Write value
            | None -> writer.Write false
        match this.AutoAttack with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false

    /// Read AttackAction from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Target = match reader.ReadBoolean() with
                     | true -> Some(reader.ReadInt32())
                     | false -> None
        AutoAttack = match reader.ReadBoolean() with
                         | true -> Some(Model.AutoAttack.readFrom reader;)
                         | false -> None
    }