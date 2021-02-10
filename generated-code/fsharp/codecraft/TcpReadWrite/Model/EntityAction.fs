#nowarn "0058"

namespace TransGenTest.Model

open TransGenTest

/// Entity's action
type EntityAction = {
    /// Move action
    MoveAction: option<Model.MoveAction>;
    /// Build action
    BuildAction: option<Model.BuildAction>;
    /// Attack action
    AttackAction: option<Model.AttackAction>;
    /// Repair action
    RepairAction: option<Model.RepairAction>;
} with

    /// Write EntityAction to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this.MoveAction with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
        match this.BuildAction with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
        match this.AttackAction with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false
        match this.RepairAction with
            | Some value ->
                writer.Write true
                value.writeTo writer
            | None -> writer.Write false

    /// Read EntityAction from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        MoveAction = match reader.ReadBoolean() with
                         | true -> Some(Model.MoveAction.readFrom reader;)
                         | false -> None
        BuildAction = match reader.ReadBoolean() with
                          | true -> Some(Model.BuildAction.readFrom reader;)
                          | false -> None
        AttackAction = match reader.ReadBoolean() with
                           | true -> Some(Model.AttackAction.readFrom reader;)
                           | false -> None
        RepairAction = match reader.ReadBoolean() with
                           | true -> Some(Model.RepairAction.readFrom reader;)
                           | false -> None
    }