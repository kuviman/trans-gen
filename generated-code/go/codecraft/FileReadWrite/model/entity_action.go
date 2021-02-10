package model

import "io"
import . "trans_gen_test/stream"

// Entity's action
type EntityAction struct {
    // Move action
    MoveAction *MoveAction
    // Build action
    BuildAction *BuildAction
    // Attack action
    AttackAction *AttackAction
    // Repair action
    RepairAction *RepairAction
}

func NewEntityAction(moveAction *MoveAction, buildAction *BuildAction, attackAction *AttackAction, repairAction *RepairAction) EntityAction {
    return EntityAction {
        MoveAction: moveAction,
        BuildAction: buildAction,
        AttackAction: attackAction,
        RepairAction: repairAction,
    }
}

// Read EntityAction from reader
func ReadEntityAction(reader io.Reader) EntityAction {
    var moveAction *MoveAction
    if ReadBool(reader) {
        var moveActionValue MoveAction
        moveActionValue = ReadMoveAction(reader)
        moveAction = &moveActionValue
    } else {
        moveAction = nil
    }
    var buildAction *BuildAction
    if ReadBool(reader) {
        var buildActionValue BuildAction
        buildActionValue = ReadBuildAction(reader)
        buildAction = &buildActionValue
    } else {
        buildAction = nil
    }
    var attackAction *AttackAction
    if ReadBool(reader) {
        var attackActionValue AttackAction
        attackActionValue = ReadAttackAction(reader)
        attackAction = &attackActionValue
    } else {
        attackAction = nil
    }
    var repairAction *RepairAction
    if ReadBool(reader) {
        var repairActionValue RepairAction
        repairActionValue = ReadRepairAction(reader)
        repairAction = &repairActionValue
    } else {
        repairAction = nil
    }
    return EntityAction {
        MoveAction: moveAction,
        BuildAction: buildAction,
        AttackAction: attackAction,
        RepairAction: repairAction,
    }
}

// Write EntityAction to writer
func (entityAction EntityAction) Write(writer io.Writer) {
    moveAction := entityAction.MoveAction
    if moveAction == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        moveActionValue := *moveAction
        moveActionValue.Write(writer)
    }
    buildAction := entityAction.BuildAction
    if buildAction == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        buildActionValue := *buildAction
        buildActionValue.Write(writer)
    }
    attackAction := entityAction.AttackAction
    if attackAction == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        attackActionValue := *attackAction
        attackActionValue.Write(writer)
    }
    repairAction := entityAction.RepairAction
    if repairAction == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        repairActionValue := *repairAction
        repairActionValue.Write(writer)
    }
}

// Get string representation of EntityAction
func (entityAction EntityAction) String() string {
    stringResult := "{ "
    stringResult += "MoveAction: "
    moveAction := entityAction.MoveAction
    if moveAction == nil {
        stringResult += "nil"
    } else {
        moveActionValue := *moveAction
        stringResult += moveActionValue.String()
    }
    stringResult += ", "
    stringResult += "BuildAction: "
    buildAction := entityAction.BuildAction
    if buildAction == nil {
        stringResult += "nil"
    } else {
        buildActionValue := *buildAction
        stringResult += buildActionValue.String()
    }
    stringResult += ", "
    stringResult += "AttackAction: "
    attackAction := entityAction.AttackAction
    if attackAction == nil {
        stringResult += "nil"
    } else {
        attackActionValue := *attackAction
        stringResult += attackActionValue.String()
    }
    stringResult += ", "
    stringResult += "RepairAction: "
    repairAction := entityAction.RepairAction
    if repairAction == nil {
        stringResult += "nil"
    } else {
        repairActionValue := *repairAction
        stringResult += repairActionValue.String()
    }
    stringResult += " }"
    return stringResult
}