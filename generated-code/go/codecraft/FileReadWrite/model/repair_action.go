package model

import "fmt"
import "io"
import . "trans_gen_test/stream"

// Repair action
type RepairAction struct {
    // Target entity's ID
    Target int32
}

func NewRepairAction(target int32) RepairAction {
    return RepairAction {
        Target: target,
    }
}

// Read RepairAction from reader
func ReadRepairAction(reader io.Reader) RepairAction {
    var target int32
    target = ReadInt32(reader)
    return RepairAction {
        Target: target,
    }
}

// Write RepairAction to writer
func (repairAction RepairAction) Write(writer io.Writer) {
    target := repairAction.Target
    WriteInt32(writer, target)
}

// Get string representation of RepairAction
func (repairAction RepairAction) String() string {
    stringResult := "{ "
    stringResult += "Target: "
    target := repairAction.Target
    stringResult += fmt.Sprint(target)
    stringResult += " }"
    return stringResult
}