package model

import "fmt"
import "io"
import . "trans_gen_test/stream"

// Attack action
type AttackAction struct {
    // If specified, target entity's ID
    Target *int32
    // If specified, configures auto attacking
    AutoAttack *AutoAttack
}

func NewAttackAction(target *int32, autoAttack *AutoAttack) AttackAction {
    return AttackAction {
        Target: target,
        AutoAttack: autoAttack,
    }
}

// Read AttackAction from reader
func ReadAttackAction(reader io.Reader) AttackAction {
    var target *int32
    if ReadBool(reader) {
        var targetValue int32
        targetValue = ReadInt32(reader)
        target = &targetValue
    } else {
        target = nil
    }
    var autoAttack *AutoAttack
    if ReadBool(reader) {
        var autoAttackValue AutoAttack
        autoAttackValue = ReadAutoAttack(reader)
        autoAttack = &autoAttackValue
    } else {
        autoAttack = nil
    }
    return AttackAction {
        Target: target,
        AutoAttack: autoAttack,
    }
}

// Write AttackAction to writer
func (attackAction AttackAction) Write(writer io.Writer) {
    target := attackAction.Target
    if target == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        targetValue := *target
        WriteInt32(writer, targetValue)
    }
    autoAttack := attackAction.AutoAttack
    if autoAttack == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        autoAttackValue := *autoAttack
        autoAttackValue.Write(writer)
    }
}

// Get string representation of AttackAction
func (attackAction AttackAction) String() string {
    stringResult := "{ "
    stringResult += "Target: "
    target := attackAction.Target
    if target == nil {
        stringResult += "nil"
    } else {
        targetValue := *target
        stringResult += fmt.Sprint(targetValue)
    }
    stringResult += ", "
    stringResult += "AutoAttack: "
    autoAttack := attackAction.AutoAttack
    if autoAttack == nil {
        stringResult += "nil"
    } else {
        autoAttackValue := *autoAttack
        stringResult += autoAttackValue.String()
    }
    stringResult += " }"
    return stringResult
}