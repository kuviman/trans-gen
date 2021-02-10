package model

import "fmt"
import "io"
import . "trans_gen_test/stream"

// Auto attack options
type AutoAttack struct {
    // Maximum distance to pathfind
    PathfindRange int32
    // List of target entity types to try to attack. If empty, all types but resource are considered
    ValidTargets []EntityType
}

func NewAutoAttack(pathfindRange int32, validTargets []EntityType) AutoAttack {
    return AutoAttack {
        PathfindRange: pathfindRange,
        ValidTargets: validTargets,
    }
}

// Read AutoAttack from reader
func ReadAutoAttack(reader io.Reader) AutoAttack {
    var pathfindRange int32
    pathfindRange = ReadInt32(reader)
    var validTargets []EntityType
    validTargets = make([]EntityType, ReadInt32(reader))
    for validTargetsIndex := range validTargets {
        var validTargetsElement EntityType
        validTargetsElement = ReadEntityType(reader)
        validTargets[validTargetsIndex] = validTargetsElement
    }
    return AutoAttack {
        PathfindRange: pathfindRange,
        ValidTargets: validTargets,
    }
}

// Write AutoAttack to writer
func (autoAttack AutoAttack) Write(writer io.Writer) {
    pathfindRange := autoAttack.PathfindRange
    WriteInt32(writer, pathfindRange)
    validTargets := autoAttack.ValidTargets
    WriteInt32(writer, int32(len(validTargets)))
    for _, validTargetsElement := range validTargets {
        WriteInt32(writer, int32(validTargetsElement))
    }
}

// Get string representation of AutoAttack
func (autoAttack AutoAttack) String() string {
    stringResult := "{ "
    stringResult += "PathfindRange: "
    pathfindRange := autoAttack.PathfindRange
    stringResult += fmt.Sprint(pathfindRange)
    stringResult += ", "
    stringResult += "ValidTargets: "
    validTargets := autoAttack.ValidTargets
    stringResult += "[ "
    for validTargetsIndex, validTargetsElement := range validTargets {
        if validTargetsIndex != 0 {
            stringResult += ", "
        }
        stringResult += EntityTypeToString(validTargetsElement)
    }
    stringResult += " ]"
    stringResult += " }"
    return stringResult
}