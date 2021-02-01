package model

import "io"
import . "trans_gen_test/stream"
import "fmt"

// Entity's repair properties
type RepairProperties struct {
    // Valid target entity types
    ValidTargets []EntityType
    // Health restored in one tick
    Power int32
}

func NewRepairProperties(validTargets []EntityType, power int32) RepairProperties {
    return RepairProperties {
        ValidTargets: validTargets,
        Power: power,
    }
}

// Read RepairProperties from reader
func ReadRepairProperties(reader io.Reader) RepairProperties {
    var validTargets []EntityType
    validTargets = make([]EntityType, ReadInt32(reader))
    for validTargetsIndex := range validTargets {
        var validTargetsElement EntityType
        validTargetsElement = ReadEntityType(reader)
        validTargets[validTargetsIndex] = validTargetsElement
    }
    var power int32
    power = ReadInt32(reader)
    return RepairProperties {
        ValidTargets: validTargets,
        Power: power,
    }
}

// Write RepairProperties to writer
func (repairProperties RepairProperties) Write(writer io.Writer) {
    validTargets := repairProperties.ValidTargets
    WriteInt32(writer, int32(len(validTargets)))
    for _, validTargetsElement := range validTargets {
        WriteInt32(writer, int32(validTargetsElement))
    }
    power := repairProperties.Power
    WriteInt32(writer, power)
}

// Get string representation of RepairProperties
func (repairProperties RepairProperties) String() string {
    stringResult := "{ "
    stringResult += "ValidTargets: "
    validTargets := repairProperties.ValidTargets
    stringResult += "[ "
    for validTargetsIndex, validTargetsElement := range validTargets {
        if validTargetsIndex != 0 {
            stringResult += ", "
        }
        stringResult += EntityTypeToString(validTargetsElement)
    }
    stringResult += " ]"
    stringResult += ", "
    stringResult += "Power: "
    power := repairProperties.Power
    stringResult += fmt.Sprint(power)
    stringResult += " }"
    return stringResult
}