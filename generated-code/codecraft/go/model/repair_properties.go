package model

import "io"
import . "trans_gen_test/stream"

type RepairProperties struct {
    ValidTargets []EntityType
    Power int32
}

func NewRepairProperties(validTargets []EntityType, power int32) RepairProperties {
    return RepairProperties {
        ValidTargets: validTargets,
        Power: power,
    }
}

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

func (repairProperties RepairProperties) Write(writer io.Writer) {
    validTargets := repairProperties.ValidTargets
    WriteInt32(writer, int32(len(validTargets)))
    for _, validTargetsElement := range validTargets {
        WriteInt32(writer, int32(validTargetsElement))
    }
    power := repairProperties.Power
    WriteInt32(writer, power)
}