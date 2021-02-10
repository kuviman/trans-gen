package model

import "fmt"
import "io"
import . "trans_gen_test/stream"

// Entity's attack properties
type AttackProperties struct {
    // Maximum attack range
    AttackRange int32
    // Damage dealt in one tick
    Damage int32
    // If true, dealing damage will collect resource from target
    CollectResource bool
}

func NewAttackProperties(attackRange int32, damage int32, collectResource bool) AttackProperties {
    return AttackProperties {
        AttackRange: attackRange,
        Damage: damage,
        CollectResource: collectResource,
    }
}

// Read AttackProperties from reader
func ReadAttackProperties(reader io.Reader) AttackProperties {
    var attackRange int32
    attackRange = ReadInt32(reader)
    var damage int32
    damage = ReadInt32(reader)
    var collectResource bool
    collectResource = ReadBool(reader)
    return AttackProperties {
        AttackRange: attackRange,
        Damage: damage,
        CollectResource: collectResource,
    }
}

// Write AttackProperties to writer
func (attackProperties AttackProperties) Write(writer io.Writer) {
    attackRange := attackProperties.AttackRange
    WriteInt32(writer, attackRange)
    damage := attackProperties.Damage
    WriteInt32(writer, damage)
    collectResource := attackProperties.CollectResource
    WriteBool(writer, collectResource)
}

// Get string representation of AttackProperties
func (attackProperties AttackProperties) String() string {
    stringResult := "{ "
    stringResult += "AttackRange: "
    attackRange := attackProperties.AttackRange
    stringResult += fmt.Sprint(attackRange)
    stringResult += ", "
    stringResult += "Damage: "
    damage := attackProperties.Damage
    stringResult += fmt.Sprint(damage)
    stringResult += ", "
    stringResult += "CollectResource: "
    collectResource := attackProperties.CollectResource
    stringResult += fmt.Sprint(collectResource)
    stringResult += " }"
    return stringResult
}