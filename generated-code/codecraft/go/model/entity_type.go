package model

import "io"
import . "trans_gen_test/stream"

// Entity type
type EntityType int32

const (
    // Wall, can be used to prevent enemy from moving through
    EntityTypeWall EntityType = 0
    // House, used to increase population
    EntityTypeHouse EntityType = 1
    // Base for recruiting new builder units
    EntityTypeBuilderBase EntityType = 2
    // Builder unit can build buildings
    EntityTypeBuilderUnit EntityType = 3
    // Base for recruiting new melee units
    EntityTypeMeleeBase EntityType = 4
    // Melee unit
    EntityTypeMeleeUnit EntityType = 5
    // Base for recruiting new ranged units
    EntityTypeRangedBase EntityType = 6
    // Ranged unit
    EntityTypeRangedUnit EntityType = 7
    // Resource can be harvested
    EntityTypeResource EntityType = 8
    // Ranged attacking building
    EntityTypeTurret EntityType = 9
)

// Read EntityType from reader
func ReadEntityType(reader io.Reader) EntityType {
    switch ReadInt32(reader) {
    case 0:
        return EntityTypeWall
    case 1:
        return EntityTypeHouse
    case 2:
        return EntityTypeBuilderBase
    case 3:
        return EntityTypeBuilderUnit
    case 4:
        return EntityTypeMeleeBase
    case 5:
        return EntityTypeMeleeUnit
    case 6:
        return EntityTypeRangedBase
    case 7:
        return EntityTypeRangedUnit
    case 8:
        return EntityTypeResource
    case 9:
        return EntityTypeTurret
    }
    panic("Unexpected tag value")
}

// Get string representation of EntityType
func EntityTypeToString(entityType EntityType) string {
    switch entityType {
    case EntityTypeWall:
        return "Wall"
    case EntityTypeHouse:
        return "House"
    case EntityTypeBuilderBase:
        return "BuilderBase"
    case EntityTypeBuilderUnit:
        return "BuilderUnit"
    case EntityTypeMeleeBase:
        return "MeleeBase"
    case EntityTypeMeleeUnit:
        return "MeleeUnit"
    case EntityTypeRangedBase:
        return "RangedBase"
    case EntityTypeRangedUnit:
        return "RangedUnit"
    case EntityTypeResource:
        return "Resource"
    case EntityTypeTurret:
        return "Turret"
    }
    panic("Impossible happened")
}