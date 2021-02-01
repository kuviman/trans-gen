package model

import "io"
import . "trans_gen_test/stream"
import "fmt"

// Entity properties
type EntityProperties struct {
    // Size. Entity has a form of a square with side of this length
    Size int32
    // Score for building this entity
    BuildScore int32
    // Score for destroying this entity
    DestroyScore int32
    // Whether this entity can move
    CanMove bool
    // Number of population points this entity provides, if active
    PopulationProvide int32
    // Number of population points this entity uses
    PopulationUse int32
    // Maximum health points
    MaxHealth int32
    // Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
    InitialCost int32
    // If fog of war is enabled, maximum distance at which other entities are considered visible
    SightRange int32
    // Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
    ResourcePerHealth int32
    // Build properties, if entity can build
    Build *BuildProperties
    // Attack properties, if entity can attack
    Attack *AttackProperties
    // Repair properties, if entity can repair
    Repair *RepairProperties
}

func NewEntityProperties(size int32, buildScore int32, destroyScore int32, canMove bool, populationProvide int32, populationUse int32, maxHealth int32, initialCost int32, sightRange int32, resourcePerHealth int32, build *BuildProperties, attack *AttackProperties, repair *RepairProperties) EntityProperties {
    return EntityProperties {
        Size: size,
        BuildScore: buildScore,
        DestroyScore: destroyScore,
        CanMove: canMove,
        PopulationProvide: populationProvide,
        PopulationUse: populationUse,
        MaxHealth: maxHealth,
        InitialCost: initialCost,
        SightRange: sightRange,
        ResourcePerHealth: resourcePerHealth,
        Build: build,
        Attack: attack,
        Repair: repair,
    }
}

// Read EntityProperties from reader
func ReadEntityProperties(reader io.Reader) EntityProperties {
    var size int32
    size = ReadInt32(reader)
    var buildScore int32
    buildScore = ReadInt32(reader)
    var destroyScore int32
    destroyScore = ReadInt32(reader)
    var canMove bool
    canMove = ReadBool(reader)
    var populationProvide int32
    populationProvide = ReadInt32(reader)
    var populationUse int32
    populationUse = ReadInt32(reader)
    var maxHealth int32
    maxHealth = ReadInt32(reader)
    var initialCost int32
    initialCost = ReadInt32(reader)
    var sightRange int32
    sightRange = ReadInt32(reader)
    var resourcePerHealth int32
    resourcePerHealth = ReadInt32(reader)
    var build *BuildProperties
    if ReadBool(reader) {
        var buildValue BuildProperties
        buildValue = ReadBuildProperties(reader)
        build = &buildValue
    } else {
        build = nil
    }
    var attack *AttackProperties
    if ReadBool(reader) {
        var attackValue AttackProperties
        attackValue = ReadAttackProperties(reader)
        attack = &attackValue
    } else {
        attack = nil
    }
    var repair *RepairProperties
    if ReadBool(reader) {
        var repairValue RepairProperties
        repairValue = ReadRepairProperties(reader)
        repair = &repairValue
    } else {
        repair = nil
    }
    return EntityProperties {
        Size: size,
        BuildScore: buildScore,
        DestroyScore: destroyScore,
        CanMove: canMove,
        PopulationProvide: populationProvide,
        PopulationUse: populationUse,
        MaxHealth: maxHealth,
        InitialCost: initialCost,
        SightRange: sightRange,
        ResourcePerHealth: resourcePerHealth,
        Build: build,
        Attack: attack,
        Repair: repair,
    }
}

// Write EntityProperties to writer
func (entityProperties EntityProperties) Write(writer io.Writer) {
    size := entityProperties.Size
    WriteInt32(writer, size)
    buildScore := entityProperties.BuildScore
    WriteInt32(writer, buildScore)
    destroyScore := entityProperties.DestroyScore
    WriteInt32(writer, destroyScore)
    canMove := entityProperties.CanMove
    WriteBool(writer, canMove)
    populationProvide := entityProperties.PopulationProvide
    WriteInt32(writer, populationProvide)
    populationUse := entityProperties.PopulationUse
    WriteInt32(writer, populationUse)
    maxHealth := entityProperties.MaxHealth
    WriteInt32(writer, maxHealth)
    initialCost := entityProperties.InitialCost
    WriteInt32(writer, initialCost)
    sightRange := entityProperties.SightRange
    WriteInt32(writer, sightRange)
    resourcePerHealth := entityProperties.ResourcePerHealth
    WriteInt32(writer, resourcePerHealth)
    build := entityProperties.Build
    if build == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        buildValue := *build
        buildValue.Write(writer)
    }
    attack := entityProperties.Attack
    if attack == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        attackValue := *attack
        attackValue.Write(writer)
    }
    repair := entityProperties.Repair
    if repair == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        repairValue := *repair
        repairValue.Write(writer)
    }
}

// Get string representation of EntityProperties
func (entityProperties EntityProperties) String() string {
    stringResult := "{ "
    stringResult += "Size: "
    size := entityProperties.Size
    stringResult += fmt.Sprint(size)
    stringResult += ", "
    stringResult += "BuildScore: "
    buildScore := entityProperties.BuildScore
    stringResult += fmt.Sprint(buildScore)
    stringResult += ", "
    stringResult += "DestroyScore: "
    destroyScore := entityProperties.DestroyScore
    stringResult += fmt.Sprint(destroyScore)
    stringResult += ", "
    stringResult += "CanMove: "
    canMove := entityProperties.CanMove
    stringResult += fmt.Sprint(canMove)
    stringResult += ", "
    stringResult += "PopulationProvide: "
    populationProvide := entityProperties.PopulationProvide
    stringResult += fmt.Sprint(populationProvide)
    stringResult += ", "
    stringResult += "PopulationUse: "
    populationUse := entityProperties.PopulationUse
    stringResult += fmt.Sprint(populationUse)
    stringResult += ", "
    stringResult += "MaxHealth: "
    maxHealth := entityProperties.MaxHealth
    stringResult += fmt.Sprint(maxHealth)
    stringResult += ", "
    stringResult += "InitialCost: "
    initialCost := entityProperties.InitialCost
    stringResult += fmt.Sprint(initialCost)
    stringResult += ", "
    stringResult += "SightRange: "
    sightRange := entityProperties.SightRange
    stringResult += fmt.Sprint(sightRange)
    stringResult += ", "
    stringResult += "ResourcePerHealth: "
    resourcePerHealth := entityProperties.ResourcePerHealth
    stringResult += fmt.Sprint(resourcePerHealth)
    stringResult += ", "
    stringResult += "Build: "
    build := entityProperties.Build
    if build == nil {
        stringResult += "nil"
    } else {
        buildValue := *build
        stringResult += buildValue.String()
    }
    stringResult += ", "
    stringResult += "Attack: "
    attack := entityProperties.Attack
    if attack == nil {
        stringResult += "nil"
    } else {
        attackValue := *attack
        stringResult += attackValue.String()
    }
    stringResult += ", "
    stringResult += "Repair: "
    repair := entityProperties.Repair
    if repair == nil {
        stringResult += "nil"
    } else {
        repairValue := *repair
        stringResult += repairValue.String()
    }
    stringResult += " }"
    return stringResult
}