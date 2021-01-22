package model

import "io"
import . "trans_gen_test/stream"
import "fmt"

type EntityProperties struct {
    Size int32
    BuildScore int32
    DestroyScore int32
    CanMove bool
    PopulationProvide int32
    PopulationUse int32
    MaxHealth int32
    InitialCost int32
    SightRange int32
    ResourcePerHealth int32
    Build *BuildProperties
    Attack *AttackProperties
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