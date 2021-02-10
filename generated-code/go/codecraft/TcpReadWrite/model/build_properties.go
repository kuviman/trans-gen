package model

import "fmt"
import "io"
import . "trans_gen_test/stream"

// Entity's build properties
type BuildProperties struct {
    // Valid new entity types
    Options []EntityType
    // Initial health of new entity. If absent, it will have full health
    InitHealth *int32
}

func NewBuildProperties(options []EntityType, initHealth *int32) BuildProperties {
    return BuildProperties {
        Options: options,
        InitHealth: initHealth,
    }
}

// Read BuildProperties from reader
func ReadBuildProperties(reader io.Reader) BuildProperties {
    var options []EntityType
    options = make([]EntityType, ReadInt32(reader))
    for optionsIndex := range options {
        var optionsElement EntityType
        optionsElement = ReadEntityType(reader)
        options[optionsIndex] = optionsElement
    }
    var initHealth *int32
    if ReadBool(reader) {
        var initHealthValue int32
        initHealthValue = ReadInt32(reader)
        initHealth = &initHealthValue
    } else {
        initHealth = nil
    }
    return BuildProperties {
        Options: options,
        InitHealth: initHealth,
    }
}

// Write BuildProperties to writer
func (buildProperties BuildProperties) Write(writer io.Writer) {
    options := buildProperties.Options
    WriteInt32(writer, int32(len(options)))
    for _, optionsElement := range options {
        WriteInt32(writer, int32(optionsElement))
    }
    initHealth := buildProperties.InitHealth
    if initHealth == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        initHealthValue := *initHealth
        WriteInt32(writer, initHealthValue)
    }
}

// Get string representation of BuildProperties
func (buildProperties BuildProperties) String() string {
    stringResult := "{ "
    stringResult += "Options: "
    options := buildProperties.Options
    stringResult += "[ "
    for optionsIndex, optionsElement := range options {
        if optionsIndex != 0 {
            stringResult += ", "
        }
        stringResult += EntityTypeToString(optionsElement)
    }
    stringResult += " ]"
    stringResult += ", "
    stringResult += "InitHealth: "
    initHealth := buildProperties.InitHealth
    if initHealth == nil {
        stringResult += "nil"
    } else {
        initHealthValue := *initHealth
        stringResult += fmt.Sprint(initHealthValue)
    }
    stringResult += " }"
    return stringResult
}