package model

import "io"
import . "trans_gen_test/stream"
import "fmt"

// Example
type Example struct {
    // OneOf
    OneOf OneOf
    // Dictionary
    HashMap map[Enumeration]int32
    // Optional int
    OptionalInt *int32
    // Optional boolean
    OptionalBool *bool
    // Optional OneOf
    OptionalOneOf *OneOf
    // Optional struct
    OptionalStruct *Structure
    // Optional enum
    OptionalEnum *Enumeration
}

func NewExample(oneOf OneOf, hashMap map[Enumeration]int32, optionalInt *int32, optionalBool *bool, optionalOneOf *OneOf, optionalStruct *Structure, optionalEnum *Enumeration) Example {
    return Example {
        OneOf: oneOf,
        HashMap: hashMap,
        OptionalInt: optionalInt,
        OptionalBool: optionalBool,
        OptionalOneOf: optionalOneOf,
        OptionalStruct: optionalStruct,
        OptionalEnum: optionalEnum,
    }
}

// Read Example from reader
func ReadExample(reader io.Reader) Example {
    var oneOf OneOf
    oneOf = ReadOneOf(reader)
    var hashMap map[Enumeration]int32
    hashMapSize := ReadInt32(reader)
    hashMap = make(map[Enumeration]int32)
    for hashMapIndex := int32(0); hashMapIndex < hashMapSize; hashMapIndex++ {
        var hashMapKey Enumeration
        hashMapKey = ReadEnumeration(reader)
        var hashMapValue int32
        hashMapValue = ReadInt32(reader)
        hashMap[hashMapKey] = hashMapValue
    }
    var optionalInt *int32
    if ReadBool(reader) {
        var optionalIntValue int32
        optionalIntValue = ReadInt32(reader)
        optionalInt = &optionalIntValue
    } else {
        optionalInt = nil
    }
    var optionalBool *bool
    if ReadBool(reader) {
        var optionalBoolValue bool
        optionalBoolValue = ReadBool(reader)
        optionalBool = &optionalBoolValue
    } else {
        optionalBool = nil
    }
    var optionalOneOf *OneOf
    if ReadBool(reader) {
        var optionalOneOfValue OneOf
        optionalOneOfValue = ReadOneOf(reader)
        optionalOneOf = &optionalOneOfValue
    } else {
        optionalOneOf = nil
    }
    var optionalStruct *Structure
    if ReadBool(reader) {
        var optionalStructValue Structure
        optionalStructValue = ReadStructure(reader)
        optionalStruct = &optionalStructValue
    } else {
        optionalStruct = nil
    }
    var optionalEnum *Enumeration
    if ReadBool(reader) {
        var optionalEnumValue Enumeration
        optionalEnumValue = ReadEnumeration(reader)
        optionalEnum = &optionalEnumValue
    } else {
        optionalEnum = nil
    }
    return Example {
        OneOf: oneOf,
        HashMap: hashMap,
        OptionalInt: optionalInt,
        OptionalBool: optionalBool,
        OptionalOneOf: optionalOneOf,
        OptionalStruct: optionalStruct,
        OptionalEnum: optionalEnum,
    }
}

// Write Example to writer
func (example Example) Write(writer io.Writer) {
    oneOf := example.OneOf
    oneOf.Write(writer)
    hashMap := example.HashMap
    WriteInt32(writer, int32(len(hashMap)))
    for hashMapKey, hashMapValue := range hashMap {
        WriteInt32(writer, int32(hashMapKey))
        WriteInt32(writer, hashMapValue)
    }
    optionalInt := example.OptionalInt
    if optionalInt == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        optionalIntValue := *optionalInt
        WriteInt32(writer, optionalIntValue)
    }
    optionalBool := example.OptionalBool
    if optionalBool == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        optionalBoolValue := *optionalBool
        WriteBool(writer, optionalBoolValue)
    }
    optionalOneOf := example.OptionalOneOf
    if optionalOneOf == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        optionalOneOfValue := *optionalOneOf
        optionalOneOfValue.Write(writer)
    }
    optionalStruct := example.OptionalStruct
    if optionalStruct == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        optionalStructValue := *optionalStruct
        optionalStructValue.Write(writer)
    }
    optionalEnum := example.OptionalEnum
    if optionalEnum == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        optionalEnumValue := *optionalEnum
        WriteInt32(writer, int32(optionalEnumValue))
    }
}

// Get string representation of Example
func (example Example) String() string {
    stringResult := "{ "
    stringResult += "OneOf: "
    oneOf := example.OneOf
    stringResult += oneOf.String()
    stringResult += ", "
    stringResult += "HashMap: "
    hashMap := example.HashMap
    stringResult += "map[ "
    hashMapIndex := 0
    for hashMapKey, hashMapValue := range hashMap {
        if hashMapIndex != 0 {
            stringResult += ", "
        }
        stringResult += EnumerationToString(hashMapKey)
        stringResult += ": "
        stringResult += fmt.Sprint(hashMapValue)
        hashMapIndex++
    }
    stringResult += " ]"
    stringResult += ", "
    stringResult += "OptionalInt: "
    optionalInt := example.OptionalInt
    if optionalInt == nil {
        stringResult += "nil"
    } else {
        optionalIntValue := *optionalInt
        stringResult += fmt.Sprint(optionalIntValue)
    }
    stringResult += ", "
    stringResult += "OptionalBool: "
    optionalBool := example.OptionalBool
    if optionalBool == nil {
        stringResult += "nil"
    } else {
        optionalBoolValue := *optionalBool
        stringResult += fmt.Sprint(optionalBoolValue)
    }
    stringResult += ", "
    stringResult += "OptionalOneOf: "
    optionalOneOf := example.OptionalOneOf
    if optionalOneOf == nil {
        stringResult += "nil"
    } else {
        optionalOneOfValue := *optionalOneOf
        stringResult += optionalOneOfValue.String()
    }
    stringResult += ", "
    stringResult += "OptionalStruct: "
    optionalStruct := example.OptionalStruct
    if optionalStruct == nil {
        stringResult += "nil"
    } else {
        optionalStructValue := *optionalStruct
        stringResult += optionalStructValue.String()
    }
    stringResult += ", "
    stringResult += "OptionalEnum: "
    optionalEnum := example.OptionalEnum
    if optionalEnum == nil {
        stringResult += "nil"
    } else {
        optionalEnumValue := *optionalEnum
        stringResult += EnumerationToString(optionalEnumValue)
    }
    stringResult += " }"
    return stringResult
}