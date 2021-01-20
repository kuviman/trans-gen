package model

import "io"
import . "trans_gen_test/stream"

type Structure struct {
    OneOfOne OneOf
    OneOfTwo OneOf
    HashMap map[Enumeration]int32
    Text string
    FloatNumber float32
    DoubleNumber float64
}

func NewStructure(oneOfOne OneOf, oneOfTwo OneOf, hashMap map[Enumeration]int32, text string, floatNumber float32, doubleNumber float64) Structure {
    return Structure {
        OneOfOne: oneOfOne,
        OneOfTwo: oneOfTwo,
        HashMap: hashMap,
        Text: text,
        FloatNumber: floatNumber,
        DoubleNumber: doubleNumber,
    }
}

func ReadStructure(reader io.Reader) Structure {
    var oneOfOne OneOf
    oneOfOne = ReadOneOf(reader)
    var oneOfTwo OneOf
    oneOfTwo = ReadOneOf(reader)
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
    var text string
    text = ReadString(reader)
    var floatNumber float32
    floatNumber = ReadFloat32(reader)
    var doubleNumber float64
    doubleNumber = ReadFloat64(reader)
    return Structure {
        OneOfOne: oneOfOne,
        OneOfTwo: oneOfTwo,
        HashMap: hashMap,
        Text: text,
        FloatNumber: floatNumber,
        DoubleNumber: doubleNumber,
    }
}

func (structure Structure) Write(writer io.Writer) {
    oneOfOne := structure.OneOfOne
    oneOfOne.Write(writer)
    oneOfTwo := structure.OneOfTwo
    oneOfTwo.Write(writer)
    hashMap := structure.HashMap
    WriteInt32(writer, int32(len(hashMap)))
    for hashMapKey, hashMapValue := range hashMap {
        WriteInt32(writer, int32(hashMapKey))
        WriteInt32(writer, hashMapValue)
    }
    text := structure.Text
    WriteString(writer, text)
    floatNumber := structure.FloatNumber
    WriteFloat32(writer, floatNumber)
    doubleNumber := structure.DoubleNumber
    WriteFloat64(writer, doubleNumber)
}