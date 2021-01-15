package model

import "io"
import . "trans_gen_test/stream"

type Structure struct {
    OneOfOne OneOf
    OneOfTwo OneOf
    HashMap map[Enumeration]int32
    Text string
    RealNumber float64
}
func NewStructure(oneOfOne OneOf, oneOfTwo OneOf, hashMap map[Enumeration]int32, text string, realNumber float64) Structure {
    return Structure {
        OneOfOne: oneOfOne,
        OneOfTwo: oneOfTwo,
        HashMap: hashMap,
        Text: text,
        RealNumber: realNumber,
    }
}
func ReadStructure(reader io.Reader) Structure {
    result := Structure {}
    result.OneOfOne = ReadOneOf(reader)
    result.OneOfTwo = ReadOneOf(reader)
    HashMapSize := ReadInt32(reader)
    result.HashMap = make(map[Enumeration]int32)
    for i := int32(0); i < HashMapSize; i++ {
        var HashMapKey Enumeration
        HashMapKey = ReadEnumeration(reader)
        var HashMapValue int32
        HashMapValue = ReadInt32(reader)
        result.HashMap[HashMapKey] = HashMapValue
    }
    result.Text = ReadString(reader)
    result.RealNumber = ReadFloat64(reader)
    return result
}
func (value Structure) Write(writer io.Writer) {
    value.OneOfOne.Write(writer)
    value.OneOfTwo.Write(writer)
    WriteInt32(writer, int32(len(value.HashMap)))
    for HashMapKey, HashMapValue := range value.HashMap {
        WriteInt32(writer, int32(HashMapKey))
        WriteInt32(writer, HashMapValue)
    }
    WriteString(writer, value.Text)
    WriteFloat64(writer, value.RealNumber)
}
