package model

import "io"
import . "trans_gen_test/stream"
import "fmt"

type Structure struct {
    Text string
    FloatNumber float32
    DoubleNumber float64
}

func NewStructure(text string, floatNumber float32, doubleNumber float64) Structure {
    return Structure {
        Text: text,
        FloatNumber: floatNumber,
        DoubleNumber: doubleNumber,
    }
}

func ReadStructure(reader io.Reader) Structure {
    var text string
    text = ReadString(reader)
    var floatNumber float32
    floatNumber = ReadFloat32(reader)
    var doubleNumber float64
    doubleNumber = ReadFloat64(reader)
    return Structure {
        Text: text,
        FloatNumber: floatNumber,
        DoubleNumber: doubleNumber,
    }
}

func (structure Structure) Write(writer io.Writer) {
    text := structure.Text
    WriteString(writer, text)
    floatNumber := structure.FloatNumber
    WriteFloat32(writer, floatNumber)
    doubleNumber := structure.DoubleNumber
    WriteFloat64(writer, doubleNumber)
}

func (structure Structure) String() string {
    stringResult := "{ "
    stringResult += "Text: "
    text := structure.Text
    stringResult += "\"" + text + "\""
    stringResult += ", "
    stringResult += "FloatNumber: "
    floatNumber := structure.FloatNumber
    stringResult += fmt.Sprint(floatNumber)
    stringResult += ", "
    stringResult += "DoubleNumber: "
    doubleNumber := structure.DoubleNumber
    stringResult += fmt.Sprint(doubleNumber)
    stringResult += " }"
    return stringResult
}