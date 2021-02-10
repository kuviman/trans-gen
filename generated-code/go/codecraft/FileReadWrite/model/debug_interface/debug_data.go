package debug_interface

import "fmt"
import "io"
import . "trans_gen_test/stream"

// Debug data can be drawn in the app
type DebugData interface {
    // Write DebugData to writer
    Write(writer io.Writer)

    // Get string representation of DebugData
    String() string
}

// Read DebugData from reader
func ReadDebugData(reader io.Reader) DebugData {
    switch ReadInt32(reader) {
    case 0:
        return ReadDebugDataLog(reader)
    case 1:
        return ReadDebugDataPrimitives(reader)
    case 2:
        return ReadDebugDataPlacedText(reader)
    }
    panic("Unexpected tag value")
}

// Log some text
type DebugDataLog struct {
    // Text to show
    Text string
}

func NewDebugDataLog(text string) DebugDataLog {
    return DebugDataLog {
        Text: text,
    }
}

// Read Log from reader
func ReadDebugDataLog(reader io.Reader) DebugDataLog {
    var text string
    text = ReadString(reader)
    return DebugDataLog {
        Text: text,
    }
}

// Write Log to writer
func (debugDataLog DebugDataLog) Write(writer io.Writer) {
    WriteInt32(writer, 0)
    text := debugDataLog.Text
    WriteString(writer, text)
}

// Get string representation of Log
func (debugDataLog DebugDataLog) String() string {
    stringResult := "{ "
    stringResult += "Text: "
    text := debugDataLog.Text
    stringResult += "\"" + text + "\""
    stringResult += " }"
    return stringResult
}

// Draw primitives
type DebugDataPrimitives struct {
    // Vertices
    Vertices []ColoredVertex
    // Primitive type
    PrimitiveType PrimitiveType
}

func NewDebugDataPrimitives(vertices []ColoredVertex, primitiveType PrimitiveType) DebugDataPrimitives {
    return DebugDataPrimitives {
        Vertices: vertices,
        PrimitiveType: primitiveType,
    }
}

// Read Primitives from reader
func ReadDebugDataPrimitives(reader io.Reader) DebugDataPrimitives {
    var vertices []ColoredVertex
    vertices = make([]ColoredVertex, ReadInt32(reader))
    for verticesIndex := range vertices {
        var verticesElement ColoredVertex
        verticesElement = ReadColoredVertex(reader)
        vertices[verticesIndex] = verticesElement
    }
    var primitiveType PrimitiveType
    primitiveType = ReadPrimitiveType(reader)
    return DebugDataPrimitives {
        Vertices: vertices,
        PrimitiveType: primitiveType,
    }
}

// Write Primitives to writer
func (debugDataPrimitives DebugDataPrimitives) Write(writer io.Writer) {
    WriteInt32(writer, 1)
    vertices := debugDataPrimitives.Vertices
    WriteInt32(writer, int32(len(vertices)))
    for _, verticesElement := range vertices {
        verticesElement.Write(writer)
    }
    primitiveType := debugDataPrimitives.PrimitiveType
    WriteInt32(writer, int32(primitiveType))
}

// Get string representation of Primitives
func (debugDataPrimitives DebugDataPrimitives) String() string {
    stringResult := "{ "
    stringResult += "Vertices: "
    vertices := debugDataPrimitives.Vertices
    stringResult += "[ "
    for verticesIndex, verticesElement := range vertices {
        if verticesIndex != 0 {
            stringResult += ", "
        }
        stringResult += verticesElement.String()
    }
    stringResult += " ]"
    stringResult += ", "
    stringResult += "PrimitiveType: "
    primitiveType := debugDataPrimitives.PrimitiveType
    stringResult += PrimitiveTypeToString(primitiveType)
    stringResult += " }"
    return stringResult
}

// Draw text
type DebugDataPlacedText struct {
    // Vertex to determine text position and color
    Vertex ColoredVertex
    // Text
    Text string
    // Text alignment (0 means left, 0.5 means center, 1 means right)
    Alignment float32
    // Font size in pixels
    Size float32
}

func NewDebugDataPlacedText(vertex ColoredVertex, text string, alignment float32, size float32) DebugDataPlacedText {
    return DebugDataPlacedText {
        Vertex: vertex,
        Text: text,
        Alignment: alignment,
        Size: size,
    }
}

// Read PlacedText from reader
func ReadDebugDataPlacedText(reader io.Reader) DebugDataPlacedText {
    var vertex ColoredVertex
    vertex = ReadColoredVertex(reader)
    var text string
    text = ReadString(reader)
    var alignment float32
    alignment = ReadFloat32(reader)
    var size float32
    size = ReadFloat32(reader)
    return DebugDataPlacedText {
        Vertex: vertex,
        Text: text,
        Alignment: alignment,
        Size: size,
    }
}

// Write PlacedText to writer
func (debugDataPlacedText DebugDataPlacedText) Write(writer io.Writer) {
    WriteInt32(writer, 2)
    vertex := debugDataPlacedText.Vertex
    vertex.Write(writer)
    text := debugDataPlacedText.Text
    WriteString(writer, text)
    alignment := debugDataPlacedText.Alignment
    WriteFloat32(writer, alignment)
    size := debugDataPlacedText.Size
    WriteFloat32(writer, size)
}

// Get string representation of PlacedText
func (debugDataPlacedText DebugDataPlacedText) String() string {
    stringResult := "{ "
    stringResult += "Vertex: "
    vertex := debugDataPlacedText.Vertex
    stringResult += vertex.String()
    stringResult += ", "
    stringResult += "Text: "
    text := debugDataPlacedText.Text
    stringResult += "\"" + text + "\""
    stringResult += ", "
    stringResult += "Alignment: "
    alignment := debugDataPlacedText.Alignment
    stringResult += fmt.Sprint(alignment)
    stringResult += ", "
    stringResult += "Size: "
    size := debugDataPlacedText.Size
    stringResult += fmt.Sprint(size)
    stringResult += " }"
    return stringResult
}