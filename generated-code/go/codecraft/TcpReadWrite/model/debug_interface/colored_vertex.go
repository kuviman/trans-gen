package debug_interface

import "io"
import . "trans_gen_test/common"
import . "trans_gen_test/stream"

// Vertex for debug rendering
type ColoredVertex struct {
    // Position in world coordinates (if none, screen position (0, 0) is used)
    WorldPos *Vec2Float32
    // Additional offset in screen coordinates
    ScreenOffset Vec2Float32
    // Color to use
    Color Color
}

func NewColoredVertex(worldPos *Vec2Float32, screenOffset Vec2Float32, color Color) ColoredVertex {
    return ColoredVertex {
        WorldPos: worldPos,
        ScreenOffset: screenOffset,
        Color: color,
    }
}

// Read ColoredVertex from reader
func ReadColoredVertex(reader io.Reader) ColoredVertex {
    var worldPos *Vec2Float32
    if ReadBool(reader) {
        var worldPosValue Vec2Float32
        worldPosValue = ReadVec2Float32(reader)
        worldPos = &worldPosValue
    } else {
        worldPos = nil
    }
    var screenOffset Vec2Float32
    screenOffset = ReadVec2Float32(reader)
    var color Color
    color = ReadColor(reader)
    return ColoredVertex {
        WorldPos: worldPos,
        ScreenOffset: screenOffset,
        Color: color,
    }
}

// Write ColoredVertex to writer
func (coloredVertex ColoredVertex) Write(writer io.Writer) {
    worldPos := coloredVertex.WorldPos
    if worldPos == nil {
        WriteBool(writer, false)
    } else {
        WriteBool(writer, true)
        worldPosValue := *worldPos
        worldPosValue.Write(writer)
    }
    screenOffset := coloredVertex.ScreenOffset
    screenOffset.Write(writer)
    color := coloredVertex.Color
    color.Write(writer)
}

// Get string representation of ColoredVertex
func (coloredVertex ColoredVertex) String() string {
    stringResult := "{ "
    stringResult += "WorldPos: "
    worldPos := coloredVertex.WorldPos
    if worldPos == nil {
        stringResult += "nil"
    } else {
        worldPosValue := *worldPos
        stringResult += worldPosValue.String()
    }
    stringResult += ", "
    stringResult += "ScreenOffset: "
    screenOffset := coloredVertex.ScreenOffset
    stringResult += screenOffset.String()
    stringResult += ", "
    stringResult += "Color: "
    color := coloredVertex.Color
    stringResult += color.String()
    stringResult += " }"
    return stringResult
}