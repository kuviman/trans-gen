package debug_interface

import "io"
import . "trans_gen_test/stream"

// Primitive type for debug rendering
type PrimitiveType int32

const (
    // Lines, number of vertices should be divisible by 2
    PrimitiveTypeLines PrimitiveType = 0
    // Triangles, number of vertices should be divisible by 3
    PrimitiveTypeTriangles PrimitiveType = 1
)

// Read PrimitiveType from reader
func ReadPrimitiveType(reader io.Reader) PrimitiveType {
    switch ReadInt32(reader) {
    case 0:
        return PrimitiveTypeLines
    case 1:
        return PrimitiveTypeTriangles
    }
    panic("Unexpected tag value")
}

// Get string representation of PrimitiveType
func PrimitiveTypeToString(primitiveType PrimitiveType) string {
    switch primitiveType {
    case PrimitiveTypeLines:
        return "Lines"
    case PrimitiveTypeTriangles:
        return "Triangles"
    }
    panic("Impossible happened")
}