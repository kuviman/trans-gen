module model.debug_interface.primitive_type;

import stream;

/// Primitive type for debug rendering
enum PrimitiveType : int {
    /// Lines, number of vertices should be divisible by 2
    Lines = 0,
    /// Triangles, number of vertices should be divisible by 3
    Triangles = 1,
}

/// Read PrimitiveType from reader
PrimitiveType readPrimitiveType(Stream reader) {
    switch (reader.readInt()) {
        case PrimitiveType.Lines:
            return PrimitiveType.Lines;
        case PrimitiveType.Triangles:
            return PrimitiveType.Triangles;
        default:
            throw new Exception("Unexpected tag value");
    }
}