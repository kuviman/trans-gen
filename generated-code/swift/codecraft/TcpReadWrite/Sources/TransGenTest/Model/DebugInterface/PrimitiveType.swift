/// Primitive type for debug rendering
public enum PrimitiveType: Int32 {
    /// Lines, number of vertices should be divisible by 2
    case lines = 0

    /// Triangles, number of vertices should be divisible by 3
    case triangles = 1

    /// Read PrimitiveType from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> PrimitiveType {
        return PrimitiveType(rawValue: stream.readInt32())!
    }

    /// Write PrimitiveType to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(rawValue)
    }
}