/// Debug data can be drawn in the app
enum DebugData {
    /// Log some text
    ///
    /// - text: Text to show
    case log(text: String)

    /// Draw primitives
    ///
    /// - vertices: Vertices
    /// - primitiveType: Primitive type
    case primitives(vertices: [ColoredVertex], primitiveType: PrimitiveType)

    /// Draw text
    ///
    /// - vertex: Vertex to determine text position and color
    /// - text: Text
    /// - alignment: Text alignment (0 means left, 0.5 means center, 1 means right)
    /// - size: Font size in pixels
    case placedText(vertex: ColoredVertex, text: String, alignment: Float, size: Float)

    /// Read DebugData from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> DebugData {
        switch stream.readInt32() {
            case 0:
                var text: String
                text = stream.readString()
                return DebugData.log(text: text)
            case 1:
                var vertices: [ColoredVertex]
                let verticesSize = stream.readInt32()
                vertices = (0..<verticesSize).map{ _ in
                    var verticesSize: ColoredVertex
                    verticesSize = ColoredVertex.readFrom(stream)
                    return verticesSize
                }
                var primitiveType: PrimitiveType
                primitiveType = PrimitiveType.readFrom(stream)
                return DebugData.primitives(vertices: vertices, primitiveType: primitiveType)
            case 2:
                var vertex: ColoredVertex
                vertex = ColoredVertex.readFrom(stream)
                var text: String
                text = stream.readString()
                var alignment: Float
                alignment = stream.readFloat()
                var size: Float
                size = stream.readFloat()
                return DebugData.placedText(vertex: vertex, text: text, alignment: alignment, size: size)
            default:
                fatalError("Unexpected tag value")
        }
    }

    /// Write DebugData to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        switch self {
            case let .log(text):
                stream.writeInt32(0)
                stream.writeString(text)
            case let .primitives(vertices, primitiveType):
                stream.writeInt32(1)
                stream.writeInt32(Int32(vertices.count))
                for verticesElement in vertices {
                    verticesElement.writeTo(stream)
                }
                primitiveType.writeTo(stream)
            case let .placedText(vertex, text, alignment, size):
                stream.writeInt32(2)
                vertex.writeTo(stream)
                stream.writeString(text)
                stream.writeFloat(alignment)
                stream.writeFloat(size)
        }
    }
}