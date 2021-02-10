/// Vertex for debug rendering
public struct ColoredVertex {
    /// Position in world coordinates (if none, screen position (0, 0) is used)
    let worldPos: Vec2Float?

    /// Additional offset in screen coordinates
    let screenOffset: Vec2Float

    /// Color to use
    let color: Color

    /// Read ColoredVertex from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> ColoredVertex {
        var worldPos: Vec2Float?
        if stream.readBool() {
            worldPos = Vec2Float.readFrom(stream)
        } else {
            worldPos = nil
        }
        var screenOffset: Vec2Float
        screenOffset = Vec2Float.readFrom(stream)
        var color: Color
        color = Color.readFrom(stream)
        return ColoredVertex(worldPos: worldPos, screenOffset: screenOffset, color: color)
    }

    /// Write ColoredVertex to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        if worldPos == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let worldPosValue = worldPos!
            worldPosValue.writeTo(stream)
        }
        screenOffset.writeTo(stream)
        color.writeTo(stream)
    }
}