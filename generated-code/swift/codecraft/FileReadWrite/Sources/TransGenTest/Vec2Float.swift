/// 2 dimensional vector.
public struct Vec2Float {
    /// `x` coordinate of the vector
    let x: Float

    /// `y` coordinate of the vector
    let y: Float

    /// Read Vec2Float from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> Vec2Float {
        var x: Float
        x = stream.readFloat()
        var y: Float
        y = stream.readFloat()
        return Vec2Float(x: x, y: y)
    }

    /// Write Vec2Float to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeFloat(x)
        stream.writeFloat(y)
    }
}