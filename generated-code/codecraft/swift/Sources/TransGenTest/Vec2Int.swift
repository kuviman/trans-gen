/// 2 dimensional vector.
public struct Vec2Int {
    /// `x` coordinate of the vector
    let x: Int32

    /// `y` coordinate of the vector
    let y: Int32

    /// Read Vec2Int from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> Vec2Int {
        var x: Int32
        x = stream.readInt32()
        var y: Int32
        y = stream.readInt32()
        return Vec2Int(x: x, y: y)
    }

    /// Write Vec2Int to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(x)
        stream.writeInt32(y)
    }
}