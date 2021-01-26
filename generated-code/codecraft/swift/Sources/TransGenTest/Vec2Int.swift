public struct Vec2Int {
    let x: Int32
    let y: Int32

    static func readFrom<S: InputStream>(_ stream: S) -> Vec2Int {
        var x: Int32
        x = stream.readInt32()
        var y: Int32
        y = stream.readInt32()
        return Vec2Int(x: x, y: y)
    }

    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(x)
        stream.writeInt32(y)
    }
}