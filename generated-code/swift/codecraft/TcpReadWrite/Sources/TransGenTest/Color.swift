/// RGBA Color
public struct Color {
    /// Red component
    let r: Float

    /// Green component
    let g: Float

    /// Blue component
    let b: Float

    /// Alpha (opacity) component
    let a: Float

    /// Read Color from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> Color {
        var r: Float
        r = stream.readFloat()
        var g: Float
        g = stream.readFloat()
        var b: Float
        b = stream.readFloat()
        var a: Float
        a = stream.readFloat()
        return Color(r: r, g: g, b: b, a: a)
    }

    /// Write Color to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeFloat(r)
        stream.writeFloat(g)
        stream.writeFloat(b)
        stream.writeFloat(a)
    }
}