/// Example structure
public struct Structure {
    /// Text
    let text: String

    /// 32-bit float
    let floatNumber: Float

    /// 64-bit float
    let doubleNumber: Double

    /// Read Structure from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> Structure {
        var text: String
        text = stream.readString()
        var floatNumber: Float
        floatNumber = stream.readFloat()
        var doubleNumber: Double
        doubleNumber = stream.readDouble()
        return Structure(text: text, floatNumber: floatNumber, doubleNumber: doubleNumber)
    }

    /// Write Structure to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeString(text)
        stream.writeFloat(floatNumber)
        stream.writeDouble(doubleNumber)
    }
}