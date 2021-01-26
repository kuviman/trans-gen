public struct Structure {
    let text: String
    let floatNumber: Float
    let doubleNumber: Double

    static func readFrom<S: InputStream>(_ stream: S) -> Structure {
        var text: String
        text = stream.readString()
        var floatNumber: Float
        floatNumber = stream.readFloat()
        var doubleNumber: Double
        doubleNumber = stream.readDouble()
        return Structure(text: text, floatNumber: floatNumber, doubleNumber: doubleNumber)
    }

    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeString(text)
        stream.writeFloat(floatNumber)
        stream.writeDouble(doubleNumber)
    }
}