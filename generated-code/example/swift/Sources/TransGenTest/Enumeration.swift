public enum Enumeration: Int32 {
    case valueOne = 0
    case valueTwo = 1

    static func readFrom<S: InputStream>(_ stream: S) -> Enumeration {
        return Enumeration(rawValue: stream.readInt32())!
    }

    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(rawValue)
    }
}