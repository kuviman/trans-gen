/// Example enumeration
public enum Enumeration: Int32 {
    /// First option
    case valueOne = 0

    /// Second option
    case valueTwo = 1

    /// Read Enumeration from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> Enumeration {
        return Enumeration(rawValue: stream.readInt32())!
    }

    /// Write Enumeration to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(rawValue)
    }
}