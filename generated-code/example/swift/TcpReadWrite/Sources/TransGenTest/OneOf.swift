/// Oneof example
enum OneOf {
    /// First option
    ///
    /// - vecInt: List of integers
    /// - longInt: Long integer
    case optionOne(vecInt: [Int32], longInt: Int64)

    /// Second option
    ///
    /// - value: usize
    case optionTwo(value: Int32)

    /// Read OneOf from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> OneOf {
        switch stream.readInt32() {
            case 0:
                var vecInt: [Int32]
                let vecIntSize = stream.readInt32()
                vecInt = (0..<vecIntSize).map{ _ in
                    var vecIntSize: Int32
                    vecIntSize = stream.readInt32()
                    return vecIntSize
                }
                var longInt: Int64
                longInt = stream.readInt64()
                return OneOf.optionOne(vecInt: vecInt, longInt: longInt)
            case 1:
                var value: Int32
                value = stream.readInt32()
                return OneOf.optionTwo(value: value)
            default:
                fatalError("Unexpected tag value")
        }
    }

    /// Write OneOf to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        switch self {
            case let .optionOne(vecInt, longInt):
                stream.writeInt32(0)
                stream.writeInt32(Int32(vecInt.count))
                for vecIntElement in vecInt {
                    stream.writeInt32(vecIntElement)
                }
                stream.writeInt64(longInt)
            case let .optionTwo(value):
                stream.writeInt32(1)
                stream.writeInt32(value)
        }
    }
}