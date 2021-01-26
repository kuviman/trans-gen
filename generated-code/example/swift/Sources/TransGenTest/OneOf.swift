enum OneOf {
    case optionOne(vecInt: [Int32], longInt: Int64)
    case optionTwo(value: Int32)

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