public enum EntityType: Int32 {
    case wall = 0
    case house = 1
    case builderBase = 2
    case builderUnit = 3
    case meleeBase = 4
    case meleeUnit = 5
    case rangedBase = 6
    case rangedUnit = 7
    case resource = 8
    case turret = 9

    static func readFrom<S: InputStream>(_ stream: S) -> EntityType {
        return EntityType(rawValue: stream.readInt32())!
    }

    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(rawValue)
    }
}