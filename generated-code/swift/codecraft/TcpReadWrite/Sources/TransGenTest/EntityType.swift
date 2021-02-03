/// Entity type
public enum EntityType: Int32 {
    /// Wall, can be used to prevent enemy from moving through
    case wall = 0

    /// House, used to increase population
    case house = 1

    /// Base for recruiting new builder units
    case builderBase = 2

    /// Builder unit can build buildings
    case builderUnit = 3

    /// Base for recruiting new melee units
    case meleeBase = 4

    /// Melee unit
    case meleeUnit = 5

    /// Base for recruiting new ranged units
    case rangedBase = 6

    /// Ranged unit
    case rangedUnit = 7

    /// Resource can be harvested
    case resource = 8

    /// Ranged attacking building
    case turret = 9

    /// Read EntityType from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> EntityType {
        return EntityType(rawValue: stream.readInt32())!
    }

    /// Write EntityType to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(rawValue)
    }
}