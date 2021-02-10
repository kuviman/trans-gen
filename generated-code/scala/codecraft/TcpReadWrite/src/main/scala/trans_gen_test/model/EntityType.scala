package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Entity type
 */
sealed abstract class EntityType (val tag: Int) {
    /**
     * Write EntityType to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, tag)
    }
}

object EntityType {
    /**
     * Wall, can be used to prevent enemy from moving through
     */
    case object WALL extends EntityType(0)
    /**
     * House, used to increase population
     */
    case object HOUSE extends EntityType(1)
    /**
     * Base for recruiting new builder units
     */
    case object BUILDER_BASE extends EntityType(2)
    /**
     * Builder unit can build buildings
     */
    case object BUILDER_UNIT extends EntityType(3)
    /**
     * Base for recruiting new melee units
     */
    case object MELEE_BASE extends EntityType(4)
    /**
     * Melee unit
     */
    case object MELEE_UNIT extends EntityType(5)
    /**
     * Base for recruiting new ranged units
     */
    case object RANGED_BASE extends EntityType(6)
    /**
     * Ranged unit
     */
    case object RANGED_UNIT extends EntityType(7)
    /**
     * Resource can be harvested
     */
    case object RESOURCE extends EntityType(8)
    /**
     * Ranged attacking building
     */
    case object TURRET extends EntityType(9)

    /**
     * Read EntityType from input stream
     */
    def readFrom(stream: java.io.InputStream): EntityType =
        StreamUtil.readInt(stream) match {
            case WALL.tag => WALL
            case HOUSE.tag => HOUSE
            case BUILDER_BASE.tag => BUILDER_BASE
            case BUILDER_UNIT.tag => BUILDER_UNIT
            case MELEE_BASE.tag => MELEE_BASE
            case MELEE_UNIT.tag => MELEE_UNIT
            case RANGED_BASE.tag => RANGED_BASE
            case RANGED_UNIT.tag => RANGED_UNIT
            case RESOURCE.tag => RESOURCE
            case TURRET.tag => TURRET
            case _ => throw new java.io.IOException("Unexpected tag value")
        }
}