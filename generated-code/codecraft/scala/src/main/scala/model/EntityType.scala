package model

import util.StreamUtil

sealed abstract class EntityType (val tag: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, tag)
    }
}

object EntityType {
    case object WALL extends EntityType(0)
    case object HOUSE extends EntityType(1)
    case object BUILDER_BASE extends EntityType(2)
    case object BUILDER_UNIT extends EntityType(3)
    case object MELEE_BASE extends EntityType(4)
    case object MELEE_UNIT extends EntityType(5)
    case object RANGED_BASE extends EntityType(6)
    case object RANGED_UNIT extends EntityType(7)
    case object RESOURCE extends EntityType(8)
    case object TURRET extends EntityType(9)

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