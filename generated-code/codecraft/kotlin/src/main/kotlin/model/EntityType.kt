package model

import util.StreamUtil

enum class EntityType private constructor(val tag: Int) {
    WALL(0),
    HOUSE(1),
    BUILDER_BASE(2),
    BUILDER_UNIT(3),
    MELEE_BASE(4),
    MELEE_UNIT(5),
    RANGED_BASE(6),
    RANGED_UNIT(7),
    RESOURCE(8),
    TURRET(9);

    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): EntityType {
            return when (StreamUtil.readInt(stream)) {
            WALL.tag -> WALL
            HOUSE.tag -> HOUSE
            BUILDER_BASE.tag -> BUILDER_BASE
            BUILDER_UNIT.tag -> BUILDER_UNIT
            MELEE_BASE.tag -> MELEE_BASE
            MELEE_UNIT.tag -> MELEE_UNIT
            RANGED_BASE.tag -> RANGED_BASE
            RANGED_UNIT.tag -> RANGED_UNIT
            RESOURCE.tag -> RESOURCE
            TURRET.tag -> TURRET
            else -> throw java.io.IOException("Unexpected tag value")
            }
        }
    }
}