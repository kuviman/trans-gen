package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Entity type
 */
enum class EntityType private constructor(val tag: Int) {
    /**
     * Wall, can be used to prevent enemy from moving through
     */
    WALL(0),
    /**
     * House, used to increase population
     */
    HOUSE(1),
    /**
     * Base for recruiting new builder units
     */
    BUILDER_BASE(2),
    /**
     * Builder unit can build buildings
     */
    BUILDER_UNIT(3),
    /**
     * Base for recruiting new melee units
     */
    MELEE_BASE(4),
    /**
     * Melee unit
     */
    MELEE_UNIT(5),
    /**
     * Base for recruiting new ranged units
     */
    RANGED_BASE(6),
    /**
     * Ranged unit
     */
    RANGED_UNIT(7),
    /**
     * Resource can be harvested
     */
    RESOURCE(8),
    /**
     * Ranged attacking building
     */
    TURRET(9);

    companion object {
        /**
         * Read EntityType from input stream
         */
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