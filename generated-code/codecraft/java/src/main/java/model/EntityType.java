package model;

import util.StreamUtil;

/**
 * Entity type
 */
public enum EntityType {
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

    public int tag;

    EntityType(int tag) {
        this.tag = tag;
    }

    /**
     * Read EntityType from input stream
     */
    public static EntityType readFrom(java.io.InputStream stream) throws java.io.IOException {
        switch (StreamUtil.readInt(stream)) {
        case 0:
            return WALL;
        case 1:
            return HOUSE;
        case 2:
            return BUILDER_BASE;
        case 3:
            return BUILDER_UNIT;
        case 4:
            return MELEE_BASE;
        case 5:
            return MELEE_UNIT;
        case 6:
            return RANGED_BASE;
        case 7:
            return RANGED_UNIT;
        case 8:
            return RESOURCE;
        case 9:
            return TURRET;
        default:
            throw new java.io.IOException("Unexpected tag value");
        }
    }
}