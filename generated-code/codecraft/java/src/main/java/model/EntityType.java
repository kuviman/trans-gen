package model;

import util.StreamUtil;

public enum EntityType {
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

    public int tag;

    EntityType(int tag) {
        this.tag = tag;
    }

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