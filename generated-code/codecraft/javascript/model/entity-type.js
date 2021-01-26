/**
 * Entity type
 */
class EntityType {
    constructor(name, tag) {
        this.name = name;
        this.tag = tag;
    }

    /**
     * Wall, can be used to prevent enemy from moving through
     */
    static WALL = new EntityType("WALL", 0);
    /**
     * House, used to increase population
     */
    static HOUSE = new EntityType("HOUSE", 1);
    /**
     * Base for recruiting new builder units
     */
    static BUILDER_BASE = new EntityType("BUILDER_BASE", 2);
    /**
     * Builder unit can build buildings
     */
    static BUILDER_UNIT = new EntityType("BUILDER_UNIT", 3);
    /**
     * Base for recruiting new melee units
     */
    static MELEE_BASE = new EntityType("MELEE_BASE", 4);
    /**
     * Melee unit
     */
    static MELEE_UNIT = new EntityType("MELEE_UNIT", 5);
    /**
     * Base for recruiting new ranged units
     */
    static RANGED_BASE = new EntityType("RANGED_BASE", 6);
    /**
     * Ranged unit
     */
    static RANGED_UNIT = new EntityType("RANGED_UNIT", 7);
    /**
     * Resource can be harvested
     */
    static RESOURCE = new EntityType("RESOURCE", 8);
    /**
     * Ranged attacking building
     */
    static TURRET = new EntityType("TURRET", 9);

    /**
     * Read EntityType from input stream
     */
    static async readFrom(stream) {
        const tag = await stream.readInt();
        if (tag == EntityType.WALL.tag) {
            return EntityType.WALL;
        }
        if (tag == EntityType.HOUSE.tag) {
            return EntityType.HOUSE;
        }
        if (tag == EntityType.BUILDER_BASE.tag) {
            return EntityType.BUILDER_BASE;
        }
        if (tag == EntityType.BUILDER_UNIT.tag) {
            return EntityType.BUILDER_UNIT;
        }
        if (tag == EntityType.MELEE_BASE.tag) {
            return EntityType.MELEE_BASE;
        }
        if (tag == EntityType.MELEE_UNIT.tag) {
            return EntityType.MELEE_UNIT;
        }
        if (tag == EntityType.RANGED_BASE.tag) {
            return EntityType.RANGED_BASE;
        }
        if (tag == EntityType.RANGED_UNIT.tag) {
            return EntityType.RANGED_UNIT;
        }
        if (tag == EntityType.RESOURCE.tag) {
            return EntityType.RESOURCE;
        }
        if (tag == EntityType.TURRET.tag) {
            return EntityType.TURRET;
        }
        throw new Error("Unexpected tag value");
    }

    /**
     * Write EntityType to output stream
     */
    async writeTo(stream) {
        await stream.writeInt(this.tag);
    }

    [Symbol.for('nodejs.util.inspect.custom')]() {
        return this.name;
    }
}

module.exports = EntityType;