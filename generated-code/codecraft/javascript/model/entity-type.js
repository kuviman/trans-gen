class EntityType {
    constructor(name, tag) {
        this.name = name;
        this.tag = tag;
    }

    static WALL = new EntityType("WALL", 0);
    static HOUSE = new EntityType("HOUSE", 1);
    static BUILDER_BASE = new EntityType("BUILDER_BASE", 2);
    static BUILDER_UNIT = new EntityType("BUILDER_UNIT", 3);
    static MELEE_BASE = new EntityType("MELEE_BASE", 4);
    static MELEE_UNIT = new EntityType("MELEE_UNIT", 5);
    static RANGED_BASE = new EntityType("RANGED_BASE", 6);
    static RANGED_UNIT = new EntityType("RANGED_UNIT", 7);
    static RESOURCE = new EntityType("RESOURCE", 8);
    static TURRET = new EntityType("TURRET", 9);

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

    async writeTo(stream) {
        await stream.writeInt(this.tag);
    }

    [Symbol.for('nodejs.util.inspect.custom')]() {
        return this.name;
    }
}

module.exports = EntityType;