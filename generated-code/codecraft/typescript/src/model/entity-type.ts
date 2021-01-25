import { StreamWrapper } from "../stream-wrapper";

export class EntityType {
    readonly name: string;
    readonly tag: number;

    constructor(name: string, tag: number) {
        this.name = name;
        this.tag = tag;
    }

    static readonly WALL = new EntityType("WALL", 0);
    static readonly HOUSE = new EntityType("HOUSE", 1);
    static readonly BUILDER_BASE = new EntityType("BUILDER_BASE", 2);
    static readonly BUILDER_UNIT = new EntityType("BUILDER_UNIT", 3);
    static readonly MELEE_BASE = new EntityType("MELEE_BASE", 4);
    static readonly MELEE_UNIT = new EntityType("MELEE_UNIT", 5);
    static readonly RANGED_BASE = new EntityType("RANGED_BASE", 6);
    static readonly RANGED_UNIT = new EntityType("RANGED_UNIT", 7);
    static readonly RESOURCE = new EntityType("RESOURCE", 8);
    static readonly TURRET = new EntityType("TURRET", 9);

    static async readFrom(stream: StreamWrapper): Promise<EntityType> {
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

    async writeTo(stream: StreamWrapper) {
        await stream.writeInt(this.tag);
    }

    [Symbol.for('nodejs.util.inspect.custom')]() {
        return this.name;
    }
}