import { StreamWrapper } from "../stream-wrapper";

/**
 * Entity's attack properties
 */
export class AttackProperties {
    /**
     * Maximum attack range
     */
    attackRange: number
    /**
     * Damage dealt in one tick
     */
    damage: number
    /**
     * If true, dealing damage will collect resource from target
     */
    collectResource: boolean

    constructor(attackRange: number, damage: number, collectResource: boolean) {
        this.attackRange = attackRange;
        this.damage = damage;
        this.collectResource = collectResource;
    }

    /**
     * Read AttackProperties from input stream
     */
    static async readFrom(stream: StreamWrapper): Promise<AttackProperties> {
        let attackRange;
        attackRange = await stream.readInt();
        let damage;
        damage = await stream.readInt();
        let collectResource;
        collectResource = await stream.readBool();
        return new AttackProperties(attackRange, damage, collectResource)
    }

    /**
     * Write AttackProperties to output stream
     */
    async writeTo(stream: StreamWrapper) {
        let attackRange = this.attackRange;
        await stream.writeInt(attackRange);
        let damage = this.damage;
        await stream.writeInt(damage);
        let collectResource = this.collectResource;
        await stream.writeBool(collectResource);
    }
}