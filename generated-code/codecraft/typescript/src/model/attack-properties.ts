import { StreamWrapper } from "../stream-wrapper";

export class AttackProperties {
    attackRange: number
    damage: number
    collectResource: boolean

    constructor(attackRange: number, damage: number, collectResource: boolean) {
        this.attackRange = attackRange;
        this.damage = damage;
        this.collectResource = collectResource;
    }

    static async readFrom(stream: StreamWrapper): Promise<AttackProperties> {
        let attackRange;
        attackRange = await stream.readInt();
        let damage;
        damage = await stream.readInt();
        let collectResource;
        collectResource = await stream.readBool();
        return new AttackProperties(attackRange, damage, collectResource)
    }

    async writeTo(stream: StreamWrapper) {
        let attackRange = this.attackRange;
        await stream.writeInt(attackRange);
        let damage = this.damage;
        await stream.writeInt(damage);
        let collectResource = this.collectResource;
        await stream.writeBool(collectResource);
    }
}