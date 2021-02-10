import { AutoAttack } from "./auto-attack";
import { Stream } from "../stream";

/**
 * Attack action
 */
export class AttackAction {
    /**
     * If specified, target entity's ID
     */
    target: number | null
    /**
     * If specified, configures auto attacking
     */
    autoAttack: AutoAttack | null

    constructor(target: number | null, autoAttack: AutoAttack | null) {
        this.target = target;
        this.autoAttack = autoAttack;
    }

    /**
     * Read AttackAction from input stream
     */
    static async readFrom(stream: Stream): Promise<AttackAction> {
        let target;
        if (await stream.readBool()) {
            target = await stream.readInt();
        } else {
            target = null;
        }
        let autoAttack;
        if (await stream.readBool()) {
            autoAttack = await AutoAttack.readFrom(stream);
        } else {
            autoAttack = null;
        }
        return new AttackAction(target, autoAttack)
    }

    /**
     * Write AttackAction to output stream
     */
    async writeTo(stream: Stream) {
        let target = this.target;
        if (target === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeInt(target);
        }
        let autoAttack = this.autoAttack;
        if (autoAttack === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await autoAttack.writeTo(stream);
        }
    }
}