const AutoAttack = require.main.require('./model/auto-attack');
/**
 * Attack action
 */
class AttackAction {
    /**
     * If specified, target entity's ID
     */
    target;
    /**
     * If specified, configures auto attacking
     */
    autoAttack;

    constructor(target, autoAttack) {
        this.target = target;
        this.autoAttack = autoAttack;
    }

    /**
     * Read AttackAction from input stream
     */
    static async readFrom(stream) {
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
        return new AttackAction(target, autoAttack);
    }

    /**
     * Write AttackAction to output stream
     */
    async writeTo(stream) {
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
module.exports = AttackAction