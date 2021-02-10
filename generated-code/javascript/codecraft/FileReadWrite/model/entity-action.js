const AttackAction = require.main.require('./model/attack-action');
const BuildAction = require.main.require('./model/build-action');
const MoveAction = require.main.require('./model/move-action');
const RepairAction = require.main.require('./model/repair-action');
/**
 * Entity's action
 */
class EntityAction {
    /**
     * Move action
     */
    moveAction;
    /**
     * Build action
     */
    buildAction;
    /**
     * Attack action
     */
    attackAction;
    /**
     * Repair action
     */
    repairAction;

    constructor(moveAction, buildAction, attackAction, repairAction) {
        this.moveAction = moveAction;
        this.buildAction = buildAction;
        this.attackAction = attackAction;
        this.repairAction = repairAction;
    }

    /**
     * Read EntityAction from input stream
     */
    static async readFrom(stream) {
        let moveAction;
        if (await stream.readBool()) {
            moveAction = await MoveAction.readFrom(stream);
        } else {
            moveAction = null;
        }
        let buildAction;
        if (await stream.readBool()) {
            buildAction = await BuildAction.readFrom(stream);
        } else {
            buildAction = null;
        }
        let attackAction;
        if (await stream.readBool()) {
            attackAction = await AttackAction.readFrom(stream);
        } else {
            attackAction = null;
        }
        let repairAction;
        if (await stream.readBool()) {
            repairAction = await RepairAction.readFrom(stream);
        } else {
            repairAction = null;
        }
        return new EntityAction(moveAction, buildAction, attackAction, repairAction);
    }

    /**
     * Write EntityAction to output stream
     */
    async writeTo(stream) {
        let moveAction = this.moveAction;
        if (moveAction === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await moveAction.writeTo(stream);
        }
        let buildAction = this.buildAction;
        if (buildAction === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await buildAction.writeTo(stream);
        }
        let attackAction = this.attackAction;
        if (attackAction === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await attackAction.writeTo(stream);
        }
        let repairAction = this.repairAction;
        if (repairAction === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await repairAction.writeTo(stream);
        }
    }
}
module.exports = EntityAction