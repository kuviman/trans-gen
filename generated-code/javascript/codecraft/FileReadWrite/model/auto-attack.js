const EntityType = require.main.require('./model/entity-type');
/**
 * Auto attack options
 */
class AutoAttack {
    /**
     * Maximum distance to pathfind
     */
    pathfindRange;
    /**
     * List of target entity types to try to attack. If empty, all types but resource are considered
     */
    validTargets;

    constructor(pathfindRange, validTargets) {
        this.pathfindRange = pathfindRange;
        this.validTargets = validTargets;
    }

    /**
     * Read AutoAttack from input stream
     */
    static async readFrom(stream) {
        let pathfindRange;
        pathfindRange = await stream.readInt();
        let validTargets;
        validTargets = [];
        for (let validTargetsCount = await stream.readInt(); validTargetsCount > 0; validTargetsCount--) {
            let validTargetsElement;
            validTargetsElement = await EntityType.readFrom(stream);
            validTargets.push(validTargetsElement);
        }
        return new AutoAttack(pathfindRange, validTargets);
    }

    /**
     * Write AutoAttack to output stream
     */
    async writeTo(stream) {
        let pathfindRange = this.pathfindRange;
        await stream.writeInt(pathfindRange);
        let validTargets = this.validTargets;
        await stream.writeInt(validTargets.length);
        for (let validTargetsElement of validTargets) {
            await validTargetsElement.writeTo(stream);
        }
    }
}
module.exports = AutoAttack