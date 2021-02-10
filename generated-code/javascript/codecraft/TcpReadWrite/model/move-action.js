const Vec2Int = require.main.require('./vec2-int');
/**
 * Move action
 */
class MoveAction {
    /**
     * Target position
     */
    target;
    /**
     * Whether to try find closest position, if path to target is not found
     */
    findClosestPosition;
    /**
     * Whether to destroy other entities on the way
     */
    breakThrough;

    constructor(target, findClosestPosition, breakThrough) {
        this.target = target;
        this.findClosestPosition = findClosestPosition;
        this.breakThrough = breakThrough;
    }

    /**
     * Read MoveAction from input stream
     */
    static async readFrom(stream) {
        let target;
        target = await Vec2Int.readFrom(stream);
        let findClosestPosition;
        findClosestPosition = await stream.readBool();
        let breakThrough;
        breakThrough = await stream.readBool();
        return new MoveAction(target, findClosestPosition, breakThrough);
    }

    /**
     * Write MoveAction to output stream
     */
    async writeTo(stream) {
        let target = this.target;
        await target.writeTo(stream);
        let findClosestPosition = this.findClosestPosition;
        await stream.writeBool(findClosestPosition);
        let breakThrough = this.breakThrough;
        await stream.writeBool(breakThrough);
    }
}
module.exports = MoveAction