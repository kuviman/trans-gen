import { Vec2Int } from "../vec2-int";
import { Stream } from "../stream";

/**
 * Move action
 */
export class MoveAction {
    /**
     * Target position
     */
    target: Vec2Int
    /**
     * Whether to try find closest position, if path to target is not found
     */
    findClosestPosition: boolean
    /**
     * Whether to destroy other entities on the way
     */
    breakThrough: boolean

    constructor(target: Vec2Int, findClosestPosition: boolean, breakThrough: boolean) {
        this.target = target;
        this.findClosestPosition = findClosestPosition;
        this.breakThrough = breakThrough;
    }

    /**
     * Read MoveAction from input stream
     */
    static async readFrom(stream: Stream): Promise<MoveAction> {
        let target;
        target = await Vec2Int.readFrom(stream);
        let findClosestPosition;
        findClosestPosition = await stream.readBool();
        let breakThrough;
        breakThrough = await stream.readBool();
        return new MoveAction(target, findClosestPosition, breakThrough)
    }

    /**
     * Write MoveAction to output stream
     */
    async writeTo(stream: Stream) {
        let target = this.target;
        await target.writeTo(stream);
        let findClosestPosition = this.findClosestPosition;
        await stream.writeBool(findClosestPosition);
        let breakThrough = this.breakThrough;
        await stream.writeBool(breakThrough);
    }
}