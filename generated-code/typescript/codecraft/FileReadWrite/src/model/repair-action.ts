import { Stream } from "../stream";

/**
 * Repair action
 */
export class RepairAction {
    /**
     * Target entity's ID
     */
    target: number

    constructor(target: number) {
        this.target = target;
    }

    /**
     * Read RepairAction from input stream
     */
    static async readFrom(stream: Stream): Promise<RepairAction> {
        let target;
        target = await stream.readInt();
        return new RepairAction(target)
    }

    /**
     * Write RepairAction to output stream
     */
    async writeTo(stream: Stream) {
        let target = this.target;
        await stream.writeInt(target);
    }
}