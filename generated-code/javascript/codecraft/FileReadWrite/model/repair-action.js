/**
 * Repair action
 */
class RepairAction {
    /**
     * Target entity's ID
     */
    target;

    constructor(target) {
        this.target = target;
    }

    /**
     * Read RepairAction from input stream
     */
    static async readFrom(stream) {
        let target;
        target = await stream.readInt();
        return new RepairAction(target);
    }

    /**
     * Write RepairAction to output stream
     */
    async writeTo(stream) {
        let target = this.target;
        await stream.writeInt(target);
    }
}
module.exports = RepairAction