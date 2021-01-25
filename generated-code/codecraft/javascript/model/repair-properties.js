const EntityType = require('./entity-type');
class RepairProperties {
    constructor(validTargets, power) {
        this.validTargets = validTargets;
        this.power = power;
    }

    static async readFrom(stream) {
        let validTargets;
        validTargets = [];
        for (let validTargetsCount = await stream.readInt(); validTargetsCount > 0; validTargetsCount--) {
            let validTargetsElement;
            validTargetsElement = await EntityType.readFrom(stream);
            validTargets.push(validTargetsElement);
        }
        let power;
        power = await stream.readInt();
        return new RepairProperties(validTargets, power);
    }

    async writeTo(stream) {
        let validTargets = this.validTargets;
        await stream.writeInt(validTargets.length);
        for (let validTargetsElement of validTargets) {
            await validTargetsElement.writeTo(stream);
        }
        let power = this.power;
        await stream.writeInt(power);
    }
}
module.exports = RepairProperties