import { EntityType } from "./entity-type";
import { StreamWrapper } from "../stream-wrapper";

export class RepairProperties {
    validTargets: Array<EntityType>
    power: number

    constructor(validTargets: Array<EntityType>, power: number) {
        this.validTargets = validTargets;
        this.power = power;
    }

    static async readFrom(stream: StreamWrapper): Promise<RepairProperties> {
        let validTargets;
        validTargets = [];
        for (let validTargetsCount = await stream.readInt(); validTargetsCount > 0; validTargetsCount--) {
            let validTargetsElement;
            validTargetsElement = await EntityType.readFrom(stream);
            validTargets.push(validTargetsElement);
        }
        let power;
        power = await stream.readInt();
        return new RepairProperties(validTargets, power)
    }

    async writeTo(stream: StreamWrapper) {
        let validTargets = this.validTargets;
        await stream.writeInt(validTargets.length);
        for (let validTargetsElement of validTargets) {
            await validTargetsElement.writeTo(stream);
        }
        let power = this.power;
        await stream.writeInt(power);
    }
}