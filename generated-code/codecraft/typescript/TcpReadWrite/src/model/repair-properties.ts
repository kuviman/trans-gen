import { EntityType } from "./entity-type";
import { Stream } from "../stream";

/**
 * Entity's repair properties
 */
export class RepairProperties {
    /**
     * Valid target entity types
     */
    validTargets: Array<EntityType>
    /**
     * Health restored in one tick
     */
    power: number

    constructor(validTargets: Array<EntityType>, power: number) {
        this.validTargets = validTargets;
        this.power = power;
    }

    /**
     * Read RepairProperties from input stream
     */
    static async readFrom(stream: Stream): Promise<RepairProperties> {
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

    /**
     * Write RepairProperties to output stream
     */
    async writeTo(stream: Stream) {
        let validTargets = this.validTargets;
        await stream.writeInt(validTargets.length);
        for (let validTargetsElement of validTargets) {
            await validTargetsElement.writeTo(stream);
        }
        let power = this.power;
        await stream.writeInt(power);
    }
}