import { EntityType } from "./entity-type";
import { StreamWrapper } from "../stream-wrapper";

export class BuildProperties {
    options: Array<EntityType>
    initHealth: number | null

    constructor(options: Array<EntityType>, initHealth: number | null) {
        this.options = options;
        this.initHealth = initHealth;
    }

    static async readFrom(stream: StreamWrapper): Promise<BuildProperties> {
        let options;
        options = [];
        for (let optionsCount = await stream.readInt(); optionsCount > 0; optionsCount--) {
            let optionsElement;
            optionsElement = await EntityType.readFrom(stream);
            options.push(optionsElement);
        }
        let initHealth;
        if (await stream.readBool()) {
            initHealth = await stream.readInt();
        } else {
            initHealth = null;
        }
        return new BuildProperties(options, initHealth)
    }

    async writeTo(stream: StreamWrapper) {
        let options = this.options;
        await stream.writeInt(options.length);
        for (let optionsElement of options) {
            await optionsElement.writeTo(stream);
        }
        let initHealth = this.initHealth;
        if (initHealth === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeInt(initHealth);
        }
    }
}