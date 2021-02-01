import { EntityType } from "./entity-type";
import { Stream } from "../stream";

/**
 * Entity's build properties
 */
export class BuildProperties {
    /**
     * Valid new entity types
     */
    options: Array<EntityType>
    /**
     * Initial health of new entity. If absent, it will have full health
     */
    initHealth: number | null

    constructor(options: Array<EntityType>, initHealth: number | null) {
        this.options = options;
        this.initHealth = initHealth;
    }

    /**
     * Read BuildProperties from input stream
     */
    static async readFrom(stream: Stream): Promise<BuildProperties> {
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

    /**
     * Write BuildProperties to output stream
     */
    async writeTo(stream: Stream) {
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