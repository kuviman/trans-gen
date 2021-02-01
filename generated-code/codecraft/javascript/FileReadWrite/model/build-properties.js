const EntityType = require('./entity-type');
/**
 * Entity's build properties
 */
class BuildProperties {
    /**
     * Valid new entity types
     */
    options;
    /**
     * Initial health of new entity. If absent, it will have full health
     */
    initHealth;

    constructor(options, initHealth) {
        this.options = options;
        this.initHealth = initHealth;
    }

    /**
     * Read BuildProperties from input stream
     */
    static async readFrom(stream) {
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
        return new BuildProperties(options, initHealth);
    }

    /**
     * Write BuildProperties to output stream
     */
    async writeTo(stream) {
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
module.exports = BuildProperties