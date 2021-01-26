/**
 * Example enumeration
 */
class Enumeration {
    constructor(name, tag) {
        this.name = name;
        this.tag = tag;
    }

    /**
     * First option
     */
    static VALUE_ONE = new Enumeration("VALUE_ONE", 0);
    /**
     * Second option
     */
    static VALUE_TWO = new Enumeration("VALUE_TWO", 1);

    /**
     * Read Enumeration from input stream
     */
    static async readFrom(stream) {
        const tag = await stream.readInt();
        if (tag == Enumeration.VALUE_ONE.tag) {
            return Enumeration.VALUE_ONE;
        }
        if (tag == Enumeration.VALUE_TWO.tag) {
            return Enumeration.VALUE_TWO;
        }
        throw new Error("Unexpected tag value");
    }

    /**
     * Write Enumeration to output stream
     */
    async writeTo(stream) {
        await stream.writeInt(this.tag);
    }

    [Symbol.for('nodejs.util.inspect.custom')]() {
        return this.name;
    }
}

module.exports = Enumeration;