/**
 * Example structure
 */
class Structure {
    /**
     * Text
     */
    text;
    /**
     * 32-bit float
     */
    floatNumber;
    /**
     * 64-bit float
     */
    doubleNumber;

    constructor(text, floatNumber, doubleNumber) {
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }

    /**
     * Read Structure from input stream
     */
    static async readFrom(stream) {
        let text;
        text = await stream.readString();
        let floatNumber;
        floatNumber = await stream.readFloat();
        let doubleNumber;
        doubleNumber = await stream.readDouble();
        return new Structure(text, floatNumber, doubleNumber);
    }

    /**
     * Write Structure to output stream
     */
    async writeTo(stream) {
        let text = this.text;
        await stream.writeString(text);
        let floatNumber = this.floatNumber;
        await stream.writeFloat(floatNumber);
        let doubleNumber = this.doubleNumber;
        await stream.writeDouble(doubleNumber);
    }
}
module.exports = Structure