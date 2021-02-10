/**
 * Primitive type for debug rendering
 */
class PrimitiveType {
    constructor(name, tag) {
        this.name = name;
        this.tag = tag;
    }

    /**
     * Lines, number of vertices should be divisible by 2
     */
    static LINES = new PrimitiveType("LINES", 0);
    /**
     * Triangles, number of vertices should be divisible by 3
     */
    static TRIANGLES = new PrimitiveType("TRIANGLES", 1);

    /**
     * Read PrimitiveType from input stream
     */
    static async readFrom(stream) {
        const tag = await stream.readInt();
        if (tag == PrimitiveType.LINES.tag) {
            return PrimitiveType.LINES;
        }
        if (tag == PrimitiveType.TRIANGLES.tag) {
            return PrimitiveType.TRIANGLES;
        }
        throw new Error("Unexpected tag value");
    }

    /**
     * Write PrimitiveType to output stream
     */
    async writeTo(stream) {
        await stream.writeInt(this.tag);
    }

    [Symbol.for('nodejs.util.inspect.custom')]() {
        return this.name;
    }
}

module.exports = PrimitiveType;