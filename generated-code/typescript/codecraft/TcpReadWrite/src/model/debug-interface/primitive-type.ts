import { Stream } from "../../stream";

/**
 * Primitive type for debug rendering
 */
export class PrimitiveType {
    readonly name: string;
    readonly tag: number;

    constructor(name: string, tag: number) {
        this.name = name;
        this.tag = tag;
    }

    /**
     * Lines, number of vertices should be divisible by 2
     */
    static readonly LINES = new PrimitiveType("LINES", 0);
    /**
     * Triangles, number of vertices should be divisible by 3
     */
    static readonly TRIANGLES = new PrimitiveType("TRIANGLES", 1);

    /**
     * Read PrimitiveType from input stream
     */
    static async readFrom(stream: Stream): Promise<PrimitiveType> {
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
    async writeTo(stream: Stream) {
        await stream.writeInt(this.tag);
    }

    [Symbol.for('nodejs.util.inspect.custom')]() {
        return this.name;
    }
}