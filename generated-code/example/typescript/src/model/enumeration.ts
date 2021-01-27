import { StreamWrapper } from "../stream-wrapper";

/**
 * Example enumeration
 */
export class Enumeration {
    readonly name: string;
    readonly tag: number;

    constructor(name: string, tag: number) {
        this.name = name;
        this.tag = tag;
    }

    /**
     * First option
     */
    static readonly VALUE_ONE = new Enumeration("VALUE_ONE", 0);
    /**
     * Second option
     */
    static readonly VALUE_TWO = new Enumeration("VALUE_TWO", 1);

    /**
     * Read Enumeration from input stream
     */
    static async readFrom(stream: StreamWrapper): Promise<Enumeration> {
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
    async writeTo(stream: StreamWrapper) {
        await stream.writeInt(this.tag);
    }

    [Symbol.for('nodejs.util.inspect.custom')]() {
        return this.name;
    }
}