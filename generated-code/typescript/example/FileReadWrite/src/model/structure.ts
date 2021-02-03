import { Stream } from "../stream";

/**
 * Example structure
 */
export class Structure {
    /**
     * Text
     */
    text: string
    /**
     * 32-bit float
     */
    floatNumber: number
    /**
     * 64-bit float
     */
    doubleNumber: number

    constructor(text: string, floatNumber: number, doubleNumber: number) {
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }

    /**
     * Read Structure from input stream
     */
    static async readFrom(stream: Stream): Promise<Structure> {
        let text;
        text = await stream.readString();
        let floatNumber;
        floatNumber = await stream.readFloat();
        let doubleNumber;
        doubleNumber = await stream.readDouble();
        return new Structure(text, floatNumber, doubleNumber)
    }

    /**
     * Write Structure to output stream
     */
    async writeTo(stream: Stream) {
        let text = this.text;
        await stream.writeString(text);
        let floatNumber = this.floatNumber;
        await stream.writeFloat(floatNumber);
        let doubleNumber = this.doubleNumber;
        await stream.writeDouble(doubleNumber);
    }
}