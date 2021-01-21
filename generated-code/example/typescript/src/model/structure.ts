import { StreamWrapper } from "../stream-wrapper";

export class Structure {
    text: string
    floatNumber: number
    doubleNumber: number

    constructor(text: string, floatNumber: number, doubleNumber: number) {
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }

    static async readFrom(stream: StreamWrapper): Promise<Structure> {
        let text;
        text = await stream.readString();
        let floatNumber;
        floatNumber = await stream.readFloat();
        let doubleNumber;
        doubleNumber = await stream.readDouble();
        return new Structure(text, floatNumber, doubleNumber)
    }

    async writeTo(stream: StreamWrapper) {
        let text = this.text;
        await stream.writeString(text);
        let floatNumber = this.floatNumber;
        await stream.writeFloat(floatNumber);
        let doubleNumber = this.doubleNumber;
        await stream.writeDouble(doubleNumber);
    }
}