import { Enumeration } from "./enumeration";
import { OneOf } from "./one-of";
import { StreamWrapper } from "../stream-wrapper";

export class Structure {
    oneOfOne: OneOf
    oneOfTwo: OneOf
    hashMap: Map<Enumeration, number>
    text: string
    floatNumber: number
    doubleNumber: number

    constructor(oneOfOne: OneOf, oneOfTwo: OneOf, hashMap: Map<Enumeration, number>, text: string, floatNumber: number, doubleNumber: number) {
        this.oneOfOne = oneOfOne;
        this.oneOfTwo = oneOfTwo;
        this.hashMap = hashMap;
        this.text = text;
        this.floatNumber = floatNumber;
        this.doubleNumber = doubleNumber;
    }

    static async readFrom(stream: StreamWrapper): Promise<Structure> {
        let oneOfOne;
        oneOfOne = await OneOf.readFrom(stream);
        let oneOfTwo;
        oneOfTwo = await OneOf.readFrom(stream);
        let hashMap;
        hashMap = new Map();
        for (let hashMapCount = await stream.readInt(); hashMapCount > 0; hashMapCount--) {
            let hashMapKey;
            let hashMapValue;
            hashMapKey = await stream.readInt();
            hashMapValue = await stream.readInt();
            hashMap.set(hashMapKey, hashMapValue)
        }
        let text;
        text = await stream.readString();
        let floatNumber;
        floatNumber = await stream.readFloat();
        let doubleNumber;
        doubleNumber = await stream.readDouble();
        return new Structure(oneOfOne, oneOfTwo, hashMap, text, floatNumber, doubleNumber)
    }

    async writeTo(stream: StreamWrapper) {
        let oneOfOne = this.oneOfOne;
        await oneOfOne.writeTo(stream);
        let oneOfTwo = this.oneOfTwo;
        await oneOfTwo.writeTo(stream);
        let hashMap = this.hashMap;
        await stream.writeInt(hashMap.size);
        for (let [hashMapKey, hashMapValue] of hashMap) {
            await stream.writeInt(hashMapKey);
            await stream.writeInt(hashMapValue);
        }
        let text = this.text;
        await stream.writeString(text);
        let floatNumber = this.floatNumber;
        await stream.writeFloat(floatNumber);
        let doubleNumber = this.doubleNumber;
        await stream.writeDouble(doubleNumber);
    }
}
