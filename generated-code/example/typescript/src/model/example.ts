import { Enumeration } from "./enumeration";
import { OneOf } from "./one-of";
import { Structure } from "./structure";
import { StreamWrapper } from "../stream-wrapper";

export class Example {
    oneOf: OneOf
    hashMap: Map<Enumeration, number>
    optionalInt: number | null
    optionalBool: boolean | null
    optionalOneOf: OneOf | null
    optionalStruct: Structure | null
    optionalEnum: Enumeration | null

    constructor(oneOf: OneOf, hashMap: Map<Enumeration, number>, optionalInt: number | null, optionalBool: boolean | null, optionalOneOf: OneOf | null, optionalStruct: Structure | null, optionalEnum: Enumeration | null) {
        this.oneOf = oneOf;
        this.hashMap = hashMap;
        this.optionalInt = optionalInt;
        this.optionalBool = optionalBool;
        this.optionalOneOf = optionalOneOf;
        this.optionalStruct = optionalStruct;
        this.optionalEnum = optionalEnum;
    }

    static async readFrom(stream: StreamWrapper): Promise<Example> {
        let oneOf;
        oneOf = await OneOf.readFrom(stream);
        let hashMap;
        hashMap = new Map();
        for (let hashMapCount = await stream.readInt(); hashMapCount > 0; hashMapCount--) {
            let hashMapKey;
            let hashMapValue;
            hashMapKey = await stream.readInt();
            hashMapValue = await stream.readInt();
            hashMap.set(hashMapKey, hashMapValue)
        }
        let optionalInt;
        if (await stream.readBool()) {
            optionalInt = await stream.readInt();
        } else {
            optionalInt = null;
        }
        let optionalBool;
        if (await stream.readBool()) {
            optionalBool = await stream.readBool();
        } else {
            optionalBool = null;
        }
        let optionalOneOf;
        if (await stream.readBool()) {
            optionalOneOf = await OneOf.readFrom(stream);
        } else {
            optionalOneOf = null;
        }
        let optionalStruct;
        if (await stream.readBool()) {
            optionalStruct = await Structure.readFrom(stream);
        } else {
            optionalStruct = null;
        }
        let optionalEnum;
        if (await stream.readBool()) {
            optionalEnum = await stream.readInt();
        } else {
            optionalEnum = null;
        }
        return new Example(oneOf, hashMap, optionalInt, optionalBool, optionalOneOf, optionalStruct, optionalEnum)
    }

    async writeTo(stream: StreamWrapper) {
        let oneOf = this.oneOf;
        await oneOf.writeTo(stream);
        let hashMap = this.hashMap;
        await stream.writeInt(hashMap.size);
        for (let [hashMapKey, hashMapValue] of hashMap) {
            await stream.writeInt(hashMapKey);
            await stream.writeInt(hashMapValue);
        }
        let optionalInt = this.optionalInt;
        if (optionalInt === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeInt(optionalInt);
        }
        let optionalBool = this.optionalBool;
        if (optionalBool === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeBool(optionalBool);
        }
        let optionalOneOf = this.optionalOneOf;
        if (optionalOneOf === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await optionalOneOf.writeTo(stream);
        }
        let optionalStruct = this.optionalStruct;
        if (optionalStruct === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await optionalStruct.writeTo(stream);
        }
        let optionalEnum = this.optionalEnum;
        if (optionalEnum === null) {
            await stream.writeBool(false);
        } else {
            await stream.writeBool(true);
            await stream.writeInt(optionalEnum);
        }
    }
}