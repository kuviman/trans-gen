const Enumeration = require('./enumeration');
const OneOf = require('./one-of');
const Structure = require('./structure');
class Example {
    constructor(oneOf, hashMap, optionalInt, optionalBool, optionalOneOf, optionalStruct, optionalEnum) {
        this.oneOf = oneOf;
        this.hashMap = hashMap;
        this.optionalInt = optionalInt;
        this.optionalBool = optionalBool;
        this.optionalOneOf = optionalOneOf;
        this.optionalStruct = optionalStruct;
        this.optionalEnum = optionalEnum;
    }

    static async readFrom(stream) {
        let oneOf;
        oneOf = await OneOf.readFrom(stream);
        let hashMap;
        hashMap = new Map();
        for (let hashMapCount = await stream.readInt(); hashMapCount > 0; hashMapCount--) {
            let hashMapKey;
            let hashMapValue;
            hashMapKey = await Enumeration.readFrom(stream);
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
            optionalEnum = await Enumeration.readFrom(stream);
        } else {
            optionalEnum = null;
        }
        return new Example(oneOf, hashMap, optionalInt, optionalBool, optionalOneOf, optionalStruct, optionalEnum);
    }

    async writeTo(stream) {
        let oneOf = this.oneOf;
        await oneOf.writeTo(stream);
        let hashMap = this.hashMap;
        await stream.writeInt(hashMap.size);
        for (let [hashMapKey, hashMapValue] of hashMap) {
            await hashMapKey.writeTo(stream);
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
            await optionalEnum.writeTo(stream);
        }
    }
}
module.exports = Example