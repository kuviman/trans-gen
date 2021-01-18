

class OneOf {
    static async readFrom(stream) {
        let tag = await stream.readInt();
        if (tag == OptionOne.TAG) {
            return await OptionOne.readFrom(stream);
        }
        if (tag == OptionTwo.TAG) {
            return await OptionTwo.readFrom(stream);
        }
        throw new Error("Unexpected tag value");
    }
}
class OptionOne extends OneOf {
    constructor(vecI32, longInt) {
        super();
        this.vecI32 = vecI32;
        this.longInt = longInt;
    }

    static async readFrom(stream) {
        let vecI32;
        vecI32 = [];
        for (let vecI32Count = await stream.readInt(); vecI32Count > 0; vecI32Count--) {
            let vecI32Element;
            vecI32Element = await stream.readInt();
            vecI32.push(vecI32Element);
        }
        let longInt;
        longInt = await stream.readLong();
        return new OptionOne(vecI32, longInt);
    }

    async writeTo(stream) {
        await stream.writeInt(OptionOne.TAG);
        let vecI32 = this.vecI32;
        await stream.writeInt(vecI32.length);
        for (let vecI32Element of vecI32) {
            await stream.writeInt(vecI32Element);
        }
        let longInt = this.longInt;
        await stream.writeLong(longInt);
    }
}

OptionOne.TAG = 0;
OneOf.OptionOne = OptionOne;
class OptionTwo extends OneOf {
    constructor(value) {
        super();
        this.value = value;
    }

    static async readFrom(stream) {
        let value;
        value = await stream.readInt();
        return new OptionTwo(value);
    }

    async writeTo(stream) {
        await stream.writeInt(OptionTwo.TAG);
        let value = this.value;
        await stream.writeInt(value);
    }
}

OptionTwo.TAG = 1;
OneOf.OptionTwo = OptionTwo;
module.exports = OneOf;
