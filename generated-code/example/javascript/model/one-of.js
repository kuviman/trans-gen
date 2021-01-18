

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
    constructor(vecInt, longInt) {
        super();
        this.vecInt = vecInt;
        this.longInt = longInt;
    }

    static async readFrom(stream) {
        let vecInt;
        vecInt = [];
        for (let vecIntCount = await stream.readInt(); vecIntCount > 0; vecIntCount--) {
            let vecIntElement;
            vecIntElement = await stream.readInt();
            vecInt.push(vecIntElement);
        }
        let longInt;
        longInt = await stream.readLong();
        return new OptionOne(vecInt, longInt);
    }

    async writeTo(stream) {
        await stream.writeInt(OptionOne.TAG);
        let vecInt = this.vecInt;
        await stream.writeInt(vecInt.length);
        for (let vecIntElement of vecInt) {
            await stream.writeInt(vecIntElement);
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
