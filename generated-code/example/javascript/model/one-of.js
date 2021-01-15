

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
    constructor(value) {
        super();
        this.value = value;
    }

    static async readFrom(stream) {
        let value;
        value = [];
        for (let valueCount = await stream.readInt(); valueCount > 0; valueCount--) {
            let valueElement;
            valueElement = await stream.readInt();
            value.push(valueElement);
        }
        return new OptionOne(value);
    }

    async writeTo(stream) {
        await stream.writeInt(OptionOne.TAG);
        let value = this.value;
        await stream.writeInt(value.length);
        for (let valueElement of value) {
            await stream.writeInt(valueElement);
        }
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
