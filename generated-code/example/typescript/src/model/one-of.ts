
import { StreamWrapper } from "../stream-wrapper";

export abstract class OneOf {
    abstract writeTo(stream: StreamWrapper): Promise<void>;
    static async readFrom(stream: StreamWrapper): Promise<OneOf> {
        const tag = await stream.readInt();
        if (tag == OneOf.OptionOne.TAG) {
            return await OneOf.OptionOne.readFrom(stream);
        }
        if (tag == OneOf.OptionTwo.TAG) {
            return await OneOf.OptionTwo.readFrom(stream);
        }
        throw new Error("Unexpected tag value");
    }
}

export namespace OneOf {
    export class OptionOne extends OneOf {
        vecInt: Array<number>
        longInt: bigint
    
        constructor(vecInt: Array<number>, longInt: bigint) {
            super();
            this.vecInt = vecInt;
            this.longInt = longInt;
        }
    
        static async readFrom(stream: StreamWrapper): Promise<OneOf.OptionOne> {
            let vecInt;
            vecInt = [];
            for (let vecIntCount = await stream.readInt(); vecIntCount > 0; vecIntCount--) {
                let vecIntElement;
                vecIntElement = await stream.readInt();
                vecInt.push(vecIntElement);
            }
            let longInt;
            longInt = await stream.readLong();
            return new OptionOne(vecInt, longInt)
        }
    
        async writeTo(stream: StreamWrapper) {
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
    
    export namespace OptionOne {
        export const TAG = 0;
    }
    export class OptionTwo extends OneOf {
        value: number
    
        constructor(value: number) {
            super();
            this.value = value;
        }
    
        static async readFrom(stream: StreamWrapper): Promise<OneOf.OptionTwo> {
            let value;
            value = await stream.readInt();
            return new OptionTwo(value)
        }
    
        async writeTo(stream: StreamWrapper) {
            await stream.writeInt(OptionTwo.TAG);
            let value = this.value;
            await stream.writeInt(value);
        }
    }
    
    export namespace OptionTwo {
        export const TAG = 1;
    }
}
