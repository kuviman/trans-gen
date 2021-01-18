
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
        vecI32: Array<number>
        longInt: bigint
    
        constructor(vecI32: Array<number>, longInt: bigint) {
            super();
            this.vecI32 = vecI32;
            this.longInt = longInt;
        }
    
        static async readFrom(stream: StreamWrapper): Promise<OneOf.OptionOne> {
            let vecI32;
            vecI32 = [];
            for (let vecI32Count = await stream.readInt(); vecI32Count > 0; vecI32Count--) {
                let vecI32Element;
                vecI32Element = await stream.readInt();
                vecI32.push(vecI32Element);
            }
            let longInt;
            longInt = await stream.readLong();
            return new OptionOne(vecI32, longInt)
        }
    
        async writeTo(stream: StreamWrapper) {
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
