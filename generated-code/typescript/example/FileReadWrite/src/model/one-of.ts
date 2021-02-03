import { Stream } from "../stream";

/**
 * Oneof example
 */
export abstract class OneOf {
    /**
     * Write OneOf to output stream
     */
    abstract writeTo(stream: Stream): Promise<void>;

    /**
     * Read OneOf from input stream
     */
    static async readFrom(stream: Stream): Promise<OneOf> {
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
    /**
     * First option
     */
    export class OptionOne extends OneOf {
        /**
         * List of integers
         */
        vecInt: Array<number>
        /**
         * Long integer
         */
        longInt: bigint
    
        constructor(vecInt: Array<number>, longInt: bigint) {
            super();
            this.vecInt = vecInt;
            this.longInt = longInt;
        }
    
        /**
         * Read OptionOne from input stream
         */
        static async readFrom(stream: Stream): Promise<OneOf.OptionOne> {
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
    
        /**
         * Write OptionOne to output stream
         */
        async writeTo(stream: Stream) {
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
    /**
     * Second option
     */
    export class OptionTwo extends OneOf {
        /**
         * usize
         */
        value: number
    
        constructor(value: number) {
            super();
            this.value = value;
        }
    
        /**
         * Read OptionTwo from input stream
         */
        static async readFrom(stream: Stream): Promise<OneOf.OptionTwo> {
            let value;
            value = await stream.readInt();
            return new OptionTwo(value)
        }
    
        /**
         * Write OptionTwo to output stream
         */
        async writeTo(stream: Stream) {
            await stream.writeInt(OptionTwo.TAG);
            let value = this.value;
            await stream.writeInt(value);
        }
    }
    
    export namespace OptionTwo {
        export const TAG = 1;
    }
}