
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
        value: Array<number>
    
        constructor(value: Array<number>) {
            super();
            this.value = value;
        }
    
        static async readFrom(stream: StreamWrapper): Promise<OneOf.OptionOne> {
            let value;
            value = [];
            for (let valueCount = await stream.readInt(); valueCount > 0; valueCount--) {
                let valueElement;
                valueElement = await stream.readInt();
                value.push(valueElement);
            }
            return new OptionOne(value)
        }
    
        async writeTo(stream: StreamWrapper) {
            await stream.writeInt(OptionOne.TAG);
            let value = this.value;
            await stream.writeInt(value.length);
            for (let valueElement of value) {
                await stream.writeInt(valueElement);
            }
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
