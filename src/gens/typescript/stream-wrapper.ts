import os from "os";

export interface Stream {
    read(byteCount: number): Promise<Buffer>;
    write(data: Buffer): void;
    flush(): void;
}

const BOOL_SIZE = 1;
const INT_SIZE = 4;
const LONG_SIZE = 8;
const FLOAT_SIZE = 4;
const DOUBLE_SIZE = 8;

export class StreamWrapper {
    stream: Stream;
    isLittleEndianMachine: boolean;

    constructor(stream: Stream) {
        this.stream = stream;
        this.isLittleEndianMachine = (os.endianness() === 'LE');
    }

    async flush() {
        this.stream.flush();
    }

    // Reading primitives

    async readBool(): Promise<boolean> {
        const buffer = await this.stream.read(BOOL_SIZE);
        return !!buffer.readInt8();
    }

    async readInt(): Promise<number> {
        const buffer = await this.stream.read(INT_SIZE);
        if (this.isLittleEndianMachine) {
            return buffer.readInt32LE(0);
        }
        return buffer.readInt32BE(0);
    }

    async readLong(): Promise<bigint> {
        const buffer = await this.stream.read(LONG_SIZE);
        if (this.isLittleEndianMachine) {
            return buffer.readBigInt64LE();
        }
        return buffer.readBigInt64BE();
    }

    async readFloat(): Promise<number> {
        const buffer = await this.stream.read(FLOAT_SIZE);
        if (this.isLittleEndianMachine) {
            return buffer.readFloatLE();
        }
        return buffer.readFloatBE();
    }

    async readDouble(): Promise<number> {
        const buffer = await this.stream.read(DOUBLE_SIZE);
        if (this.isLittleEndianMachine) {
            return buffer.readDoubleLE();
        }
        return buffer.readDoubleBE();
    }

    async readString(): Promise<string> {
        const length = await this.readInt();
        const buffer = await this.stream.read(length);
        return buffer.toString();
    }

    // Writing primitives

    async writeBool(value: boolean) {
        const buffer = Buffer.alloc(BOOL_SIZE);
        buffer.writeInt8(value ? 1 : 0);
        return await this.stream.write(buffer);
    }

    async writeInt(value: number) {
        const buffer = Buffer.alloc(INT_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeInt32LE(value);
        } else {
            buffer.writeInt32BE(value);
        }
        return await this.stream.write(buffer);
    }

    async writeLong(value: bigint) {
        const buffer = Buffer.alloc(LONG_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeBigInt64LE(value);
        } else {
            buffer.writeBigInt64BE(value);
        }
        return await this.stream.write(buffer);
    }

    async writeFloat(value: number) {
        const buffer = Buffer.alloc(FLOAT_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeFloatLE(value);
        } else {
            buffer.writeFloatBE(value);
        }
        return await this.stream.write(buffer);
    }

    async writeDouble(value: number) {
        const buffer = Buffer.alloc(DOUBLE_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeDoubleLE(value);
        } else {
            buffer.writeDoubleBE(value);
        }
        return await this.stream.write(buffer);
    }

    async writeString(value: string) {
        const data = Buffer.from(value);
        this.writeInt(data.length);
        return await this.stream.write(data);
    }
}