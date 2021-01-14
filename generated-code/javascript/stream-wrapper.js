'use strict';

const os = require('os');

const BOOL_SIZE = 1;
const INT_SIZE = 4;
const LONG_SIZE = 8;
const FLOAT_SIZE = 4;
const DOUBLE_SIZE = 8;

class StreamWrapper {
    constructor(stream) {
        this.stream = stream;
        this.isLittleEndianMachine = (os.endianness() === 'LE');
    }

    async flush() {
        this.stream.flush();
    }

    // Reading primitives
    async readBool() {
        const buffer = await this.stream.read(BOOL_SIZE);
        return !!buffer.readInt8();
    }

    async readInt() {
        const buffer = await this.stream.read(INT_SIZE);
        if (this.isLittleEndianMachine) {
            return parseInt(buffer.readInt32LE(0, INT_SIZE));
        }
        return parseInt(buffer.readInt32BE(0, INT_SIZE));
    }

    async readLong() {
        const buffer = await this.stream.read(LONG_SIZE);
        if (this.isLittleEndianMachine) {
            return parseInt(buffer.readBigInt64LE());
        }
        return parseInt(buffer.readBigInt64BE());
    }

    async readFloat() {
        const buffer = await this.stream.read(FLOAT_SIZE);
        if (this.isLittleEndianMachine) {
            return buffer.readFloatLE();
        }
        return buffer.readFloatBE();
    }

    async readDouble() {
        const buffer = await this.stream.read(DOUBLE_SIZE);
        if (this.isLittleEndianMachine) {
            return buffer.readDoubleLE();
        }
        return buffer.readDoubleBE();
    }

    async readString() {
        const length = await this.readInt();
        const buffer = await this.stream.read(length);
        const result = buffer.toString();
        if (result.length !== length) {
            throw new Error('Unexpected EOF');
        }
        return result;
    }

    // Writing primitives

    async writeBool(value) {
        const buffer = Buffer.alloc(BOOL_SIZE);
        buffer.writeInt8(value);
        return await this.stream.write(buffer);
    }

    async writeInt(value) {
        const buffer = Buffer.alloc(INT_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeInt32LE(value);
        } else {
            buffer.writeInt32BE(value);
        }
        return await this.stream.write(buffer);
    }

    async writeLong(value) {
        const buffer = Buffer.alloc(LONG_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeBigInt64LE(value);
        } else {
            buffer.writeBigInt64BE(value);
        }
        return await this.stream.write(buffer);
    }

    async writeFloat(value) {
        const buffer = Buffer.alloc(FLOAT_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeFloatLE(value);
        } else {
            buffer.writeFloatBE(value);
        }
        return await this.stream.write(buffer);
    }

    async writeDouble(value) {
        const buffer = Buffer.alloc(DOUBLE_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeDoubleLE(value);
        } else {
            buffer.writeDoubleBE(value);
        }
        return await this.stream.write(buffer);
    }

    async writeString(value) {
        this.writeInt(value.length);
        return await this.stream.write(value, 'utf8');
    }
}

module.exports = StreamWrapper;