'use strict';

const Stream = require('./stream');
const model = require('./model/index');
const fs = require('fs');

class FileStream extends Stream {
    constructor(path, flags) {
        super();
        this.file = fs.openSync(path, flags);
    }
    async read(byteCount) {
        const buffer = Buffer.alloc(byteCount);
        const readBytes = fs.readSync(this.file, buffer, 0, byteCount, null);
        if (readBytes != byteCount) {
            throw new Error("Unexpected EOF");
        }
        return buffer;
    }
    async write(data) {
        fs.writeSync(this.file, data);
    }
    async flush() { }
}

async function run() {
    if (process.argv.length != 4) {
        throw new Error("Pass input and output as parameters")
    }

    const inputFile = process.argv[2];
    const outputFile = process.argv[3];

    const input = await model.Example.readFrom(new FileStream(inputFile, 'r'));
    console.log(input);
    const outputStream = new FileStream(outputFile, 'w');
    await input.writeTo(outputStream);
    await outputStream.flush();
}

run();