import { StreamWrapper, Stream } from "./stream-wrapper";
import * as model from "./model";
import fs from "fs";

class FileWrapper implements Stream {
    file: number;

    constructor(path: fs.PathLike, flags: fs.OpenMode) {
        this.file = fs.openSync(path, flags);
    }

    async read(byteCount: number): Promise<Buffer> {
        const buffer = Buffer.alloc(byteCount);
        const readBytes = fs.readSync(this.file, buffer, 0, byteCount, null);
        if (readBytes != byteCount) {
            throw new Error("Unexpected EOF");
        }
        return buffer;
    }
    async write(data: Buffer) {
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

    const input = await model.Example.readFrom(new StreamWrapper(new FileWrapper(inputFile, 'r')));
    const outputStream = new StreamWrapper(new FileWrapper(outputFile, 'w'));
    input.writeTo(outputStream);
    outputStream.flush();
}

run();