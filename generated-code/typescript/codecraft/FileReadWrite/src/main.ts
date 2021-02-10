import { Stream } from "./stream";
import fs from "fs";
import { MessageGameModel } from "./codegame/message-game-model";

class FileStream extends Stream {
    file: number;

    constructor(path: fs.PathLike, flags: fs.OpenMode) {
        super();
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
    const inputFile = process.argv[2];
    const outputFile = process.argv[3];
    const repeat = parseInt(process.argv[4]);

    for (let i = 0; i < repeat; i++) {
        const input = await MessageGameModel.readFrom(new FileStream(inputFile, 'r'));
        if (repeat == 1) {
            console.log(input);
        }
        const outputStream = new FileStream(outputFile, 'w');
        await input.writeTo(outputStream);
        await outputStream.flush();
    }
}

run();