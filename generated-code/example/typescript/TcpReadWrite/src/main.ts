import { TcpStream } from "./tcp-stream";
import * as model from "./model";

async function run() {
    if (process.argv.length != 4) {
        throw new Error("Pass host and port as parameters")
    }

    const host = process.argv[2];
    const port = parseInt(process.argv[3]);

    let tcpStream = new TcpStream(host, port);

    const input = await model.Example.readFrom(tcpStream);
    console.log(input);
    await input.writeTo(tcpStream);
    await tcpStream.flush();
    tcpStream.destroy();
}

run();