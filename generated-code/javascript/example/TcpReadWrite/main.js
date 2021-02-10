'use strict';

const TcpStream = require('./tcp-stream');
const Example = require('./example');

async function run() {
    const host = process.argv[2];
    const port = parseInt(process.argv[3]);
    const stdout = process.argv[4] == "true";

    let tcpStream = new TcpStream(host, port);

    while (await tcpStream.readBool()) {
        const input = await Example.readFrom(tcpStream);
        if (stdout) {
            console.log(input);
        }
        await input.writeTo(tcpStream);
        await tcpStream.flush();
    }
    tcpStream.destroy();
}

run();