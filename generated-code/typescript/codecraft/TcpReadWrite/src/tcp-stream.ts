import { Stream } from "./stream";
import { Socket } from "net";

export class TcpStream extends Stream {
    private socket: Socket;
    private data: Buffer;
    private needAmount: number | null;
    private resolve: any;
    constructor(host: string, port: number) {
        super();
        const _this = this;
        this.socket = new Socket().setNoDelay(true);
        this.socket
            .setNoDelay(true)
            .on('error', (error) => {
                console.error('Socket error: ' + error.message);
                process.exit(1);
            });
        new Promise(function (resolve) {
            _this.socket.connect(port, host, () => resolve(undefined));
        });
        this.data = Buffer.alloc(0);
        this.needAmount = null;
        this.resolve = null;
        this.socket.on('data', function (data) { _this.dataHandler(data) });
    }
    destroy() {
        this.socket.destroy();
    }
    dataHandler(data: Buffer) {
        this.data = Buffer.concat([this.data, data]);
        this.update();
    }
    update() {
        if (this.needAmount === null || this.needAmount > this.data.length) {
            return;
        }
        const data = this.data.slice(0, this.needAmount);
        this.data = this.data.slice(this.needAmount);
        this.needAmount = null;
        this.resolve(data);
        this.update();
    }
    async read(byteCount: number): Promise<Buffer> {
        const _this = this;
        return await new Promise<Buffer>(function (resolve, reject) {
            _this.needAmount = byteCount;
            _this.resolve = resolve;
            _this.update();
        });
    }
    async write(data: Buffer): Promise<void> {
        const _this = this;
        return await new Promise<void>(function (resolve, reject) {
            _this.socket.write(data, 'utf8', function (error) {
                if (error) {
                    return reject(error);
                }
                resolve(undefined);
            });
        });
    }
    async flush() { }
}