'use strict';

const Stream = require('./stream');
const Socket = require('net').Socket;

class TcpStream extends Stream {
    constructor(host, port) {
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
    dataHandler(data) {
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
    async read(byteCount) {
        const _this = this;
        return await new Promise(function (resolve, reject) {
            _this.needAmount = byteCount;
            _this.resolve = resolve;
            _this.update();
        });
    }
    async write(data) {
        const _this = this;
        return await new Promise(function (resolve, reject) {
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

module.exports = TcpStream;