import socket_stream;
import std.stdio;
import std.socket;
import std.conv;
import std.exception;
import example;

void main(string[] args)
{
    string host = args[1];
    string port = args[2];
    bool stdout = parse!bool(args[3]);

    auto addr = getAddress(host, port)[0];
    auto socket = new Socket(addr.addressFamily, SocketType.STREAM);
    socket.setOption(SocketOptionLevel.TCP, SocketOption.TCP_NODELAY, true);
    socket.connect(addr);
    auto stream = new SocketStream(socket);
    while (stream.readBool()) {
        Example input = Example.readFrom(stream);
        if (stdout) {
            writeln(input);
        }
        input.writeTo(stream);
        stream.flush();
    }
    socket.close();
}