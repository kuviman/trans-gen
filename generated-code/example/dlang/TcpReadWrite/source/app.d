import model;
import socket_stream;
import std.stdio;
import std.socket;
import std.conv;
import std.exception;

void main(string[] args)
{
    if (args.length != 3)
    {
        throw new Error("Pass host and port as parameters");
    }

    string host = args[1];
    string port = args[2];

    auto addr = getAddress(host, port)[0];
    auto socket = new Socket(addr.addressFamily, SocketType.STREAM);
    socket.setOption(SocketOptionLevel.TCP, SocketOption.TCP_NODELAY, true);
    socket.connect(addr);
    auto stream = new SocketStream(socket);
    Example input = Example.readFrom(stream);
    writeln(input);
    input.writeTo(stream);
    stream.flush();
    socket.close();
}