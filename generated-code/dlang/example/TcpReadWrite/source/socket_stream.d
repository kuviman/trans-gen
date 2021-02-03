import stream;
import std.socket;
import std.exception;

class SocketStream : Stream
{
    this(Socket socket)
    {
        this.socket = socket;
    }

    override ubyte[] readBytes(size_t byteCount)
    {
        ubyte[] data = new ubyte[byteCount];
        size_t offset = 0;
        while (offset < byteCount)
        {
            auto received = socket.receive(data[offset .. data.length]);
            enforce(received > 0);
            offset += received;
        }
        return data;
    }

    override void writeBytes(const ubyte[] data)
    {
        size_t offset = 0;
        while (offset < data.length)
        {
            auto sent = socket.send(data[offset .. data.length]);
            enforce(sent > 0);
            offset += sent;
        }
    }

    override void flush()
    {
    }

private:
    Socket socket;
}
