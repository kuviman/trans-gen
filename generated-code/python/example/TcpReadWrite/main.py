from example import Example
from stream_wrapper import StreamWrapper
import sys
import socket


if __name__ == "__main__":
    host = sys.argv[1]
    port = int(sys.argv[2])
    stdout = bool(sys.argv[3])

    sock = socket.socket()
    sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, True)
    sock.connect((host, port))
    stream = StreamWrapper(sock.makefile('rwb'))

    while stream.read_bool():
        input = Example.read_from(stream)
        if stdout:
            print(repr(input))
        input.write_to(stream)
        stream.flush()

    sock.close()