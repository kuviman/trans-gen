import model
from stream_wrapper import StreamWrapper
import sys
import socket


if __name__ == "__main__":
    if len(sys.argv) != 3:
        raise Exception("Pass host and port as parameters")

    host = sys.argv[1]
    port = int(sys.argv[2])

    sock = socket.socket()
    sock.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, True)
    sock.connect((host, port))
    stream = StreamWrapper(sock.makefile('rwb'))

    input = model.Example.read_from(stream)
    print(repr(input))
    input.write_to(stream)
    stream.flush()

    sock.close()