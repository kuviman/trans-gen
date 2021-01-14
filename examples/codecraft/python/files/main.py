import model
from stream_wrapper import StreamWrapper
import sys


class Runner:
    def __init__(self, host, port, token):
        self.socket = socket.socket()
        self.socket.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, True)
        self.socket.connect((host, port))
        socket_stream = self.socket.makefile('rwb')
        self.reader = StreamWrapper(socket_stream)
        self.writer = StreamWrapper(socket_stream)
        self.token = token
        self.writer.write_string(self.token)
        self.writer.flush()

    def run(self):
        strategy = MyStrategy()
        debug_interface = DebugInterface(self.reader, self.writer)

        while True:
            message = model.ServerMessage.read_from(self.reader)
            if isinstance(message, model.ServerMessage.GetAction):
                model.ClientMessage.ActionMessage(strategy.get_action(
                    message.player_view, debug_interface if message.debug_available else None)).write_to(self.writer)
                self.writer.flush()
            elif isinstance(message, model.ServerMessage.Finish):
                break
            elif isinstance(message, model.ServerMessage.DebugUpdate):
                strategy.debug_update(message.player_view, debug_interface)
                model.ClientMessage.DebugUpdateDone().write_to(self.writer)
                self.writer.flush()
            else:
                raise Exception("Unexpected server message")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        raise Exception("Pass input and output as parameters")

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    input = model.PlayerView.read_from(StreamWrapper(open(input_file, 'rb')))
    input.write_to(StreamWrapper(open(output_file, 'wb')))
