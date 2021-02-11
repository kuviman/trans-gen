from codegame.client_message import ClientMessage
from codegame.server_message import ServerMessage
from stream_wrapper import StreamWrapper

class MessageGameModel:
    """Client or server message"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "MessageGameModel":
        """Read MessageGameModel from input stream
        """
        tag = stream.read_int()
        if tag == Client.TAG:
            return MessageGameModel.Client.read_from(stream)
        if tag == Server.TAG:
            return MessageGameModel.Server.read_from(stream)
        raise Exception("Unexpected tag value")

class Client(MessageGameModel):
    """Client message"""

    TAG = 0

    __slots__ = ("message",)

    message: ClientMessage

    def __init__(self, message: ClientMessage):
        self.message = message
        """Message"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "Client":
        """Read Client from input stream
        """
        message = ClientMessage.read_from(stream)
        return Client(message)
    
    def write_to(self, stream: StreamWrapper):
        """Write Client to output stream
        """
        stream.write_int(self.TAG)
        self.message.write_to(stream)
    
    def __repr__(self):
        return "Client(" + \
            repr(self.message) + \
            ")"

MessageGameModel.Client = Client

class Server(MessageGameModel):
    """Server message"""

    TAG = 1

    __slots__ = ("message",)

    message: ServerMessage

    def __init__(self, message: ServerMessage):
        self.message = message
        """Message"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "Server":
        """Read Server from input stream
        """
        message = ServerMessage.read_from(stream)
        return Server(message)
    
    def write_to(self, stream: StreamWrapper):
        """Write Server to output stream
        """
        stream.write_int(self.TAG)
        self.message.write_to(stream)
    
    def __repr__(self):
        return "Server(" + \
            repr(self.message) + \
            ")"

MessageGameModel.Server = Server