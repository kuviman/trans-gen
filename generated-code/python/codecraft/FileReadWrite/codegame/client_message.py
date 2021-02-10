from codegame.debug_command import DebugCommand
from model.action import Action

class ClientMessage:
    """Message sent from client"""

    @staticmethod
    def read_from(stream):
        """Read ClientMessage from input stream
        """
        tag = stream.read_int()
        if tag == DebugMessage.TAG:
            return ClientMessage.DebugMessage.read_from(stream)
        if tag == ActionMessage.TAG:
            return ClientMessage.ActionMessage.read_from(stream)
        if tag == DebugUpdateDone.TAG:
            return ClientMessage.DebugUpdateDone.read_from(stream)
        if tag == RequestDebugState.TAG:
            return ClientMessage.RequestDebugState.read_from(stream)
        raise Exception("Unexpected tag value")

class DebugMessage(ClientMessage):
    """Ask app to perform new debug command"""

    TAG = 0

    def __init__(self, command):
        self.command = command
        """Command to perform"""

    @staticmethod
    def read_from(stream):
        """Read DebugMessage from input stream
        """
        command = DebugCommand.read_from(stream)
        return DebugMessage(command)

    def write_to(self, stream):
        """Write DebugMessage to output stream
        """
        stream.write_int(self.TAG)
        self.command.write_to(stream)

    def __repr__(self):
        return "DebugMessage(" + \
            repr(self.command) + \
            ")"

ClientMessage.DebugMessage = DebugMessage

class ActionMessage(ClientMessage):
    """Reply for ServerMessage::GetAction"""

    TAG = 1

    def __init__(self, action):
        self.action = action
        """Player's action"""

    @staticmethod
    def read_from(stream):
        """Read ActionMessage from input stream
        """
        action = Action.read_from(stream)
        return ActionMessage(action)

    def write_to(self, stream):
        """Write ActionMessage to output stream
        """
        stream.write_int(self.TAG)
        self.action.write_to(stream)

    def __repr__(self):
        return "ActionMessage(" + \
            repr(self.action) + \
            ")"

ClientMessage.ActionMessage = ActionMessage

class DebugUpdateDone(ClientMessage):
    """Signifies finish of the debug update"""

    TAG = 2

    def __init__(self):
        pass

    @staticmethod
    def read_from(stream):
        """Read DebugUpdateDone from input stream
        """
        return DebugUpdateDone()

    def write_to(self, stream):
        """Write DebugUpdateDone to output stream
        """
        stream.write_int(self.TAG)

    def __repr__(self):
        return "DebugUpdateDone(" + \
            ")"

ClientMessage.DebugUpdateDone = DebugUpdateDone

class RequestDebugState(ClientMessage):
    """Request debug state from the app"""

    TAG = 3

    def __init__(self):
        pass

    @staticmethod
    def read_from(stream):
        """Read RequestDebugState from input stream
        """
        return RequestDebugState()

    def write_to(self, stream):
        """Write RequestDebugState to output stream
        """
        stream.write_int(self.TAG)

    def __repr__(self):
        return "RequestDebugState(" + \
            ")"

ClientMessage.RequestDebugState = RequestDebugState