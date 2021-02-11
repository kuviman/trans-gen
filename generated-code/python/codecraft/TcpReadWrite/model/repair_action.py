from stream_wrapper import StreamWrapper

class RepairAction:
    """Repair action"""

    __slots__ = ("target",)

    target: int

    def __init__(self, target: int):
        self.target = target
        """Target entity's ID"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "RepairAction":
        """Read RepairAction from input stream
        """
        target = stream.read_int()
        return RepairAction(target)
    
    def write_to(self, stream: StreamWrapper):
        """Write RepairAction to output stream
        """
        stream.write_int(self.target)
    
    def __repr__(self):
        return "RepairAction(" + \
            repr(self.target) + \
            ")"