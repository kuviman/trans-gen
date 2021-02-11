from stream_wrapper import StreamWrapper
from vec2_int import Vec2Int

class MoveAction:
    """Move action"""

    __slots__ = ("target","find_closest_position","break_through",)

    target: Vec2Int
    find_closest_position: bool
    break_through: bool

    def __init__(self, target: Vec2Int, find_closest_position: bool, break_through: bool):
        self.target = target
        """Target position"""
        self.find_closest_position = find_closest_position
        """Whether to try find closest position, if path to target is not found"""
        self.break_through = break_through
        """Whether to destroy other entities on the way"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "MoveAction":
        """Read MoveAction from input stream
        """
        target = Vec2Int.read_from(stream)
        find_closest_position = stream.read_bool()
        break_through = stream.read_bool()
        return MoveAction(target, find_closest_position, break_through)
    
    def write_to(self, stream: StreamWrapper):
        """Write MoveAction to output stream
        """
        self.target.write_to(stream)
        stream.write_bool(self.find_closest_position)
        stream.write_bool(self.break_through)
    
    def __repr__(self):
        return "MoveAction(" + \
            repr(self.target) + \
            ", " + \
            repr(self.find_closest_position) + \
            ", " + \
            repr(self.break_through) + \
            ")"