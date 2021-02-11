from stream_wrapper import StreamWrapper

class Player:
    """Player (strategy, client)"""

    __slots__ = ("id","score","resource",)

    id: int
    score: int
    resource: int

    def __init__(self, id: int, score: int, resource: int):
        self.id = id
        """Player's ID"""
        self.score = score
        """Current score"""
        self.resource = resource
        """Current amount of resource"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "Player":
        """Read Player from input stream
        """
        id = stream.read_int()
        score = stream.read_int()
        resource = stream.read_int()
        return Player(id, score, resource)
    
    def write_to(self, stream: StreamWrapper):
        """Write Player to output stream
        """
        stream.write_int(self.id)
        stream.write_int(self.score)
        stream.write_int(self.resource)
    
    def __repr__(self):
        return "Player(" + \
            repr(self.id) + \
            ", " + \
            repr(self.score) + \
            ", " + \
            repr(self.resource) + \
            ")"