from stream_wrapper import StreamWrapper

class Color:
    """RGBA Color"""

    __slots__ = ("r","g","b","a",)

    r: float
    g: float
    b: float
    a: float

    def __init__(self, r: float, g: float, b: float, a: float):
        self.r = r
        """Red component"""
        self.g = g
        """Green component"""
        self.b = b
        """Blue component"""
        self.a = a
        """Alpha (opacity) component"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "Color":
        """Read Color from input stream
        """
        r = stream.read_float()
        g = stream.read_float()
        b = stream.read_float()
        a = stream.read_float()
        return Color(r, g, b, a)
    
    def write_to(self, stream: StreamWrapper):
        """Write Color to output stream
        """
        stream.write_float(self.r)
        stream.write_float(self.g)
        stream.write_float(self.b)
        stream.write_float(self.a)
    
    def __repr__(self):
        return "Color(" + \
            repr(self.r) + \
            ", " + \
            repr(self.g) + \
            ", " + \
            repr(self.b) + \
            ", " + \
            repr(self.a) + \
            ")"