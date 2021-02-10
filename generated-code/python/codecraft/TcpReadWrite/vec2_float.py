class Vec2Float:
    """2 dimensional vector."""

    def __init__(self, x, y):
        self.x = x
        """`x` coordinate of the vector"""
        self.y = y
        """`y` coordinate of the vector"""

    @staticmethod
    def read_from(stream):
        """Read Vec2Float from input stream
        """
        x = stream.read_float()
        y = stream.read_float()
        return Vec2Float(x, y)
    
    def write_to(self, stream):
        """Write Vec2Float to output stream
        """
        stream.write_float(self.x)
        stream.write_float(self.y)
    
    def __repr__(self):
        return "Vec2Float(" + \
            repr(self.x) + \
            ", " + \
            repr(self.y) + \
            ")"