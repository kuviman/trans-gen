class Vec2Int:
    """2 dimensional vector."""

    def __init__(self, x, y):
        self.x = x
        """`x` coordinate of the vector"""
        self.y = y
        """`y` coordinate of the vector"""

    @staticmethod
    def read_from(stream):
        """Read Vec2Int from input stream
        """
        x = stream.read_int()
        y = stream.read_int()
        return Vec2Int(x, y)

    def write_to(self, stream):
        """Write Vec2Int to output stream
        """
        stream.write_int(self.x)
        stream.write_int(self.y)

    def __repr__(self):
        return "Vec2Int(" + \
            repr(self.x) + \
            ", " + \
            repr(self.y) + \
            ")"