from enum import IntEnum

class PrimitiveType(IntEnum):
    """Primitive type for debug rendering"""

    LINES = 0
    """Lines, number of vertices should be divisible by 2"""
    TRIANGLES = 1
    """Triangles, number of vertices should be divisible by 3"""

    def __repr__(self):
        return str(self)