from enum import IntEnum

class Enumeration(IntEnum):
    """Example enumeration"""

    VALUE_ONE = 0
    """First option"""
    VALUE_TWO = 1
    """Second option"""

    def __repr__(self):
        return str(self)