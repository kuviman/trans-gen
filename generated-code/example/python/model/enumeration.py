from enum import IntEnum

class Enumeration(IntEnum):
    VALUE_ONE = 0
    VALUE_TWO = 1

    def __repr__(self):
        return str(self)