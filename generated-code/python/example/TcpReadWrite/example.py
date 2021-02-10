from enumeration import Enumeration
from one_of import OneOf
from structure import Structure

class Example:
    """Example"""

    def __init__(self, one_of, hash_map, optional_int, optional_bool, optional_one_of, optional_struct, optional_enum):
        self.one_of = one_of
        """OneOf"""
        self.hash_map = hash_map
        """Dictionary"""
        self.optional_int = optional_int
        """Optional int"""
        self.optional_bool = optional_bool
        """Optional boolean"""
        self.optional_one_of = optional_one_of
        """Optional OneOf"""
        self.optional_struct = optional_struct
        """Optional struct"""
        self.optional_enum = optional_enum
        """Optional enum"""

    @staticmethod
    def read_from(stream):
        """Read Example from input stream
        """
        one_of = OneOf.read_from(stream)
        hash_map = {}
        for _ in range(stream.read_int()):
            hash_map_key = Enumeration(stream.read_int())
            hash_map_value = stream.read_int()
            hash_map[hash_map_key] = hash_map_value
        if stream.read_bool():
            optional_int = stream.read_int()
        else:
            optional_int = None
        if stream.read_bool():
            optional_bool = stream.read_bool()
        else:
            optional_bool = None
        if stream.read_bool():
            optional_one_of = OneOf.read_from(stream)
        else:
            optional_one_of = None
        if stream.read_bool():
            optional_struct = Structure.read_from(stream)
        else:
            optional_struct = None
        if stream.read_bool():
            optional_enum = Enumeration(stream.read_int())
        else:
            optional_enum = None
        return Example(one_of, hash_map, optional_int, optional_bool, optional_one_of, optional_struct, optional_enum)

    def write_to(self, stream):
        """Write Example to output stream
        """
        self.one_of.write_to(stream)
        stream.write_int(len(self.hash_map))
        for key, value in self.hash_map.items():
            stream.write_int(key)
            stream.write_int(value)
        if self.optional_int is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            stream.write_int(self.optional_int)
        if self.optional_bool is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            stream.write_bool(self.optional_bool)
        if self.optional_one_of is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.optional_one_of.write_to(stream)
        if self.optional_struct is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.optional_struct.write_to(stream)
        if self.optional_enum is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            stream.write_int(self.optional_enum)

    def __repr__(self):
        return "Example(" + \
            repr(self.one_of) + \
            ", " + \
            repr(self.hash_map) + \
            ", " + \
            repr(self.optional_int) + \
            ", " + \
            repr(self.optional_bool) + \
            ", " + \
            repr(self.optional_one_of) + \
            ", " + \
            repr(self.optional_struct) + \
            ", " + \
            repr(self.optional_enum) + \
            ")"