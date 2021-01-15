from .one_of import OneOf
from .one_of import OneOf
from .enumeration import Enumeration
class Structure:
    def __init__(self, one_of_one, one_of_two, hash_map, text, real_number):
        self.one_of_one = one_of_one
        self.one_of_two = one_of_two
        self.hash_map = hash_map
        self.text = text
        self.real_number = real_number
    @staticmethod
    def read_from(stream):
        one_of_one = OneOf.read_from(stream)
        one_of_two = OneOf.read_from(stream)
        hash_map = {}
        for _ in range(stream.read_int()):
            hash_map_key = Enumeration(stream.read_int())
            hash_map_value = stream.read_int()
            hash_map[hash_map_key] = hash_map_value
        text = stream.read_string()
        real_number = stream.read_double()
        return Structure(one_of_one, one_of_two, hash_map, text, real_number)
    def write_to(self, stream):
        self.one_of_one.write_to(stream)
        self.one_of_two.write_to(stream)
        stream.write_int(len(self.hash_map))
        for key, value in self.hash_map.items():
            stream.write_int(key)
            stream.write_int(value)
        stream.write_string(self.text)
        stream.write_double(self.real_number)
    def __repr__(self):
        return "Structure(" + \
            repr(self.one_of_one) + "," + \
            repr(self.one_of_two) + "," + \
            repr(self.hash_map) + "," + \
            repr(self.text) + "," + \
            repr(self.real_number) + \
            ")"
