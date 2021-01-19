class OneOf:
    @staticmethod
    def read_from(stream):
        tag = stream.read_int()
        if tag == OptionOne.TAG:
            return OneOf.OptionOne.read_from(stream)
        if tag == OptionTwo.TAG:
            return OneOf.OptionTwo.read_from(stream)
        raise Exception("Unexpected tag value")

class OptionOne(OneOf):
    TAG = 0

    def __init__(self, vec_int, long_int):
        self.vec_int = vec_int
        self.long_int = long_int

    @staticmethod
    def read_from(stream):
        vec_int = []
        for _ in range(stream.read_int()):
            vec_int_element = stream.read_int()
            vec_int.append(vec_int_element)
        long_int = stream.read_long()
        return OptionOne(vec_int, long_int)

    def write_to(self, stream):
        stream.write_int(self.TAG)
        stream.write_int(len(self.vec_int))
        for element in self.vec_int:
            stream.write_int(element)
        stream.write_long(self.long_int)

    def __repr__(self):
        return "OptionOne(" + \
            repr(self.vec_int) + \
            ", " + \
            repr(self.long_int) + \
            ")"

OneOf.OptionOne = OptionOne

class OptionTwo(OneOf):
    TAG = 1

    def __init__(self, value):
        self.value = value

    @staticmethod
    def read_from(stream):
        value = stream.read_int()
        return OptionTwo(value)

    def write_to(self, stream):
        stream.write_int(self.TAG)
        stream.write_int(self.value)

    def __repr__(self):
        return "OptionTwo(" + \
            repr(self.value) + \
            ")"

OneOf.OptionTwo = OptionTwo