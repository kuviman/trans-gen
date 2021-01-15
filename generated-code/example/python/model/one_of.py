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
    def __init__(self, value):
        self.value = value
    @staticmethod
    def read_from(stream):
        value = []
        for _ in range(stream.read_int()):
            value_element = stream.read_int()
            value.append(value_element)
        return OptionOne(value)
    def write_to(self, stream):
        stream.write_int(self.TAG)
        stream.write_int(len(self.value))
        for element in self.value:
            stream.write_int(element)
    def __repr__(self):
        return "OptionOne(" + \
            repr(self.value) + \
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
