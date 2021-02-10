class Structure:
    """Example structure"""

    def __init__(self, text, float_number, double_number):
        self.text = text
        """Text"""
        self.float_number = float_number
        """32-bit float"""
        self.double_number = double_number
        """64-bit float"""

    @staticmethod
    def read_from(stream):
        """Read Structure from input stream
        """
        text = stream.read_string()
        float_number = stream.read_float()
        double_number = stream.read_double()
        return Structure(text, float_number, double_number)
    
    def write_to(self, stream):
        """Write Structure to output stream
        """
        stream.write_string(self.text)
        stream.write_float(self.float_number)
        stream.write_double(self.double_number)
    
    def __repr__(self):
        return "Structure(" + \
            repr(self.text) + \
            ", " + \
            repr(self.float_number) + \
            ", " + \
            repr(self.double_number) + \
            ")"