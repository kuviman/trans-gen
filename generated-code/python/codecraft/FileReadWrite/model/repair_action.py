class RepairAction:
    """Repair action"""

    def __init__(self, target):
        self.target = target
        """Target entity's ID"""

    @staticmethod
    def read_from(stream):
        """Read RepairAction from input stream
        """
        target = stream.read_int()
        return RepairAction(target)

    def write_to(self, stream):
        """Write RepairAction to output stream
        """
        stream.write_int(self.target)

    def __repr__(self):
        return "RepairAction(" + \
            repr(self.target) + \
            ")"