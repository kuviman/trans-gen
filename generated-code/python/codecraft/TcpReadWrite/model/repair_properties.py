from model.entity_type import EntityType
from stream_wrapper import StreamWrapper
from typing import List

class RepairProperties:
    """Entity's repair properties"""

    __slots__ = ("valid_targets","power",)

    valid_targets: List[EntityType]
    power: int

    def __init__(self, valid_targets: List[EntityType], power: int):
        self.valid_targets = valid_targets
        """Valid target entity types"""
        self.power = power
        """Health restored in one tick"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "RepairProperties":
        """Read RepairProperties from input stream
        """
        valid_targets = []
        for _ in range(stream.read_int()):
            valid_targets_element = EntityType(stream.read_int())
            valid_targets.append(valid_targets_element)
        power = stream.read_int()
        return RepairProperties(valid_targets, power)
    
    def write_to(self, stream: StreamWrapper):
        """Write RepairProperties to output stream
        """
        stream.write_int(len(self.valid_targets))
        for element in self.valid_targets:
            stream.write_int(element)
        stream.write_int(self.power)
    
    def __repr__(self):
        return "RepairProperties(" + \
            repr(self.valid_targets) + \
            ", " + \
            repr(self.power) + \
            ")"