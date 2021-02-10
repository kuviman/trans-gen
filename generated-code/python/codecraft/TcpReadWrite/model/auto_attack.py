from model.entity_type import EntityType

class AutoAttack:
    """Auto attack options"""

    def __init__(self, pathfind_range, valid_targets):
        self.pathfind_range = pathfind_range
        """Maximum distance to pathfind"""
        self.valid_targets = valid_targets
        """List of target entity types to try to attack. If empty, all types but resource are considered"""

    @staticmethod
    def read_from(stream):
        """Read AutoAttack from input stream
        """
        pathfind_range = stream.read_int()
        valid_targets = []
        for _ in range(stream.read_int()):
            valid_targets_element = EntityType(stream.read_int())
            valid_targets.append(valid_targets_element)
        return AutoAttack(pathfind_range, valid_targets)
    
    def write_to(self, stream):
        """Write AutoAttack to output stream
        """
        stream.write_int(self.pathfind_range)
        stream.write_int(len(self.valid_targets))
        for element in self.valid_targets:
            stream.write_int(element)
    
    def __repr__(self):
        return "AutoAttack(" + \
            repr(self.pathfind_range) + \
            ", " + \
            repr(self.valid_targets) + \
            ")"