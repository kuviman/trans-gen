from model.entity_type import EntityType
from vec2_int import Vec2Int

class BuildAction:
    """Build action"""

    def __init__(self, entity_type, position):
        self.entity_type = entity_type
        """Type of an entity to build"""
        self.position = position
        """Desired position of new entity"""

    @staticmethod
    def read_from(stream):
        """Read BuildAction from input stream
        """
        entity_type = EntityType(stream.read_int())
        position = Vec2Int.read_from(stream)
        return BuildAction(entity_type, position)
    
    def write_to(self, stream):
        """Write BuildAction to output stream
        """
        stream.write_int(self.entity_type)
        self.position.write_to(stream)
    
    def __repr__(self):
        return "BuildAction(" + \
            repr(self.entity_type) + \
            ", " + \
            repr(self.position) + \
            ")"