from model.entity_type import EntityType
from stream_wrapper import StreamWrapper
from vec2_int import Vec2Int

class BuildAction:
    """Build action"""

    __slots__ = ("entity_type","position",)

    entity_type: EntityType
    position: Vec2Int

    def __init__(self, entity_type: EntityType, position: Vec2Int):
        self.entity_type = entity_type
        """Type of an entity to build"""
        self.position = position
        """Desired position of new entity"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "BuildAction":
        """Read BuildAction from input stream
        """
        entity_type = EntityType(stream.read_int())
        position = Vec2Int.read_from(stream)
        return BuildAction(entity_type, position)
    
    def write_to(self, stream: StreamWrapper):
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