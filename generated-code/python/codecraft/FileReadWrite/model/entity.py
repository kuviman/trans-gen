from model.entity_type import EntityType
from stream_wrapper import StreamWrapper
from typing import Optional
from vec2_int import Vec2Int

class Entity:
    """Game entity"""

    __slots__ = ("id","player_id","entity_type","position","health","active",)

    id: int
    player_id: Optional[int]
    entity_type: EntityType
    position: Vec2Int
    health: int
    active: bool

    def __init__(self, id: int, player_id: Optional[int], entity_type: EntityType, position: Vec2Int, health: int, active: bool):
        self.id = id
        """Entity's ID. Unique for each entity"""
        self.player_id = player_id
        """Entity's owner player ID, if owned by a player"""
        self.entity_type = entity_type
        """Entity's type"""
        self.position = position
        """Entity's position (corner with minimal coordinates)"""
        self.health = health
        """Current health"""
        self.active = active
        """If entity is active, it can perform actions"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "Entity":
        """Read Entity from input stream
        """
        id = stream.read_int()
        if stream.read_bool():
            player_id = stream.read_int()
        else:
            player_id = None
        entity_type = EntityType(stream.read_int())
        position = Vec2Int.read_from(stream)
        health = stream.read_int()
        active = stream.read_bool()
        return Entity(id, player_id, entity_type, position, health, active)
    
    def write_to(self, stream: StreamWrapper):
        """Write Entity to output stream
        """
        stream.write_int(self.id)
        if self.player_id is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            stream.write_int(self.player_id)
        stream.write_int(self.entity_type)
        self.position.write_to(stream)
        stream.write_int(self.health)
        stream.write_bool(self.active)
    
    def __repr__(self):
        return "Entity(" + \
            repr(self.id) + \
            ", " + \
            repr(self.player_id) + \
            ", " + \
            repr(self.entity_type) + \
            ", " + \
            repr(self.position) + \
            ", " + \
            repr(self.health) + \
            ", " + \
            repr(self.active) + \
            ")"