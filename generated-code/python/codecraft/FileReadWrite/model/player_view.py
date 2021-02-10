from model.entity import Entity
from model.entity_properties import EntityProperties
from model.entity_type import EntityType
from model.player import Player

class PlayerView:
    """Information available to the player"""

    def __init__(self, my_id, map_size, fog_of_war, entity_properties, max_tick_count, max_pathfind_nodes, current_tick, players, entities):
        self.my_id = my_id
        """Your player's ID"""
        self.map_size = map_size
        """Size of the map"""
        self.fog_of_war = fog_of_war
        """Whether fog of war is enabled"""
        self.entity_properties = entity_properties
        """Entity properties for each entity type"""
        self.max_tick_count = max_tick_count
        """Max tick count for the game"""
        self.max_pathfind_nodes = max_pathfind_nodes
        """Max pathfind nodes when performing pathfinding in the game simulator"""
        self.current_tick = current_tick
        """Current tick"""
        self.players = players
        """List of players"""
        self.entities = entities
        """List of entities"""

    @staticmethod
    def read_from(stream):
        """Read PlayerView from input stream
        """
        my_id = stream.read_int()
        map_size = stream.read_int()
        fog_of_war = stream.read_bool()
        entity_properties = {}
        for _ in range(stream.read_int()):
            entity_properties_key = EntityType(stream.read_int())
            entity_properties_value = EntityProperties.read_from(stream)
            entity_properties[entity_properties_key] = entity_properties_value
        max_tick_count = stream.read_int()
        max_pathfind_nodes = stream.read_int()
        current_tick = stream.read_int()
        players = []
        for _ in range(stream.read_int()):
            players_element = Player.read_from(stream)
            players.append(players_element)
        entities = []
        for _ in range(stream.read_int()):
            entities_element = Entity.read_from(stream)
            entities.append(entities_element)
        return PlayerView(my_id, map_size, fog_of_war, entity_properties, max_tick_count, max_pathfind_nodes, current_tick, players, entities)

    def write_to(self, stream):
        """Write PlayerView to output stream
        """
        stream.write_int(self.my_id)
        stream.write_int(self.map_size)
        stream.write_bool(self.fog_of_war)
        stream.write_int(len(self.entity_properties))
        for key, value in self.entity_properties.items():
            stream.write_int(key)
            value.write_to(stream)
        stream.write_int(self.max_tick_count)
        stream.write_int(self.max_pathfind_nodes)
        stream.write_int(self.current_tick)
        stream.write_int(len(self.players))
        for element in self.players:
            element.write_to(stream)
        stream.write_int(len(self.entities))
        for element in self.entities:
            element.write_to(stream)

    def __repr__(self):
        return "PlayerView(" + \
            repr(self.my_id) + \
            ", " + \
            repr(self.map_size) + \
            ", " + \
            repr(self.fog_of_war) + \
            ", " + \
            repr(self.entity_properties) + \
            ", " + \
            repr(self.max_tick_count) + \
            ", " + \
            repr(self.max_pathfind_nodes) + \
            ", " + \
            repr(self.current_tick) + \
            ", " + \
            repr(self.players) + \
            ", " + \
            repr(self.entities) + \
            ")"