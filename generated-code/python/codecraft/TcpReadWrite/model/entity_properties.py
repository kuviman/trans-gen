from model.attack_properties import AttackProperties
from model.build_properties import BuildProperties
from model.repair_properties import RepairProperties

class EntityProperties:
    """Entity properties"""

    def __init__(self, size, build_score, destroy_score, can_move, population_provide, population_use, max_health, initial_cost, sight_range, resource_per_health, build, attack, repair):
        self.size = size
        """Size. Entity has a form of a square with side of this length"""
        self.build_score = build_score
        """Score for building this entity"""
        self.destroy_score = destroy_score
        """Score for destroying this entity"""
        self.can_move = can_move
        """Whether this entity can move"""
        self.population_provide = population_provide
        """Number of population points this entity provides, if active"""
        self.population_use = population_use
        """Number of population points this entity uses"""
        self.max_health = max_health
        """Maximum health points"""
        self.initial_cost = initial_cost
        """Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type"""
        self.sight_range = sight_range
        """If fog of war is enabled, maximum distance at which other entities are considered visible"""
        self.resource_per_health = resource_per_health
        """Amount of resource added to enemy able to collect resource on dealing damage for 1 health point"""
        self.build = build
        """Build properties, if entity can build"""
        self.attack = attack
        """Attack properties, if entity can attack"""
        self.repair = repair
        """Repair properties, if entity can repair"""

    @staticmethod
    def read_from(stream):
        """Read EntityProperties from input stream
        """
        size = stream.read_int()
        build_score = stream.read_int()
        destroy_score = stream.read_int()
        can_move = stream.read_bool()
        population_provide = stream.read_int()
        population_use = stream.read_int()
        max_health = stream.read_int()
        initial_cost = stream.read_int()
        sight_range = stream.read_int()
        resource_per_health = stream.read_int()
        if stream.read_bool():
            build = BuildProperties.read_from(stream)
        else:
            build = None
        if stream.read_bool():
            attack = AttackProperties.read_from(stream)
        else:
            attack = None
        if stream.read_bool():
            repair = RepairProperties.read_from(stream)
        else:
            repair = None
        return EntityProperties(size, build_score, destroy_score, can_move, population_provide, population_use, max_health, initial_cost, sight_range, resource_per_health, build, attack, repair)
    
    def write_to(self, stream):
        """Write EntityProperties to output stream
        """
        stream.write_int(self.size)
        stream.write_int(self.build_score)
        stream.write_int(self.destroy_score)
        stream.write_bool(self.can_move)
        stream.write_int(self.population_provide)
        stream.write_int(self.population_use)
        stream.write_int(self.max_health)
        stream.write_int(self.initial_cost)
        stream.write_int(self.sight_range)
        stream.write_int(self.resource_per_health)
        if self.build is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.build.write_to(stream)
        if self.attack is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.attack.write_to(stream)
        if self.repair is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            self.repair.write_to(stream)
    
    def __repr__(self):
        return "EntityProperties(" + \
            repr(self.size) + \
            ", " + \
            repr(self.build_score) + \
            ", " + \
            repr(self.destroy_score) + \
            ", " + \
            repr(self.can_move) + \
            ", " + \
            repr(self.population_provide) + \
            ", " + \
            repr(self.population_use) + \
            ", " + \
            repr(self.max_health) + \
            ", " + \
            repr(self.initial_cost) + \
            ", " + \
            repr(self.sight_range) + \
            ", " + \
            repr(self.resource_per_health) + \
            ", " + \
            repr(self.build) + \
            ", " + \
            repr(self.attack) + \
            ", " + \
            repr(self.repair) + \
            ")"