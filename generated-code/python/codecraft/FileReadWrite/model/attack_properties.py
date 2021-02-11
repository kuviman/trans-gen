from stream_wrapper import StreamWrapper

class AttackProperties:
    """Entity's attack properties"""

    __slots__ = ("attack_range","damage","collect_resource",)

    attack_range: int
    damage: int
    collect_resource: bool

    def __init__(self, attack_range: int, damage: int, collect_resource: bool):
        self.attack_range = attack_range
        """Maximum attack range"""
        self.damage = damage
        """Damage dealt in one tick"""
        self.collect_resource = collect_resource
        """If true, dealing damage will collect resource from target"""

    @staticmethod
    def read_from(stream: StreamWrapper) -> "AttackProperties":
        """Read AttackProperties from input stream
        """
        attack_range = stream.read_int()
        damage = stream.read_int()
        collect_resource = stream.read_bool()
        return AttackProperties(attack_range, damage, collect_resource)
    
    def write_to(self, stream: StreamWrapper):
        """Write AttackProperties to output stream
        """
        stream.write_int(self.attack_range)
        stream.write_int(self.damage)
        stream.write_bool(self.collect_resource)
    
    def __repr__(self):
        return "AttackProperties(" + \
            repr(self.attack_range) + \
            ", " + \
            repr(self.damage) + \
            ", " + \
            repr(self.collect_resource) + \
            ")"