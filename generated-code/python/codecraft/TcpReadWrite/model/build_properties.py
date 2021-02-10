from model.entity_type import EntityType

class BuildProperties:
    """Entity's build properties"""

    def __init__(self, options, init_health):
        self.options = options
        """Valid new entity types"""
        self.init_health = init_health
        """Initial health of new entity. If absent, it will have full health"""

    @staticmethod
    def read_from(stream):
        """Read BuildProperties from input stream
        """
        options = []
        for _ in range(stream.read_int()):
            options_element = EntityType(stream.read_int())
            options.append(options_element)
        if stream.read_bool():
            init_health = stream.read_int()
        else:
            init_health = None
        return BuildProperties(options, init_health)

    def write_to(self, stream):
        """Write BuildProperties to output stream
        """
        stream.write_int(len(self.options))
        for element in self.options:
            stream.write_int(element)
        if self.init_health is None:
            stream.write_bool(False)
        else:
            stream.write_bool(True)
            stream.write_int(self.init_health)

    def __repr__(self):
        return "BuildProperties(" + \
            repr(self.options) + \
            ", " + \
            repr(self.init_health) + \
            ")"