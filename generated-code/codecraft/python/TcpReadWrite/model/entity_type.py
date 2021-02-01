from enum import IntEnum

class EntityType(IntEnum):
    """Entity type"""

    WALL = 0
    """Wall, can be used to prevent enemy from moving through"""
    HOUSE = 1
    """House, used to increase population"""
    BUILDER_BASE = 2
    """Base for recruiting new builder units"""
    BUILDER_UNIT = 3
    """Builder unit can build buildings"""
    MELEE_BASE = 4
    """Base for recruiting new melee units"""
    MELEE_UNIT = 5
    """Melee unit"""
    RANGED_BASE = 6
    """Base for recruiting new ranged units"""
    RANGED_UNIT = 7
    """Ranged unit"""
    RESOURCE = 8
    """Resource can be harvested"""
    TURRET = 9
    """Ranged attacking building"""

    def __repr__(self):
        return str(self)