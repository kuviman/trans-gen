module Model

# Entity type
module EntityType
    # Wall, can be used to prevent enemy from moving through
    WALL = 0
    # House, used to increase population
    HOUSE = 1
    # Base for recruiting new builder units
    BUILDER_BASE = 2
    # Builder unit can build buildings
    BUILDER_UNIT = 3
    # Base for recruiting new melee units
    MELEE_BASE = 4
    # Melee unit
    MELEE_UNIT = 5
    # Base for recruiting new ranged units
    RANGED_BASE = 6
    # Ranged unit
    RANGED_UNIT = 7
    # Resource can be harvested
    RESOURCE = 8
    # Ranged attacking building
    TURRET = 9

    # Read EntityType from input stream
    def self.read_from(stream)
        result = stream.read_int()
        if result < 0 || result >= 10
            raise "Unexpected tag value"
        end
        result
    end

    def self.to_s(value)
        if value == WALL
            return "WALL"
        end
        if value == HOUSE
            return "HOUSE"
        end
        if value == BUILDER_BASE
            return "BUILDER_BASE"
        end
        if value == BUILDER_UNIT
            return "BUILDER_UNIT"
        end
        if value == MELEE_BASE
            return "MELEE_BASE"
        end
        if value == MELEE_UNIT
            return "MELEE_UNIT"
        end
        if value == RANGED_BASE
            return "RANGED_BASE"
        end
        if value == RANGED_UNIT
            return "RANGED_UNIT"
        end
        if value == RESOURCE
            return "RESOURCE"
        end
        if value == TURRET
            return "TURRET"
        end
        raise "Impossible happened"
    end

    def self.to_str(value)
        self.to_s(value)
    end
end

end