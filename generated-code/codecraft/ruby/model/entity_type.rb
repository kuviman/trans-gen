module EntityType
    WALL = 0
    HOUSE = 1
    BUILDER_BASE = 2
    BUILDER_UNIT = 3
    MELEE_BASE = 4
    MELEE_UNIT = 5
    RANGED_BASE = 6
    RANGED_UNIT = 7
    RESOURCE = 8
    TURRET = 9

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