import stream;

/// Entity type
enum EntityType : int {
    /// Wall, can be used to prevent enemy from moving through
    Wall = 0,
    /// House, used to increase population
    House = 1,
    /// Base for recruiting new builder units
    BuilderBase = 2,
    /// Builder unit can build buildings
    BuilderUnit = 3,
    /// Base for recruiting new melee units
    MeleeBase = 4,
    /// Melee unit
    MeleeUnit = 5,
    /// Base for recruiting new ranged units
    RangedBase = 6,
    /// Ranged unit
    RangedUnit = 7,
    /// Resource can be harvested
    Resource = 8,
    /// Ranged attacking building
    Turret = 9,
}

/// Read EntityType from reader
EntityType readEntityType(Stream reader) {
    switch (reader.readInt()) {
        case EntityType.Wall:
            return EntityType.Wall;
        case EntityType.House:
            return EntityType.House;
        case EntityType.BuilderBase:
            return EntityType.BuilderBase;
        case EntityType.BuilderUnit:
            return EntityType.BuilderUnit;
        case EntityType.MeleeBase:
            return EntityType.MeleeBase;
        case EntityType.MeleeUnit:
            return EntityType.MeleeUnit;
        case EntityType.RangedBase:
            return EntityType.RangedBase;
        case EntityType.RangedUnit:
            return EntityType.RangedUnit;
        case EntityType.Resource:
            return EntityType.Resource;
        case EntityType.Turret:
            return EntityType.Turret;
        default:
            throw new Exception("Unexpected tag value");
    }
}