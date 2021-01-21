import stream;

enum EntityType : int {
    Wall = 0,
    House = 1,
    BuilderBase = 2,
    BuilderUnit = 3,
    MeleeBase = 4,
    MeleeUnit = 5,
    RangedBase = 6,
    RangedUnit = 7,
    Resource = 8,
    Turret = 9,
}

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