namespace TransGenTest.Model
{
    public enum EntityType
    {
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

    public static class EntityTypeHelper {
        public static EntityType ReadFrom(System.IO.BinaryReader reader) {
            switch (reader.ReadInt32())
            {
                case 0:
                    return EntityType.Wall;
                case 1:
                    return EntityType.House;
                case 2:
                    return EntityType.BuilderBase;
                case 3:
                    return EntityType.BuilderUnit;
                case 4:
                    return EntityType.MeleeBase;
                case 5:
                    return EntityType.MeleeUnit;
                case 6:
                    return EntityType.RangedBase;
                case 7:
                    return EntityType.RangedUnit;
                case 8:
                    return EntityType.Resource;
                case 9:
                    return EntityType.Turret;
                default:
                    throw new System.Exception("Unexpected tag value");
            }
        }
    }
}