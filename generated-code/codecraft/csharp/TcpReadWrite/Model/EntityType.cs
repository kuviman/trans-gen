namespace TransGenTest.Model
{
    /// <summary>
    /// Entity type
    /// </summary>
    public enum EntityType
    {
        /// <summary>
        /// Wall, can be used to prevent enemy from moving through
        /// </summary>
        Wall = 0,
        /// <summary>
        /// House, used to increase population
        /// </summary>
        House = 1,
        /// <summary>
        /// Base for recruiting new builder units
        /// </summary>
        BuilderBase = 2,
        /// <summary>
        /// Builder unit can build buildings
        /// </summary>
        BuilderUnit = 3,
        /// <summary>
        /// Base for recruiting new melee units
        /// </summary>
        MeleeBase = 4,
        /// <summary>
        /// Melee unit
        /// </summary>
        MeleeUnit = 5,
        /// <summary>
        /// Base for recruiting new ranged units
        /// </summary>
        RangedBase = 6,
        /// <summary>
        /// Ranged unit
        /// </summary>
        RangedUnit = 7,
        /// <summary>
        /// Resource can be harvested
        /// </summary>
        Resource = 8,
        /// <summary>
        /// Ranged attacking building
        /// </summary>
        Turret = 9,
    }

    public static class EntityTypeHelper {
        /// <summary> Read EntityType from reader </summary>
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