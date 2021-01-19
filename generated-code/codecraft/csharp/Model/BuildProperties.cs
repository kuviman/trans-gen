namespace TransGenTest.Model
{
    public struct BuildProperties
    {
        public Model.EntityType[] Options { get; set; }
        public int? InitHealth { get; set; }
    
        public BuildProperties(Model.EntityType[] options, int? initHealth)
        {
            this.Options = options;
            this.InitHealth = initHealth;
        }
    
        public static BuildProperties ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new BuildProperties();
            result.Options = new Model.EntityType[reader.ReadInt32()];
            for (int optionsIndex = 0; optionsIndex < result.Options.Length; optionsIndex++)
            {
                switch (reader.ReadInt32())
                {
                    case 0:
                        result.Options[optionsIndex] = Model.EntityType.Wall;
                        break;
                    case 1:
                        result.Options[optionsIndex] = Model.EntityType.House;
                        break;
                    case 2:
                        result.Options[optionsIndex] = Model.EntityType.BuilderBase;
                        break;
                    case 3:
                        result.Options[optionsIndex] = Model.EntityType.BuilderUnit;
                        break;
                    case 4:
                        result.Options[optionsIndex] = Model.EntityType.MeleeBase;
                        break;
                    case 5:
                        result.Options[optionsIndex] = Model.EntityType.MeleeUnit;
                        break;
                    case 6:
                        result.Options[optionsIndex] = Model.EntityType.RangedBase;
                        break;
                    case 7:
                        result.Options[optionsIndex] = Model.EntityType.RangedUnit;
                        break;
                    case 8:
                        result.Options[optionsIndex] = Model.EntityType.Resource;
                        break;
                    case 9:
                        result.Options[optionsIndex] = Model.EntityType.Turret;
                        break;
                    default:
                        throw new System.Exception("Unexpected tag value");
                }
            }
            if (reader.ReadBoolean())
            {
                result.InitHealth = reader.ReadInt32();
            } else
            {
                result.InitHealth = null;
            }
            return result;
        }
    
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(Options.Length);
            foreach (var optionsElement in Options)
            {
                writer.Write((int) (optionsElement));
            }
            if (!InitHealth.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                writer.Write(InitHealth.Value);
            }
        }
    }
}