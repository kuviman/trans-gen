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
                result.Options[optionsIndex] = EntityTypeHelper.ReadFrom(reader);
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
    
        public override string ToString() {
            string stringResult = "BuildProperties { ";
            stringResult += "Options: ";
            stringResult += "[ ";
            int optionsIndex = 0;
            foreach (var optionsElement in Options)
            {
                if (optionsIndex != 0) {
                    stringResult += ", ";
                }
                stringResult += optionsElement.ToString();
                optionsIndex++;
            }
            stringResult += " ]";
            stringResult += ", ";
            stringResult += "InitHealth: ";
            if (!InitHealth.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += InitHealth.Value.ToString();
            }
            stringResult += " }";
            return stringResult;
        }
    }
}