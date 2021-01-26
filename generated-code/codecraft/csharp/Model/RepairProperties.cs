namespace TransGenTest.Model
{
    /// <summary>
    /// Entity's repair properties
    /// </summary>
    public struct RepairProperties
    {
        /// <summary>
        /// Valid target entity types
        /// </summary>
        public Model.EntityType[] ValidTargets { get; set; }
        /// <summary>
        /// Health restored in one tick
        /// </summary>
        public int Power { get; set; }
    
        public RepairProperties(Model.EntityType[] validTargets, int power)
        {
            this.ValidTargets = validTargets;
            this.Power = power;
        }
    
        /// <summary> Read RepairProperties from reader </summary>
        public static RepairProperties ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new RepairProperties();
            result.ValidTargets = new Model.EntityType[reader.ReadInt32()];
            for (int validTargetsIndex = 0; validTargetsIndex < result.ValidTargets.Length; validTargetsIndex++)
            {
                result.ValidTargets[validTargetsIndex] = EntityTypeHelper.ReadFrom(reader);
            }
            result.Power = reader.ReadInt32();
            return result;
        }
    
        /// <summary> Write RepairProperties to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(ValidTargets.Length);
            foreach (var validTargetsElement in ValidTargets)
            {
                writer.Write((int) (validTargetsElement));
            }
            writer.Write(Power);
        }
    
        /// <summary> Get string representation of RepairProperties </summary>
        public override string ToString() {
            string stringResult = "RepairProperties { ";
            stringResult += "ValidTargets: ";
            stringResult += "[ ";
            int validTargetsIndex = 0;
            foreach (var validTargetsElement in ValidTargets)
            {
                if (validTargetsIndex != 0) {
                    stringResult += ", ";
                }
                stringResult += validTargetsElement.ToString();
                validTargetsIndex++;
            }
            stringResult += " ]";
            stringResult += ", ";
            stringResult += "Power: ";
            stringResult += Power.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}