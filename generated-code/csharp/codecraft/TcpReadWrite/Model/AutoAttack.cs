namespace TransGenTest.Model
{
    /// <summary>
    /// Auto attack options
    /// </summary>
    public struct AutoAttack
    {
        /// <summary>
        /// Maximum distance to pathfind
        /// </summary>
        public int PathfindRange { get; set; }
        /// <summary>
        /// List of target entity types to try to attack. If empty, all types but resource are considered
        /// </summary>
        public Model.EntityType[] ValidTargets { get; set; }
    
        public AutoAttack(int pathfindRange, Model.EntityType[] validTargets)
        {
            this.PathfindRange = pathfindRange;
            this.ValidTargets = validTargets;
        }
    
        /// <summary> Read AutoAttack from reader </summary>
        public static AutoAttack ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new AutoAttack();
            result.PathfindRange = reader.ReadInt32();
            result.ValidTargets = new Model.EntityType[reader.ReadInt32()];
            for (int validTargetsIndex = 0; validTargetsIndex < result.ValidTargets.Length; validTargetsIndex++)
            {
                result.ValidTargets[validTargetsIndex] = EntityTypeHelper.ReadFrom(reader);
            }
            return result;
        }
    
        /// <summary> Write AutoAttack to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(PathfindRange);
            writer.Write(ValidTargets.Length);
            foreach (var validTargetsElement in ValidTargets)
            {
                writer.Write((int) (validTargetsElement));
            }
        }
    
        /// <summary> Get string representation of AutoAttack </summary>
        public override string ToString() {
            string stringResult = "AutoAttack { ";
            stringResult += "PathfindRange: ";
            stringResult += PathfindRange.ToString();
            stringResult += ", ";
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
            stringResult += " }";
            return stringResult;
        }
    }
}