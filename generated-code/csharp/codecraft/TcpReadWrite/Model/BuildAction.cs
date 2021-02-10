namespace TransGenTest.Model
{
    /// <summary>
    /// Build action
    /// </summary>
    public struct BuildAction
    {
        /// <summary>
        /// Type of an entity to build
        /// </summary>
        public Model.EntityType EntityType { get; set; }
        /// <summary>
        /// Desired position of new entity
        /// </summary>
        public Vec2Int Position { get; set; }
    
        public BuildAction(Model.EntityType entityType, Vec2Int position)
        {
            this.EntityType = entityType;
            this.Position = position;
        }
    
        /// <summary> Read BuildAction from reader </summary>
        public static BuildAction ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new BuildAction();
            result.EntityType = EntityTypeHelper.ReadFrom(reader);
            result.Position = Vec2Int.ReadFrom(reader);
            return result;
        }
    
        /// <summary> Write BuildAction to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write((int) (EntityType));
            Position.WriteTo(writer);
        }
    
        /// <summary> Get string representation of BuildAction </summary>
        public override string ToString() {
            string stringResult = "BuildAction { ";
            stringResult += "EntityType: ";
            stringResult += EntityType.ToString();
            stringResult += ", ";
            stringResult += "Position: ";
            stringResult += Position.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}