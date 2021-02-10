namespace TransGenTest.Model
{
    /// <summary>
    /// Move action
    /// </summary>
    public struct MoveAction
    {
        /// <summary>
        /// Target position
        /// </summary>
        public Vec2Int Target { get; set; }
        /// <summary>
        /// Whether to try find closest position, if path to target is not found
        /// </summary>
        public bool FindClosestPosition { get; set; }
        /// <summary>
        /// Whether to destroy other entities on the way
        /// </summary>
        public bool BreakThrough { get; set; }
    
        public MoveAction(Vec2Int target, bool findClosestPosition, bool breakThrough)
        {
            this.Target = target;
            this.FindClosestPosition = findClosestPosition;
            this.BreakThrough = breakThrough;
        }
    
        /// <summary> Read MoveAction from reader </summary>
        public static MoveAction ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new MoveAction();
            result.Target = Vec2Int.ReadFrom(reader);
            result.FindClosestPosition = reader.ReadBoolean();
            result.BreakThrough = reader.ReadBoolean();
            return result;
        }
    
        /// <summary> Write MoveAction to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            Target.WriteTo(writer);
            writer.Write(FindClosestPosition);
            writer.Write(BreakThrough);
        }
    
        /// <summary> Get string representation of MoveAction </summary>
        public override string ToString() {
            string stringResult = "MoveAction { ";
            stringResult += "Target: ";
            stringResult += Target.ToString();
            stringResult += ", ";
            stringResult += "FindClosestPosition: ";
            stringResult += FindClosestPosition.ToString();
            stringResult += ", ";
            stringResult += "BreakThrough: ";
            stringResult += BreakThrough.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}