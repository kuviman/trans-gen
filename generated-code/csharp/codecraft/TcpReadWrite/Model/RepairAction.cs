namespace TransGenTest.Model
{
    /// <summary>
    /// Repair action
    /// </summary>
    public struct RepairAction
    {
        /// <summary>
        /// Target entity's ID
        /// </summary>
        public int Target { get; set; }
    
        public RepairAction(int target)
        {
            this.Target = target;
        }
    
        /// <summary> Read RepairAction from reader </summary>
        public static RepairAction ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new RepairAction();
            result.Target = reader.ReadInt32();
            return result;
        }
    
        /// <summary> Write RepairAction to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(Target);
        }
    
        /// <summary> Get string representation of RepairAction </summary>
        public override string ToString() {
            string stringResult = "RepairAction { ";
            stringResult += "Target: ";
            stringResult += Target.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}