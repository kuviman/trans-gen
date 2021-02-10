namespace TransGenTest.Model
{
    /// <summary>
    /// Attack action
    /// </summary>
    public struct AttackAction
    {
        /// <summary>
        /// If specified, target entity's ID
        /// </summary>
        public int? Target { get; set; }
        /// <summary>
        /// If specified, configures auto attacking
        /// </summary>
        public Model.AutoAttack? AutoAttack { get; set; }
    
        public AttackAction(int? target, Model.AutoAttack? autoAttack)
        {
            this.Target = target;
            this.AutoAttack = autoAttack;
        }
    
        /// <summary> Read AttackAction from reader </summary>
        public static AttackAction ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new AttackAction();
            if (reader.ReadBoolean())
            {
                result.Target = reader.ReadInt32();
            } else
            {
                result.Target = null;
            }
            if (reader.ReadBoolean())
            {
                result.AutoAttack = Model.AutoAttack.ReadFrom(reader);
            } else
            {
                result.AutoAttack = null;
            }
            return result;
        }
    
        /// <summary> Write AttackAction to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            if (!Target.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                writer.Write(Target.Value);
            }
            if (!AutoAttack.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                AutoAttack.Value.WriteTo(writer);
            }
        }
    
        /// <summary> Get string representation of AttackAction </summary>
        public override string ToString() {
            string stringResult = "AttackAction { ";
            stringResult += "Target: ";
            if (!Target.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += Target.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "AutoAttack: ";
            if (!AutoAttack.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += AutoAttack.Value.ToString();
            }
            stringResult += " }";
            return stringResult;
        }
    }
}