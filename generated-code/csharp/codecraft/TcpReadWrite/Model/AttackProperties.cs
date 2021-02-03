namespace TransGenTest.Model
{
    /// <summary>
    /// Entity's attack properties
    /// </summary>
    public struct AttackProperties
    {
        /// <summary>
        /// Maximum attack range
        /// </summary>
        public int AttackRange { get; set; }
        /// <summary>
        /// Damage dealt in one tick
        /// </summary>
        public int Damage { get; set; }
        /// <summary>
        /// If true, dealing damage will collect resource from target
        /// </summary>
        public bool CollectResource { get; set; }
    
        public AttackProperties(int attackRange, int damage, bool collectResource)
        {
            this.AttackRange = attackRange;
            this.Damage = damage;
            this.CollectResource = collectResource;
        }
    
        /// <summary> Read AttackProperties from reader </summary>
        public static AttackProperties ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new AttackProperties();
            result.AttackRange = reader.ReadInt32();
            result.Damage = reader.ReadInt32();
            result.CollectResource = reader.ReadBoolean();
            return result;
        }
    
        /// <summary> Write AttackProperties to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(AttackRange);
            writer.Write(Damage);
            writer.Write(CollectResource);
        }
    
        /// <summary> Get string representation of AttackProperties </summary>
        public override string ToString() {
            string stringResult = "AttackProperties { ";
            stringResult += "AttackRange: ";
            stringResult += AttackRange.ToString();
            stringResult += ", ";
            stringResult += "Damage: ";
            stringResult += Damage.ToString();
            stringResult += ", ";
            stringResult += "CollectResource: ";
            stringResult += CollectResource.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}