namespace TransGenTest.Model
{
    /// <summary>
    /// Entity properties
    /// </summary>
    public struct EntityProperties
    {
        /// <summary>
        /// Size. Entity has a form of a square with side of this length
        /// </summary>
        public int Size { get; set; }
        /// <summary>
        /// Score for building this entity
        /// </summary>
        public int BuildScore { get; set; }
        /// <summary>
        /// Score for destroying this entity
        /// </summary>
        public int DestroyScore { get; set; }
        /// <summary>
        /// Whether this entity can move
        /// </summary>
        public bool CanMove { get; set; }
        /// <summary>
        /// Number of population points this entity provides, if active
        /// </summary>
        public int PopulationProvide { get; set; }
        /// <summary>
        /// Number of population points this entity uses
        /// </summary>
        public int PopulationUse { get; set; }
        /// <summary>
        /// Maximum health points
        /// </summary>
        public int MaxHealth { get; set; }
        /// <summary>
        /// Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
        /// </summary>
        public int InitialCost { get; set; }
        /// <summary>
        /// If fog of war is enabled, maximum distance at which other entities are considered visible
        /// </summary>
        public int SightRange { get; set; }
        /// <summary>
        /// Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
        /// </summary>
        public int ResourcePerHealth { get; set; }
        /// <summary>
        /// Build properties, if entity can build
        /// </summary>
        public Model.BuildProperties? Build { get; set; }
        /// <summary>
        /// Attack properties, if entity can attack
        /// </summary>
        public Model.AttackProperties? Attack { get; set; }
        /// <summary>
        /// Repair properties, if entity can repair
        /// </summary>
        public Model.RepairProperties? Repair { get; set; }
    
        public EntityProperties(int size, int buildScore, int destroyScore, bool canMove, int populationProvide, int populationUse, int maxHealth, int initialCost, int sightRange, int resourcePerHealth, Model.BuildProperties? build, Model.AttackProperties? attack, Model.RepairProperties? repair)
        {
            this.Size = size;
            this.BuildScore = buildScore;
            this.DestroyScore = destroyScore;
            this.CanMove = canMove;
            this.PopulationProvide = populationProvide;
            this.PopulationUse = populationUse;
            this.MaxHealth = maxHealth;
            this.InitialCost = initialCost;
            this.SightRange = sightRange;
            this.ResourcePerHealth = resourcePerHealth;
            this.Build = build;
            this.Attack = attack;
            this.Repair = repair;
        }
    
        /// <summary> Read EntityProperties from reader </summary>
        public static EntityProperties ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new EntityProperties();
            result.Size = reader.ReadInt32();
            result.BuildScore = reader.ReadInt32();
            result.DestroyScore = reader.ReadInt32();
            result.CanMove = reader.ReadBoolean();
            result.PopulationProvide = reader.ReadInt32();
            result.PopulationUse = reader.ReadInt32();
            result.MaxHealth = reader.ReadInt32();
            result.InitialCost = reader.ReadInt32();
            result.SightRange = reader.ReadInt32();
            result.ResourcePerHealth = reader.ReadInt32();
            if (reader.ReadBoolean())
            {
                result.Build = Model.BuildProperties.ReadFrom(reader);
            } else
            {
                result.Build = null;
            }
            if (reader.ReadBoolean())
            {
                result.Attack = Model.AttackProperties.ReadFrom(reader);
            } else
            {
                result.Attack = null;
            }
            if (reader.ReadBoolean())
            {
                result.Repair = Model.RepairProperties.ReadFrom(reader);
            } else
            {
                result.Repair = null;
            }
            return result;
        }
    
        /// <summary> Write EntityProperties to writer </summary>
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(Size);
            writer.Write(BuildScore);
            writer.Write(DestroyScore);
            writer.Write(CanMove);
            writer.Write(PopulationProvide);
            writer.Write(PopulationUse);
            writer.Write(MaxHealth);
            writer.Write(InitialCost);
            writer.Write(SightRange);
            writer.Write(ResourcePerHealth);
            if (!Build.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                Build.Value.WriteTo(writer);
            }
            if (!Attack.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                Attack.Value.WriteTo(writer);
            }
            if (!Repair.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                Repair.Value.WriteTo(writer);
            }
        }
    
        /// <summary> Get string representation of EntityProperties </summary>
        public override string ToString() {
            string stringResult = "EntityProperties { ";
            stringResult += "Size: ";
            stringResult += Size.ToString();
            stringResult += ", ";
            stringResult += "BuildScore: ";
            stringResult += BuildScore.ToString();
            stringResult += ", ";
            stringResult += "DestroyScore: ";
            stringResult += DestroyScore.ToString();
            stringResult += ", ";
            stringResult += "CanMove: ";
            stringResult += CanMove.ToString();
            stringResult += ", ";
            stringResult += "PopulationProvide: ";
            stringResult += PopulationProvide.ToString();
            stringResult += ", ";
            stringResult += "PopulationUse: ";
            stringResult += PopulationUse.ToString();
            stringResult += ", ";
            stringResult += "MaxHealth: ";
            stringResult += MaxHealth.ToString();
            stringResult += ", ";
            stringResult += "InitialCost: ";
            stringResult += InitialCost.ToString();
            stringResult += ", ";
            stringResult += "SightRange: ";
            stringResult += SightRange.ToString();
            stringResult += ", ";
            stringResult += "ResourcePerHealth: ";
            stringResult += ResourcePerHealth.ToString();
            stringResult += ", ";
            stringResult += "Build: ";
            if (!Build.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += Build.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "Attack: ";
            if (!Attack.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += Attack.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "Repair: ";
            if (!Repair.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += Repair.Value.ToString();
            }
            stringResult += " }";
            return stringResult;
        }
    }
}