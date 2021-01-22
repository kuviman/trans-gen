namespace TransGenTest.Model
{
    public struct Entity
    {
        public int Id { get; set; }
        public int? PlayerId { get; set; }
        public Model.EntityType EntityType { get; set; }
        public Model.Vec2Int Position { get; set; }
        public int Health { get; set; }
        public bool Active { get; set; }
    
        public Entity(int id, int? playerId, Model.EntityType entityType, Model.Vec2Int position, int health, bool active)
        {
            this.Id = id;
            this.PlayerId = playerId;
            this.EntityType = entityType;
            this.Position = position;
            this.Health = health;
            this.Active = active;
        }
    
        public static Entity ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new Entity();
            result.Id = reader.ReadInt32();
            if (reader.ReadBoolean())
            {
                result.PlayerId = reader.ReadInt32();
            } else
            {
                result.PlayerId = null;
            }
            result.EntityType = EntityTypeHelper.ReadFrom(reader);
            result.Position = Model.Vec2Int.ReadFrom(reader);
            result.Health = reader.ReadInt32();
            result.Active = reader.ReadBoolean();
            return result;
        }
    
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(Id);
            if (!PlayerId.HasValue)
            {
                writer.Write(false);
            } else
            {
                writer.Write(true);
                writer.Write(PlayerId.Value);
            }
            writer.Write((int) (EntityType));
            Position.WriteTo(writer);
            writer.Write(Health);
            writer.Write(Active);
        }
    
        public override string ToString() {
            string stringResult = "Entity { ";
            stringResult += "Id: ";
            stringResult += Id.ToString();
            stringResult += ", ";
            stringResult += "PlayerId: ";
            if (!PlayerId.HasValue)
            {
                stringResult += "null";
            } else
            {
                stringResult += PlayerId.Value.ToString();
            }
            stringResult += ", ";
            stringResult += "EntityType: ";
            stringResult += EntityType.ToString();
            stringResult += ", ";
            stringResult += "Position: ";
            stringResult += Position.ToString();
            stringResult += ", ";
            stringResult += "Health: ";
            stringResult += Health.ToString();
            stringResult += ", ";
            stringResult += "Active: ";
            stringResult += Active.ToString();
            stringResult += " }";
            return stringResult;
        }
    }
}