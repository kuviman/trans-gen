namespace TransGenTest.Model
{
    public struct PlayerView
    {
        public int MyId { get; set; }
        public int MapSize { get; set; }
        public bool FogOfWar { get; set; }
        public System.Collections.Generic.IDictionary<Model.EntityType, Model.EntityProperties> EntityProperties { get; set; }
        public int MaxTickCount { get; set; }
        public int MaxPathfindNodes { get; set; }
        public int CurrentTick { get; set; }
        public Model.Player[] Players { get; set; }
        public Model.Entity[] Entities { get; set; }
    
        public PlayerView(int myId, int mapSize, bool fogOfWar, System.Collections.Generic.IDictionary<Model.EntityType, Model.EntityProperties> entityProperties, int maxTickCount, int maxPathfindNodes, int currentTick, Model.Player[] players, Model.Entity[] entities)
        {
            this.MyId = myId;
            this.MapSize = mapSize;
            this.FogOfWar = fogOfWar;
            this.EntityProperties = entityProperties;
            this.MaxTickCount = maxTickCount;
            this.MaxPathfindNodes = maxPathfindNodes;
            this.CurrentTick = currentTick;
            this.Players = players;
            this.Entities = entities;
        }
    
        public static PlayerView ReadFrom(System.IO.BinaryReader reader)
        {
            var result = new PlayerView();
            result.MyId = reader.ReadInt32();
            result.MapSize = reader.ReadInt32();
            result.FogOfWar = reader.ReadBoolean();
            int entityPropertiesSize = reader.ReadInt32();
            result.EntityProperties = new System.Collections.Generic.Dictionary<Model.EntityType, Model.EntityProperties>(entityPropertiesSize);
            for (int entityPropertiesIndex = 0; entityPropertiesIndex < entityPropertiesSize; entityPropertiesIndex++)
            {
                Model.EntityType entityPropertiesKey;
                Model.EntityProperties entityPropertiesValue;
                switch (reader.ReadInt32())
                {
                    case 0:
                        entityPropertiesKey = Model.EntityType.Wall;
                        break;
                    case 1:
                        entityPropertiesKey = Model.EntityType.House;
                        break;
                    case 2:
                        entityPropertiesKey = Model.EntityType.BuilderBase;
                        break;
                    case 3:
                        entityPropertiesKey = Model.EntityType.BuilderUnit;
                        break;
                    case 4:
                        entityPropertiesKey = Model.EntityType.MeleeBase;
                        break;
                    case 5:
                        entityPropertiesKey = Model.EntityType.MeleeUnit;
                        break;
                    case 6:
                        entityPropertiesKey = Model.EntityType.RangedBase;
                        break;
                    case 7:
                        entityPropertiesKey = Model.EntityType.RangedUnit;
                        break;
                    case 8:
                        entityPropertiesKey = Model.EntityType.Resource;
                        break;
                    case 9:
                        entityPropertiesKey = Model.EntityType.Turret;
                        break;
                    default:
                        throw new System.Exception("Unexpected tag value");
                }
                entityPropertiesValue = Model.EntityProperties.ReadFrom(reader);
                result.EntityProperties.Add(entityPropertiesKey, entityPropertiesValue);
            }
            result.MaxTickCount = reader.ReadInt32();
            result.MaxPathfindNodes = reader.ReadInt32();
            result.CurrentTick = reader.ReadInt32();
            result.Players = new Model.Player[reader.ReadInt32()];
            for (int playersIndex = 0; playersIndex < result.Players.Length; playersIndex++)
            {
                result.Players[playersIndex] = Model.Player.ReadFrom(reader);
            }
            result.Entities = new Model.Entity[reader.ReadInt32()];
            for (int entitiesIndex = 0; entitiesIndex < result.Entities.Length; entitiesIndex++)
            {
                result.Entities[entitiesIndex] = Model.Entity.ReadFrom(reader);
            }
            return result;
        }
    
        public void WriteTo(System.IO.BinaryWriter writer)
        {
            writer.Write(MyId);
            writer.Write(MapSize);
            writer.Write(FogOfWar);
            writer.Write(EntityProperties.Count);
            foreach (var entityPropertiesEntry in EntityProperties)
            {
                var entityPropertiesKey = entityPropertiesEntry.Key;
                var entityPropertiesValue = entityPropertiesEntry.Value;
                writer.Write((int) (entityPropertiesKey));
                entityPropertiesValue.WriteTo(writer);
            }
            writer.Write(MaxTickCount);
            writer.Write(MaxPathfindNodes);
            writer.Write(CurrentTick);
            writer.Write(Players.Length);
            foreach (var playersElement in Players)
            {
                playersElement.WriteTo(writer);
            }
            writer.Write(Entities.Length);
            foreach (var entitiesElement in Entities)
            {
                entitiesElement.WriteTo(writer);
            }
        }
    }
}