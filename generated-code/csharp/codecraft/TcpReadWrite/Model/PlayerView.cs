namespace TransGenTest.Model
{
    /// <summary>
    /// Information available to the player
    /// </summary>
    public struct PlayerView
    {
        /// <summary>
        /// Your player's ID
        /// </summary>
        public int MyId { get; set; }
        /// <summary>
        /// Size of the map
        /// </summary>
        public int MapSize { get; set; }
        /// <summary>
        /// Whether fog of war is enabled
        /// </summary>
        public bool FogOfWar { get; set; }
        /// <summary>
        /// Entity properties for each entity type
        /// </summary>
        public System.Collections.Generic.IDictionary<Model.EntityType, Model.EntityProperties> EntityProperties { get; set; }
        /// <summary>
        /// Max tick count for the game
        /// </summary>
        public int MaxTickCount { get; set; }
        /// <summary>
        /// Max pathfind nodes when performing pathfinding in the game simulator
        /// </summary>
        public int MaxPathfindNodes { get; set; }
        /// <summary>
        /// Current tick
        /// </summary>
        public int CurrentTick { get; set; }
        /// <summary>
        /// List of players
        /// </summary>
        public Model.Player[] Players { get; set; }
        /// <summary>
        /// List of entities
        /// </summary>
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
    
        /// <summary> Read PlayerView from reader </summary>
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
                entityPropertiesKey = EntityTypeHelper.ReadFrom(reader);
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
    
        /// <summary> Write PlayerView to writer </summary>
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
    
        /// <summary> Get string representation of PlayerView </summary>
        public override string ToString() {
            string stringResult = "PlayerView { ";
            stringResult += "MyId: ";
            stringResult += MyId.ToString();
            stringResult += ", ";
            stringResult += "MapSize: ";
            stringResult += MapSize.ToString();
            stringResult += ", ";
            stringResult += "FogOfWar: ";
            stringResult += FogOfWar.ToString();
            stringResult += ", ";
            stringResult += "EntityProperties: ";
            stringResult += "{ ";
            int entityPropertiesIndex = 0;
            foreach (var entityPropertiesEntry in EntityProperties)
            {
                if (entityPropertiesIndex != 0) {
                    stringResult += ", ";
                }
                var entityPropertiesKey = entityPropertiesEntry.Key;
                stringResult += entityPropertiesKey.ToString();
                stringResult += ": ";
                var entityPropertiesValue = entityPropertiesEntry.Value;
                stringResult += entityPropertiesValue.ToString();
                entityPropertiesIndex++;
            }
            stringResult += " }";
            stringResult += ", ";
            stringResult += "MaxTickCount: ";
            stringResult += MaxTickCount.ToString();
            stringResult += ", ";
            stringResult += "MaxPathfindNodes: ";
            stringResult += MaxPathfindNodes.ToString();
            stringResult += ", ";
            stringResult += "CurrentTick: ";
            stringResult += CurrentTick.ToString();
            stringResult += ", ";
            stringResult += "Players: ";
            stringResult += "[ ";
            int playersIndex = 0;
            foreach (var playersElement in Players)
            {
                if (playersIndex != 0) {
                    stringResult += ", ";
                }
                stringResult += playersElement.ToString();
                playersIndex++;
            }
            stringResult += " ]";
            stringResult += ", ";
            stringResult += "Entities: ";
            stringResult += "[ ";
            int entitiesIndex = 0;
            foreach (var entitiesElement in Entities)
            {
                if (entitiesIndex != 0) {
                    stringResult += ", ";
                }
                stringResult += entitiesElement.ToString();
                entitiesIndex++;
            }
            stringResult += " ]";
            stringResult += " }";
            return stringResult;
        }
    }
}