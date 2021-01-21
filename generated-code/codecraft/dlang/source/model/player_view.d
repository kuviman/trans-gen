import model;
import stream;
import std.conv;
import std.typecons : Nullable;

struct PlayerView {
    int myId;
    int mapSize;
    bool fogOfWar;
    EntityProperties[EntityType] entityProperties;
    int maxTickCount;
    int maxPathfindNodes;
    int currentTick;
    Player[] players;
    Entity[] entities;

    this(int myId, int mapSize, bool fogOfWar, EntityProperties[EntityType] entityProperties, int maxTickCount, int maxPathfindNodes, int currentTick, Player[] players, Entity[] entities) {
        this.myId = myId;
        this.mapSize = mapSize;
        this.fogOfWar = fogOfWar;
        this.entityProperties = entityProperties;
        this.maxTickCount = maxTickCount;
        this.maxPathfindNodes = maxPathfindNodes;
        this.currentTick = currentTick;
        this.players = players;
        this.entities = entities;
    }

    static PlayerView readFrom(Stream reader) {
        int myId;
        myId = reader.readInt();
        int mapSize;
        mapSize = reader.readInt();
        bool fogOfWar;
        fogOfWar = reader.readBool();
        EntityProperties[EntityType] entityProperties;
        int entityPropertiesSize = reader.readInt();
        entityProperties.clear();
        for (int entityPropertiesIndex = 0; entityPropertiesIndex < entityPropertiesSize; entityPropertiesIndex++) {
            EntityType entityPropertiesKey;
            EntityProperties entityPropertiesValue;
            entityPropertiesKey = readEntityType(reader);
            entityPropertiesValue = EntityProperties.readFrom(reader);
            entityProperties[entityPropertiesKey] = entityPropertiesValue;
        }
        int maxTickCount;
        maxTickCount = reader.readInt();
        int maxPathfindNodes;
        maxPathfindNodes = reader.readInt();
        int currentTick;
        currentTick = reader.readInt();
        Player[] players;
        players = new Player[reader.readInt()];
        for (int playersIndex = 0; playersIndex < players.length; playersIndex++) {
            Player playersKey;
            playersKey = Player.readFrom(reader);
            players[playersIndex] = playersKey;
        }
        Entity[] entities;
        entities = new Entity[reader.readInt()];
        for (int entitiesIndex = 0; entitiesIndex < entities.length; entitiesIndex++) {
            Entity entitiesKey;
            entitiesKey = Entity.readFrom(reader);
            entities[entitiesIndex] = entitiesKey;
        }
        return PlayerView(myId, mapSize, fogOfWar, entityProperties, maxTickCount, maxPathfindNodes, currentTick, players, entities);
    }

    void writeTo(Stream writer) const {
        writer.write(myId);
        writer.write(mapSize);
        writer.write(fogOfWar);
        writer.write(cast(int)(entityProperties.length));
        foreach (entityPropertiesKey, entityPropertiesValue; entityProperties) {
            writer.write(cast(int)(entityPropertiesKey));
            entityPropertiesValue.writeTo(writer);
        }
        writer.write(maxTickCount);
        writer.write(maxPathfindNodes);
        writer.write(currentTick);
        writer.write(cast(int)(players.length));
        foreach (playersElement; players) {
            playersElement.writeTo(writer);
        }
        writer.write(cast(int)(entities.length));
        foreach (entitiesElement; entities) {
            entitiesElement.writeTo(writer);
        }
    }
}