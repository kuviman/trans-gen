package trans_gen_test.model;

import trans_gen_test.util.StreamUtil;

/**
 * Information available to the player
 */
public class PlayerView {
    /**
     * Your player's ID
     */
    private int myId;

    /**
     * Your player's ID
     */
    public int getMyId() {
        return myId;
    }

    /**
     * Your player's ID
     */
    public void setMyId(int value) {
        this.myId = value;
    }
    /**
     * Size of the map
     */
    private int mapSize;

    /**
     * Size of the map
     */
    public int getMapSize() {
        return mapSize;
    }

    /**
     * Size of the map
     */
    public void setMapSize(int value) {
        this.mapSize = value;
    }
    /**
     * Whether fog of war is enabled
     */
    private boolean fogOfWar;

    /**
     * Whether fog of war is enabled
     */
    public boolean isFogOfWar() {
        return fogOfWar;
    }

    /**
     * Whether fog of war is enabled
     */
    public void setFogOfWar(boolean value) {
        this.fogOfWar = value;
    }
    /**
     * Entity properties for each entity type
     */
    private java.util.Map<trans_gen_test.model.EntityType, trans_gen_test.model.EntityProperties> entityProperties;

    /**
     * Entity properties for each entity type
     */
    public java.util.Map<trans_gen_test.model.EntityType, trans_gen_test.model.EntityProperties> getEntityProperties() {
        return entityProperties;
    }

    /**
     * Entity properties for each entity type
     */
    public void setEntityProperties(java.util.Map<trans_gen_test.model.EntityType, trans_gen_test.model.EntityProperties> value) {
        this.entityProperties = value;
    }
    /**
     * Max tick count for the game
     */
    private int maxTickCount;

    /**
     * Max tick count for the game
     */
    public int getMaxTickCount() {
        return maxTickCount;
    }

    /**
     * Max tick count for the game
     */
    public void setMaxTickCount(int value) {
        this.maxTickCount = value;
    }
    /**
     * Max pathfind nodes when performing pathfinding in the game simulator
     */
    private int maxPathfindNodes;

    /**
     * Max pathfind nodes when performing pathfinding in the game simulator
     */
    public int getMaxPathfindNodes() {
        return maxPathfindNodes;
    }

    /**
     * Max pathfind nodes when performing pathfinding in the game simulator
     */
    public void setMaxPathfindNodes(int value) {
        this.maxPathfindNodes = value;
    }
    /**
     * Current tick
     */
    private int currentTick;

    /**
     * Current tick
     */
    public int getCurrentTick() {
        return currentTick;
    }

    /**
     * Current tick
     */
    public void setCurrentTick(int value) {
        this.currentTick = value;
    }
    /**
     * List of players
     */
    private trans_gen_test.model.Player[] players;

    /**
     * List of players
     */
    public trans_gen_test.model.Player[] getPlayers() {
        return players;
    }

    /**
     * List of players
     */
    public void setPlayers(trans_gen_test.model.Player[] value) {
        this.players = value;
    }
    /**
     * List of entities
     */
    private trans_gen_test.model.Entity[] entities;

    /**
     * List of entities
     */
    public trans_gen_test.model.Entity[] getEntities() {
        return entities;
    }

    /**
     * List of entities
     */
    public void setEntities(trans_gen_test.model.Entity[] value) {
        this.entities = value;
    }

    public PlayerView(int myId, int mapSize, boolean fogOfWar, java.util.Map<trans_gen_test.model.EntityType, trans_gen_test.model.EntityProperties> entityProperties, int maxTickCount, int maxPathfindNodes, int currentTick, trans_gen_test.model.Player[] players, trans_gen_test.model.Entity[] entities) {
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

    /**
     * Read PlayerView from input stream
     */
    public static PlayerView readFrom(java.io.InputStream stream) throws java.io.IOException {
        int myId;
        myId = StreamUtil.readInt(stream);
        int mapSize;
        mapSize = StreamUtil.readInt(stream);
        boolean fogOfWar;
        fogOfWar = StreamUtil.readBoolean(stream);
        java.util.Map<trans_gen_test.model.EntityType, trans_gen_test.model.EntityProperties> entityProperties;
        int entityPropertiesSize = StreamUtil.readInt(stream);
        entityProperties = new java.util.HashMap<>(entityPropertiesSize);
        for (int entityPropertiesIndex = 0; entityPropertiesIndex < entityPropertiesSize; entityPropertiesIndex++) {
            trans_gen_test.model.EntityType entityPropertiesKey;
            entityPropertiesKey = trans_gen_test.model.EntityType.readFrom(stream);
            trans_gen_test.model.EntityProperties entityPropertiesValue;
            entityPropertiesValue = trans_gen_test.model.EntityProperties.readFrom(stream);
            entityProperties.put(entityPropertiesKey, entityPropertiesValue);
        }
        int maxTickCount;
        maxTickCount = StreamUtil.readInt(stream);
        int maxPathfindNodes;
        maxPathfindNodes = StreamUtil.readInt(stream);
        int currentTick;
        currentTick = StreamUtil.readInt(stream);
        trans_gen_test.model.Player[] players;
        players = new trans_gen_test.model.Player[StreamUtil.readInt(stream)];
        for (int playersIndex = 0; playersIndex < players.length; playersIndex++) {
            trans_gen_test.model.Player playersElement;
            playersElement = trans_gen_test.model.Player.readFrom(stream);
            players[playersIndex] = playersElement;
        }
        trans_gen_test.model.Entity[] entities;
        entities = new trans_gen_test.model.Entity[StreamUtil.readInt(stream)];
        for (int entitiesIndex = 0; entitiesIndex < entities.length; entitiesIndex++) {
            trans_gen_test.model.Entity entitiesElement;
            entitiesElement = trans_gen_test.model.Entity.readFrom(stream);
            entities[entitiesIndex] = entitiesElement;
        }
        return new PlayerView(myId, mapSize, fogOfWar, entityProperties, maxTickCount, maxPathfindNodes, currentTick, players, entities);
    }

    /**
     * Write PlayerView to output stream
     */
    public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
        StreamUtil.writeInt(stream, myId);
        StreamUtil.writeInt(stream, mapSize);
        StreamUtil.writeBoolean(stream, fogOfWar);
        StreamUtil.writeInt(stream, entityProperties.size());
        for (java.util.Map.Entry<trans_gen_test.model.EntityType, trans_gen_test.model.EntityProperties> entityPropertiesEntry : entityProperties.entrySet()) {
            trans_gen_test.model.EntityType entityPropertiesKey = entityPropertiesEntry.getKey();
            StreamUtil.writeInt(stream, entityPropertiesKey.tag);
            trans_gen_test.model.EntityProperties entityPropertiesValue = entityPropertiesEntry.getValue();
            entityPropertiesValue.writeTo(stream);
        }
        StreamUtil.writeInt(stream, maxTickCount);
        StreamUtil.writeInt(stream, maxPathfindNodes);
        StreamUtil.writeInt(stream, currentTick);
        StreamUtil.writeInt(stream, players.length);
        for (trans_gen_test.model.Player playersElement : players) {
            playersElement.writeTo(stream);
        }
        StreamUtil.writeInt(stream, entities.length);
        for (trans_gen_test.model.Entity entitiesElement : entities) {
            entitiesElement.writeTo(stream);
        }
    }

    /**
     * Get string representation of PlayerView
     */
    @Override
    public String toString() {
        StringBuilder stringBuilder = new StringBuilder("PlayerView { ");
        stringBuilder.append("myId: ");
        stringBuilder.append(String.valueOf(myId));
        stringBuilder.append(", ");
        stringBuilder.append("mapSize: ");
        stringBuilder.append(String.valueOf(mapSize));
        stringBuilder.append(", ");
        stringBuilder.append("fogOfWar: ");
        stringBuilder.append(String.valueOf(fogOfWar));
        stringBuilder.append(", ");
        stringBuilder.append("entityProperties: ");
        stringBuilder.append(String.valueOf(entityProperties));
        stringBuilder.append(", ");
        stringBuilder.append("maxTickCount: ");
        stringBuilder.append(String.valueOf(maxTickCount));
        stringBuilder.append(", ");
        stringBuilder.append("maxPathfindNodes: ");
        stringBuilder.append(String.valueOf(maxPathfindNodes));
        stringBuilder.append(", ");
        stringBuilder.append("currentTick: ");
        stringBuilder.append(String.valueOf(currentTick));
        stringBuilder.append(", ");
        stringBuilder.append("players: ");
        stringBuilder.append("[ ");
        for (int playersIndex = 0; playersIndex < players.length; playersIndex++) {
            if (playersIndex != 0) {
                stringBuilder.append(", ");
            }
            trans_gen_test.model.Player playersElement = players[playersIndex];
            stringBuilder.append(String.valueOf(playersElement));
        }
        stringBuilder.append(" ]");
        stringBuilder.append(", ");
        stringBuilder.append("entities: ");
        stringBuilder.append("[ ");
        for (int entitiesIndex = 0; entitiesIndex < entities.length; entitiesIndex++) {
            if (entitiesIndex != 0) {
                stringBuilder.append(", ");
            }
            trans_gen_test.model.Entity entitiesElement = entities[entitiesIndex];
            stringBuilder.append(String.valueOf(entitiesElement));
        }
        stringBuilder.append(" ]");
        stringBuilder.append(" }");
        return stringBuilder.toString();
    }
}