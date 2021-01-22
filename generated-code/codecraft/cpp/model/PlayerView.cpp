#include "PlayerView.hpp"

PlayerView::PlayerView() { }

PlayerView::PlayerView(int myId, int mapSize, bool fogOfWar, std::unordered_map<EntityType, EntityProperties> entityProperties, int maxTickCount, int maxPathfindNodes, int currentTick, std::vector<Player> players, std::vector<Entity> entities) : myId(myId), mapSize(mapSize), fogOfWar(fogOfWar), entityProperties(entityProperties), maxTickCount(maxTickCount), maxPathfindNodes(maxPathfindNodes), currentTick(currentTick), players(players), entities(entities) { }

PlayerView PlayerView::readFrom(InputStream& stream) {
    int myId;
    myId = stream.readInt();
    int mapSize;
    mapSize = stream.readInt();
    bool fogOfWar;
    fogOfWar = stream.readBool();
    std::unordered_map<EntityType, EntityProperties> entityProperties;
    size_t entityPropertiesSize = stream.readInt();
    entityProperties = std::unordered_map<EntityType, EntityProperties>();
    entityProperties.reserve(entityPropertiesSize);
    for (size_t entityPropertiesIndex = 0; entityPropertiesIndex < entityPropertiesSize; entityPropertiesIndex++) {
        EntityType entityPropertiesKey;
        EntityProperties entityPropertiesValue;
        entityPropertiesKey = readEntityType(stream);
        entityPropertiesValue = EntityProperties::readFrom(stream);
        entityProperties.emplace(std::make_pair(entityPropertiesKey, entityPropertiesValue));
    }
    int maxTickCount;
    maxTickCount = stream.readInt();
    int maxPathfindNodes;
    maxPathfindNodes = stream.readInt();
    int currentTick;
    currentTick = stream.readInt();
    std::vector<Player> players;
    players = std::vector<Player>(stream.readInt());
    for (size_t playersIndex = 0; playersIndex < players.size(); playersIndex++) {
        players[playersIndex] = Player::readFrom(stream);
    }
    std::vector<Entity> entities;
    entities = std::vector<Entity>(stream.readInt());
    for (size_t entitiesIndex = 0; entitiesIndex < entities.size(); entitiesIndex++) {
        entities[entitiesIndex] = Entity::readFrom(stream);
    }
    return PlayerView(myId, mapSize, fogOfWar, entityProperties, maxTickCount, maxPathfindNodes, currentTick, players, entities);
}

void PlayerView::writeTo(OutputStream& stream) const {
    stream.write(myId);
    stream.write(mapSize);
    stream.write(fogOfWar);
    stream.write((int)(entityProperties.size()));
    for (const auto& entityPropertiesEntry : entityProperties) {
        const EntityType& entityPropertiesKey = entityPropertiesEntry.first;
        const EntityProperties& entityPropertiesValue = entityPropertiesEntry.second;
        stream.write((int)(entityPropertiesKey));
        entityPropertiesValue.writeTo(stream);
    }
    stream.write(maxTickCount);
    stream.write(maxPathfindNodes);
    stream.write(currentTick);
    stream.write((int)(players.size()));
    for (const Player& playersElement : players) {
        playersElement.writeTo(stream);
    }
    stream.write((int)(entities.size()));
    for (const Entity& entitiesElement : entities) {
        entitiesElement.writeTo(stream);
    }
}

std::string PlayerView::toString() const {
    std::stringstream ss;
    ss << "PlayerView { ";
    ss << "myId: ";
    ss << myId;
    ss << ", ";
    ss << "mapSize: ";
    ss << mapSize;
    ss << ", ";
    ss << "fogOfWar: ";
    ss << fogOfWar;
    ss << ", ";
    ss << "entityProperties: ";
    ss << "{ ";
    size_t entityPropertiesIndex = 0;
    for (const auto& entityPropertiesEntry : entityProperties) {
        if (entityPropertiesIndex != 0) {
            ss << ", ";
        }
        const EntityType& entityPropertiesKey = entityPropertiesEntry.first;
        const EntityProperties& entityPropertiesValue = entityPropertiesEntry.second;
        ss << entityTypeToString(entityPropertiesKey);
        ss << ": ";
        ss << entityPropertiesValue.toString();
        entityPropertiesIndex++;
    }
    ss << " }";
    ss << ", ";
    ss << "maxTickCount: ";
    ss << maxTickCount;
    ss << ", ";
    ss << "maxPathfindNodes: ";
    ss << maxPathfindNodes;
    ss << ", ";
    ss << "currentTick: ";
    ss << currentTick;
    ss << ", ";
    ss << "players: ";
    ss << "[ ";
    for (size_t playersIndex = 0; playersIndex < players.size(); playersIndex++) {
        const Player& playersElement = players[playersIndex];
        if (playersIndex != 0) {
            ss << ", ";
        }
        ss << playersElement.toString();
    }
    ss << " ]";
    ss << ", ";
    ss << "entities: ";
    ss << "[ ";
    for (size_t entitiesIndex = 0; entitiesIndex < entities.size(); entitiesIndex++) {
        const Entity& entitiesElement = entities[entitiesIndex];
        if (entitiesIndex != 0) {
            ss << ", ";
        }
        ss << entitiesElement.toString();
    }
    ss << " ]";
    ss << " }";
    return ss.str();
}