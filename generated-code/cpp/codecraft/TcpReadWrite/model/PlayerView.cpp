#include "PlayerView.hpp"

namespace model {

PlayerView::PlayerView(int myId, int mapSize, bool fogOfWar, std::unordered_map<model::EntityType, model::EntityProperties> entityProperties, int maxTickCount, int maxPathfindNodes, int currentTick, std::vector<model::Player> players, std::vector<model::Entity> entities) : myId(myId), mapSize(mapSize), fogOfWar(fogOfWar), entityProperties(entityProperties), maxTickCount(maxTickCount), maxPathfindNodes(maxPathfindNodes), currentTick(currentTick), players(players), entities(entities) { }

// Read PlayerView from input stream
PlayerView PlayerView::readFrom(InputStream& stream) {
    int myId = stream.readInt();
    int mapSize = stream.readInt();
    bool fogOfWar = stream.readBool();
    size_t entityPropertiesSize = stream.readInt();
    std::unordered_map<model::EntityType, model::EntityProperties> entityProperties = std::unordered_map<model::EntityType, model::EntityProperties>();
    entityProperties.reserve(entityPropertiesSize);
    for (size_t entityPropertiesIndex = 0; entityPropertiesIndex < entityPropertiesSize; entityPropertiesIndex++) {
        model::EntityType entityPropertiesKey = readEntityType(stream);
        model::EntityProperties entityPropertiesValue = model::EntityProperties::readFrom(stream);
        entityProperties.emplace(std::make_pair(entityPropertiesKey, entityPropertiesValue));
    }
    int maxTickCount = stream.readInt();
    int maxPathfindNodes = stream.readInt();
    int currentTick = stream.readInt();
    std::vector<model::Player> players = std::vector<model::Player>();
    size_t playersSize = stream.readInt();
    players.reserve(playersSize);
    for (size_t playersIndex = 0; playersIndex < playersSize; playersIndex++) {
        model::Player playersElement = model::Player::readFrom(stream);
        players.emplace_back(playersElement);
    }
    std::vector<model::Entity> entities = std::vector<model::Entity>();
    size_t entitiesSize = stream.readInt();
    entities.reserve(entitiesSize);
    for (size_t entitiesIndex = 0; entitiesIndex < entitiesSize; entitiesIndex++) {
        model::Entity entitiesElement = model::Entity::readFrom(stream);
        entities.emplace_back(entitiesElement);
    }
    return PlayerView(myId, mapSize, fogOfWar, entityProperties, maxTickCount, maxPathfindNodes, currentTick, players, entities);
}

// Write PlayerView to output stream
void PlayerView::writeTo(OutputStream& stream) const {
    stream.write(myId);
    stream.write(mapSize);
    stream.write(fogOfWar);
    stream.write((int)(entityProperties.size()));
    for (const auto& entityPropertiesEntry : entityProperties) {
        const model::EntityType& entityPropertiesKey = entityPropertiesEntry.first;
        const model::EntityProperties& entityPropertiesValue = entityPropertiesEntry.second;
        stream.write((int)(entityPropertiesKey));
        entityPropertiesValue.writeTo(stream);
    }
    stream.write(maxTickCount);
    stream.write(maxPathfindNodes);
    stream.write(currentTick);
    stream.write((int)(players.size()));
    for (const model::Player& playersElement : players) {
        playersElement.writeTo(stream);
    }
    stream.write((int)(entities.size()));
    for (const model::Entity& entitiesElement : entities) {
        entitiesElement.writeTo(stream);
    }
}

// Get string representation of PlayerView
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
        const model::EntityType& entityPropertiesKey = entityPropertiesEntry.first;
        const model::EntityProperties& entityPropertiesValue = entityPropertiesEntry.second;
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
        const model::Player& playersElement = players[playersIndex];
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
        const model::Entity& entitiesElement = entities[entitiesIndex];
        if (entitiesIndex != 0) {
            ss << ", ";
        }
        ss << entitiesElement.toString();
    }
    ss << " ]";
    ss << " }";
    return ss.str();
}

}