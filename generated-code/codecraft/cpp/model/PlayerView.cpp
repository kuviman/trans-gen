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
        switch (stream.readInt()) {
        case 0:
            entityPropertiesKey = EntityType::WALL;
            break;
        case 1:
            entityPropertiesKey = EntityType::HOUSE;
            break;
        case 2:
            entityPropertiesKey = EntityType::BUILDER_BASE;
            break;
        case 3:
            entityPropertiesKey = EntityType::BUILDER_UNIT;
            break;
        case 4:
            entityPropertiesKey = EntityType::MELEE_BASE;
            break;
        case 5:
            entityPropertiesKey = EntityType::MELEE_UNIT;
            break;
        case 6:
            entityPropertiesKey = EntityType::RANGED_BASE;
            break;
        case 7:
            entityPropertiesKey = EntityType::RANGED_UNIT;
            break;
        case 8:
            entityPropertiesKey = EntityType::RESOURCE;
            break;
        case 9:
            entityPropertiesKey = EntityType::TURRET;
            break;
        default:
            throw std::runtime_error("Unexpected tag value");
        }
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