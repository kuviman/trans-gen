#ifndef __MODEL_PLAYER_VIEW_HPP__
#define __MODEL_PLAYER_VIEW_HPP__

#include "../Stream.hpp"
#include "AttackProperties.hpp"
#include "BuildProperties.hpp"
#include "Entity.hpp"
#include "EntityProperties.hpp"
#include "EntityType.hpp"
#include "Player.hpp"
#include "RepairProperties.hpp"
#include "Vec2Int.hpp"
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

// Information available to the player
class PlayerView {
public:
    // Your player's ID
    int myId;
    // Size of the map
    int mapSize;
    // Whether fog of war is enabled
    bool fogOfWar;
    // Entity properties for each entity type
    std::unordered_map<EntityType, EntityProperties> entityProperties;
    // Max tick count for the game
    int maxTickCount;
    // Max pathfind nodes when performing pathfinding in the game simulator
    int maxPathfindNodes;
    // Current tick
    int currentTick;
    // List of players
    std::vector<Player> players;
    // List of entities
    std::vector<Entity> entities;

    PlayerView(int myId, int mapSize, bool fogOfWar, std::unordered_map<EntityType, EntityProperties> entityProperties, int maxTickCount, int maxPathfindNodes, int currentTick, std::vector<Player> players, std::vector<Entity> entities);

    // Read PlayerView from input stream
    static PlayerView readFrom(InputStream& stream);

    // Write PlayerView to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of PlayerView
    std::string toString() const;
};

#endif