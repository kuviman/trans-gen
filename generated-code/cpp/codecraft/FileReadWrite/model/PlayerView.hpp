#ifndef __MODEL_PLAYER_VIEW_HPP__
#define __MODEL_PLAYER_VIEW_HPP__

#include "Stream.hpp"
#include "Vec2Int.hpp"
#include "model/AttackProperties.hpp"
#include "model/BuildProperties.hpp"
#include "model/Entity.hpp"
#include "model/EntityProperties.hpp"
#include "model/EntityType.hpp"
#include "model/Player.hpp"
#include "model/RepairProperties.hpp"
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace model {

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
    std::unordered_map<model::EntityType, model::EntityProperties> entityProperties;
    // Max tick count for the game
    int maxTickCount;
    // Max pathfind nodes when performing pathfinding in the game simulator
    int maxPathfindNodes;
    // Current tick
    int currentTick;
    // List of players
    std::vector<model::Player> players;
    // List of entities
    std::vector<model::Entity> entities;

    PlayerView(int myId, int mapSize, bool fogOfWar, std::unordered_map<model::EntityType, model::EntityProperties> entityProperties, int maxTickCount, int maxPathfindNodes, int currentTick, std::vector<model::Player> players, std::vector<model::Entity> entities);

    // Read PlayerView from input stream
    static PlayerView readFrom(InputStream& stream);

    // Write PlayerView to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of PlayerView
    std::string toString() const;
};

}

#endif