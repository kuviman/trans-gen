#ifndef __MODEL_ENTITY_HPP__
#define __MODEL_ENTITY_HPP__

#include "../Stream.hpp"
#include "EntityType.hpp"
#include "Vec2Int.hpp"
#include <memory>
#include <stdexcept>
#include <string>

class Entity {
public:
    int id;
    std::shared_ptr<int> playerId;
    EntityType entityType;
    Vec2Int position;
    int health;
    bool active;

    Entity();

    Entity(int id, std::shared_ptr<int> playerId, EntityType entityType, Vec2Int position, int health, bool active);

    static Entity readFrom(InputStream& stream);

    void writeTo(OutputStream& stream) const;
};

#endif