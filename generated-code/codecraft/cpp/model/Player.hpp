#ifndef __MODEL_PLAYER_HPP__
#define __MODEL_PLAYER_HPP__

#include "../Stream.hpp"
#include <sstream>
#include <string>

class Player {
public:
    int id;
    int score;
    int resource;

    Player();

    Player(int id, int score, int resource);

    static Player readFrom(InputStream& stream);

    void writeTo(OutputStream& stream) const;

    std::string toString() const;

    bool operator ==(const Player& other) const;
};

namespace std {
    template<>
    struct hash<Player> {
        size_t operator ()(const Player& value) const;
    };
}

#endif