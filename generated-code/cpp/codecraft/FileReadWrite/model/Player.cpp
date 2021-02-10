#include "Player.hpp"

namespace model {

Player::Player(int id, int score, int resource) : id(id), score(score), resource(resource) { }

// Read Player from input stream
Player Player::readFrom(InputStream& stream) {
    int id = stream.readInt();
    int score = stream.readInt();
    int resource = stream.readInt();
    return Player(id, score, resource);
}

// Write Player to output stream
void Player::writeTo(OutputStream& stream) const {
    stream.write(id);
    stream.write(score);
    stream.write(resource);
}

// Get string representation of Player
std::string Player::toString() const {
    std::stringstream ss;
    ss << "Player { ";
    ss << "id: ";
    ss << id;
    ss << ", ";
    ss << "score: ";
    ss << score;
    ss << ", ";
    ss << "resource: ";
    ss << resource;
    ss << " }";
    return ss.str();
}

bool Player::operator ==(const Player& other) const {
    return id == other.id && score == other.score && resource == other.resource;
}

}

size_t std::hash<model::Player>::operator ()(const model::Player& value) const {
    size_t result = 0;
    result ^= std::hash<int>{}(value.id) + 0x9e3779b9 + (result << 6) + (result >> 2);
    result ^= std::hash<int>{}(value.score) + 0x9e3779b9 + (result << 6) + (result >> 2);
    result ^= std::hash<int>{}(value.resource) + 0x9e3779b9 + (result << 6) + (result >> 2);
    return result;
}