#include "Player.hpp"

Player::Player() { }

Player::Player(int id, int score, int resource) : id(id), score(score), resource(resource) { }

Player Player::readFrom(InputStream& stream) {
    int id;
    id = stream.readInt();
    int score;
    score = stream.readInt();
    int resource;
    resource = stream.readInt();
    return Player(id, score, resource);
}

void Player::writeTo(OutputStream& stream) const {
    stream.write(id);
    stream.write(score);
    stream.write(resource);
}

bool Player::operator ==(const Player& other) const {
    return id == other.id && score == other.score && resource == other.resource;
}

size_t std::hash<Player>::operator ()(const Player& value) const {
    size_t result = 0;
    result ^= std::hash<int>{}(value.id) + 0x9e3779b9 + (result << 6) + (result >> 2);
    result ^= std::hash<int>{}(value.score) + 0x9e3779b9 + (result << 6) + (result >> 2);
    result ^= std::hash<int>{}(value.resource) + 0x9e3779b9 + (result << 6) + (result >> 2);
    return result;
}