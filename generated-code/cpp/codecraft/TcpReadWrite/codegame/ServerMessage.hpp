#ifndef __MODEL_SERVER_MESSAGE_HPP__
#define __MODEL_SERVER_MESSAGE_HPP__

#include "Stream.hpp"
#include "Vec2Int.hpp"
#include "model/AttackProperties.hpp"
#include "model/BuildProperties.hpp"
#include "model/Entity.hpp"
#include "model/EntityProperties.hpp"
#include "model/EntityType.hpp"
#include "model/Player.hpp"
#include "model/PlayerView.hpp"
#include "model/RepairProperties.hpp"
#include <memory>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace codegame {

// Message sent from server
class ServerMessage {
public:
    // Get action for next tick
    class GetAction;
    // Signifies end of the game
    class Finish;
    // Debug update
    class DebugUpdate;

    // Read ServerMessage from input stream
    static std::shared_ptr<ServerMessage> readFrom(InputStream& stream);

    // Write ServerMessage to output stream
    virtual void writeTo(OutputStream& stream) const = 0;

    // Get string representation of ServerMessage
    virtual std::string toString() const = 0;
};

// Get action for next tick
class ServerMessage::GetAction : public ServerMessage {
public:
    static const int TAG = 0;

    // Player's view
    model::PlayerView playerView;
    // Whether app is running with debug interface available
    bool debugAvailable;

    GetAction(model::PlayerView playerView, bool debugAvailable);

    // Read GetAction from input stream
    static GetAction readFrom(InputStream& stream);

    // Write GetAction to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of GetAction
    std::string toString() const;
};

// Signifies end of the game
class ServerMessage::Finish : public ServerMessage {
public:
    static const int TAG = 1;


    Finish();

    // Read Finish from input stream
    static Finish readFrom(InputStream& stream);

    // Write Finish to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Finish
    std::string toString() const;

    bool operator ==(const Finish& other) const;
};

// Debug update
class ServerMessage::DebugUpdate : public ServerMessage {
public:
    static const int TAG = 2;

    // Player's view
    model::PlayerView playerView;

    DebugUpdate(model::PlayerView playerView);

    // Read DebugUpdate from input stream
    static DebugUpdate readFrom(InputStream& stream);

    // Write DebugUpdate to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of DebugUpdate
    std::string toString() const;
};

}

#endif