#ifndef __MODEL_MESSAGE_GAME_MODEL_HPP__
#define __MODEL_MESSAGE_GAME_MODEL_HPP__

#include "Color.hpp"
#include "Stream.hpp"
#include "Vec2Float.hpp"
#include "Vec2Int.hpp"
#include "codegame/ClientMessage.hpp"
#include "codegame/DebugCommand.hpp"
#include "codegame/ServerMessage.hpp"
#include "model/Action.hpp"
#include "model/AttackAction.hpp"
#include "model/AttackProperties.hpp"
#include "model/AutoAttack.hpp"
#include "model/BuildAction.hpp"
#include "model/BuildProperties.hpp"
#include "model/Entity.hpp"
#include "model/EntityAction.hpp"
#include "model/EntityProperties.hpp"
#include "model/EntityType.hpp"
#include "model/MoveAction.hpp"
#include "model/Player.hpp"
#include "model/PlayerView.hpp"
#include "model/RepairAction.hpp"
#include "model/RepairProperties.hpp"
#include "model/debug_interface/ColoredVertex.hpp"
#include "model/debug_interface/DebugData.hpp"
#include "model/debug_interface/PrimitiveType.hpp"
#include <memory>
#include <optional>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace codegame {

// Client or server message
class MessageGameModel {
public:
    // Client message
    class Client;
    // Server message
    class Server;

    // Read MessageGameModel from input stream
    static std::shared_ptr<MessageGameModel> readFrom(InputStream& stream);

    // Write MessageGameModel to output stream
    virtual void writeTo(OutputStream& stream) const = 0;

    // Get string representation of MessageGameModel
    virtual std::string toString() const = 0;
};

// Client message
class MessageGameModel::Client : public MessageGameModel {
public:
    static const int TAG = 0;

    // Message
    std::shared_ptr<codegame::ClientMessage> message;

    Client(std::shared_ptr<codegame::ClientMessage> message);

    // Read Client from input stream
    static Client readFrom(InputStream& stream);

    // Write Client to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Client
    std::string toString() const;
};

// Server message
class MessageGameModel::Server : public MessageGameModel {
public:
    static const int TAG = 1;

    // Message
    std::shared_ptr<codegame::ServerMessage> message;

    Server(std::shared_ptr<codegame::ServerMessage> message);

    // Read Server from input stream
    static Server readFrom(InputStream& stream);

    // Write Server to output stream
    void writeTo(OutputStream& stream) const;

    // Get string representation of Server
    std::string toString() const;
};

}

#endif