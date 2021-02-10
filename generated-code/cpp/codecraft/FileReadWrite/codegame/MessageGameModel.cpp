#include "MessageGameModel.hpp"
#include <stdexcept>

namespace codegame {

MessageGameModel::Client::Client(std::shared_ptr<codegame::ClientMessage> message) : message(message) { }

// Read Client from input stream
MessageGameModel::Client MessageGameModel::Client::readFrom(InputStream& stream) {
    std::shared_ptr<codegame::ClientMessage> message = codegame::ClientMessage::readFrom(stream);
    return MessageGameModel::Client(message);
}

// Write Client to output stream
void MessageGameModel::Client::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    message->writeTo(stream);
}

// Get string representation of Client
std::string MessageGameModel::Client::toString() const {
    std::stringstream ss;
    ss << "MessageGameModel::Client { ";
    ss << "message: ";
    ss << message->toString();
    ss << " }";
    return ss.str();
}

MessageGameModel::Server::Server(std::shared_ptr<codegame::ServerMessage> message) : message(message) { }

// Read Server from input stream
MessageGameModel::Server MessageGameModel::Server::readFrom(InputStream& stream) {
    std::shared_ptr<codegame::ServerMessage> message = codegame::ServerMessage::readFrom(stream);
    return MessageGameModel::Server(message);
}

// Write Server to output stream
void MessageGameModel::Server::writeTo(OutputStream& stream) const {
    stream.write(TAG);
    message->writeTo(stream);
}

// Get string representation of Server
std::string MessageGameModel::Server::toString() const {
    std::stringstream ss;
    ss << "MessageGameModel::Server { ";
    ss << "message: ";
    ss << message->toString();
    ss << " }";
    return ss.str();
}

// Read MessageGameModel from input stream
std::shared_ptr<MessageGameModel> MessageGameModel::readFrom(InputStream& stream) {
    switch (stream.readInt()) {
    case 0:
        return std::shared_ptr<MessageGameModel::Client>(new MessageGameModel::Client(MessageGameModel::Client::readFrom(stream)));
    case 1:
        return std::shared_ptr<MessageGameModel::Server>(new MessageGameModel::Server(MessageGameModel::Server::readFrom(stream)));
    default:
        throw std::runtime_error("Unexpected tag value");
    }
}

}