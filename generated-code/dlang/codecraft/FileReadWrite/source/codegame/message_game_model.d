module codegame.message_game_model;

import stream;
import std.conv;
import std.typecons : Nullable;
import codegame.client_message;
import codegame.server_message;

/// Client or server message
abstract class MessageGameModel {
    /// Write MessageGameModel to writer
    abstract void writeTo(Stream writer) const;

    /// Read MessageGameModel from reader
    static MessageGameModel readFrom(Stream reader) {
        switch (reader.readInt()) {
            case Client.TAG:
                return Client.readFrom(reader);
            case Server.TAG:
                return Server.readFrom(reader);
            default:
                throw new Exception("Unexpected tag value");
        }
    }
    
    /// Client message
    static class Client : MessageGameModel {
        static const int TAG = 0;
    
        /// Message
        codegame.ClientMessage message;
    
        this() {}
    
        this(codegame.ClientMessage message) {
            this.message = message;
        }
    
        /// Read Client from reader
        static Client readFrom(Stream reader) {
            codegame.ClientMessage message;
            message = codegame.ClientMessage.readFrom(reader);
            return new Client(message);
        }
    
        /// Write Client to writer
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            message.writeTo(writer);
        }
    }
    
    /// Server message
    static class Server : MessageGameModel {
        static const int TAG = 1;
    
        /// Message
        codegame.ServerMessage message;
    
        this() {}
    
        this(codegame.ServerMessage message) {
            this.message = message;
        }
    
        /// Read Server from reader
        static Server readFrom(Stream reader) {
            codegame.ServerMessage message;
            message = codegame.ServerMessage.readFrom(reader);
            return new Server(message);
        }
    
        /// Write Server to writer
        override void writeTo(Stream writer) const {
            writer.write(TAG);
            message.writeTo(writer);
        }
    }
}