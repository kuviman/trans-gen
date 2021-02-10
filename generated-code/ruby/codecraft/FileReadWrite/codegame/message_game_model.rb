require './codegame/client_message'
require './codegame/server_message'

module Codegame

# Client or server message
class MessageGameModel
    # Read MessageGameModel from input stream
    def self.read_from(stream)
        tag = stream.read_int()
        if tag == MessageGameModel::Client::TAG
            return MessageGameModel::Client.read_from(stream)
        end
        if tag == MessageGameModel::Server::TAG
            return MessageGameModel::Server.read_from(stream)
        end
        raise "Unexpected tag value"
    end

    # Client message
    class Client
        TAG = 0
    
        # Message
        attr_accessor :message
    
        def initialize(message)
            @message = message
        end
    
        # Read Client from input stream
        def self.read_from(stream)
            message = Codegame::ClientMessage.read_from(stream)
            Client.new(message)
        end
    
        # Write Client to output stream
        def write_to(stream)
            stream.write_int(TAG)
            @message.write_to(stream)
        end
    
        def to_s
            string_result = "Client { "
            string_result += "message: "
            string_result += @message.to_s
            string_result += " }"
            string_result
        end
    
        def to_str
            to_s
        end
    end
    # Server message
    class Server
        TAG = 1
    
        # Message
        attr_accessor :message
    
        def initialize(message)
            @message = message
        end
    
        # Read Server from input stream
        def self.read_from(stream)
            message = Codegame::ServerMessage.read_from(stream)
            Server.new(message)
        end
    
        # Write Server to output stream
        def write_to(stream)
            stream.write_int(TAG)
            @message.write_to(stream)
        end
    
        def to_s
            string_result = "Server { "
            string_result += "message: "
            string_result += @message.to_s
            string_result += " }"
            string_result
        end
    
        def to_str
            to_s
        end
    end
end

end