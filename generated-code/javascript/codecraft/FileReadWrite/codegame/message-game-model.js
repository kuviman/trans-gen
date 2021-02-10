const ClientMessage = require.main.require('./codegame/client-message');
const ServerMessage = require.main.require('./codegame/server-message');

/**
 * Client or server message
 */
class MessageGameModel {
    /**
     * Read MessageGameModel from input stream
     */
    static async readFrom(stream) {
        let tag = await stream.readInt();
        if (tag == Client.TAG) {
            return await Client.readFrom(stream);
        }
        if (tag == Server.TAG) {
            return await Server.readFrom(stream);
        }
        throw new Error("Unexpected tag value");
    }
}
/**
 * Client message
 */
class Client extends MessageGameModel {
    /**
     * Message
     */
    message;

    constructor(message) {
        super();
        this.message = message;
    }

    /**
     * Read Client from input stream
     */
    static async readFrom(stream) {
        let message;
        message = await ClientMessage.readFrom(stream);
        return new Client(message);
    }

    /**
     * Write Client to output stream
     */
    async writeTo(stream) {
        await stream.writeInt(Client.TAG);
        let message = this.message;
        await message.writeTo(stream);
    }
}

Client.TAG = 0;
MessageGameModel.Client = Client;
/**
 * Server message
 */
class Server extends MessageGameModel {
    /**
     * Message
     */
    message;

    constructor(message) {
        super();
        this.message = message;
    }

    /**
     * Read Server from input stream
     */
    static async readFrom(stream) {
        let message;
        message = await ServerMessage.readFrom(stream);
        return new Server(message);
    }

    /**
     * Write Server to output stream
     */
    async writeTo(stream) {
        await stream.writeInt(Server.TAG);
        let message = this.message;
        await message.writeTo(stream);
    }
}

Server.TAG = 1;
MessageGameModel.Server = Server;
module.exports = MessageGameModel;