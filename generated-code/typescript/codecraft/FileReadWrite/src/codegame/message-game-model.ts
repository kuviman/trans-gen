import { ClientMessage } from "./client-message";
import { ServerMessage } from "./server-message";
import { Stream } from "../stream";

/**
 * Client or server message
 */
export abstract class MessageGameModel {
    /**
     * Write MessageGameModel to output stream
     */
    abstract writeTo(stream: Stream): Promise<void>;

    /**
     * Read MessageGameModel from input stream
     */
    static async readFrom(stream: Stream): Promise<MessageGameModel> {
        const tag = await stream.readInt();
        if (tag == MessageGameModel.Client.TAG) {
            return await MessageGameModel.Client.readFrom(stream);
        }
        if (tag == MessageGameModel.Server.TAG) {
            return await MessageGameModel.Server.readFrom(stream);
        }
        throw new Error("Unexpected tag value");
    }
}

export namespace MessageGameModel {
    /**
     * Client message
     */
    export class Client extends MessageGameModel {
        /**
         * Message
         */
        message: ClientMessage
    
        constructor(message: ClientMessage) {
            super();
            this.message = message;
        }
    
        /**
         * Read Client from input stream
         */
        static async readFrom(stream: Stream): Promise<MessageGameModel.Client> {
            let message;
            message = await ClientMessage.readFrom(stream);
            return new Client(message)
        }
    
        /**
         * Write Client to output stream
         */
        async writeTo(stream: Stream) {
            await stream.writeInt(Client.TAG);
            let message = this.message;
            await message.writeTo(stream);
        }
    }
    
    export namespace Client {
        export const TAG = 0;
    }
    /**
     * Server message
     */
    export class Server extends MessageGameModel {
        /**
         * Message
         */
        message: ServerMessage
    
        constructor(message: ServerMessage) {
            super();
            this.message = message;
        }
    
        /**
         * Read Server from input stream
         */
        static async readFrom(stream: Stream): Promise<MessageGameModel.Server> {
            let message;
            message = await ServerMessage.readFrom(stream);
            return new Server(message)
        }
    
        /**
         * Write Server to output stream
         */
        async writeTo(stream: Stream) {
            await stream.writeInt(Server.TAG);
            let message = this.message;
            await message.writeTo(stream);
        }
    }
    
    export namespace Server {
        export const TAG = 1;
    }
}