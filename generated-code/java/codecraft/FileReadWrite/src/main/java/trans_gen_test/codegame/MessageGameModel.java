package trans_gen_test.codegame;

import trans_gen_test.util.StreamUtil;

/**
 * Client or server message
 */
public abstract class MessageGameModel {
    /**
     * Write MessageGameModel to output stream
     */
    public abstract void writeTo(java.io.OutputStream stream) throws java.io.IOException;

    /**
     * Read MessageGameModel from input stream
     */
    public static MessageGameModel readFrom(java.io.InputStream stream) throws java.io.IOException {
        switch (StreamUtil.readInt(stream)) {
            case Client.TAG:
                return Client.readFrom(stream);
            case Server.TAG:
                return Server.readFrom(stream);
            default:
                throw new java.io.IOException("Unexpected tag value");
        }
    }

    /**
     * Client message
     */
    public static class Client extends MessageGameModel {
        public static final int TAG = 0;
    
        /**
         * Message
         */
        private trans_gen_test.codegame.ClientMessage message;
    
        /**
         * Message
         */
        public trans_gen_test.codegame.ClientMessage getMessage() {
            return message;
        }
    
        /**
         * Message
         */
        public void setMessage(trans_gen_test.codegame.ClientMessage value) {
            this.message = value;
        }
    
        public Client(trans_gen_test.codegame.ClientMessage message) {
            this.message = message;
        }
    
        /**
         * Read Client from input stream
         */
        public static Client readFrom(java.io.InputStream stream) throws java.io.IOException {
            trans_gen_test.codegame.ClientMessage message;
            message = trans_gen_test.codegame.ClientMessage.readFrom(stream);
            return new Client(message);
        }
    
        /**
         * Write Client to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            message.writeTo(stream);
        }
    
        /**
         * Get string representation of Client
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("Client { ");
            stringBuilder.append("message: ");
            stringBuilder.append(String.valueOf(message));
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }

    /**
     * Server message
     */
    public static class Server extends MessageGameModel {
        public static final int TAG = 1;
    
        /**
         * Message
         */
        private trans_gen_test.codegame.ServerMessage message;
    
        /**
         * Message
         */
        public trans_gen_test.codegame.ServerMessage getMessage() {
            return message;
        }
    
        /**
         * Message
         */
        public void setMessage(trans_gen_test.codegame.ServerMessage value) {
            this.message = value;
        }
    
        public Server(trans_gen_test.codegame.ServerMessage message) {
            this.message = message;
        }
    
        /**
         * Read Server from input stream
         */
        public static Server readFrom(java.io.InputStream stream) throws java.io.IOException {
            trans_gen_test.codegame.ServerMessage message;
            message = trans_gen_test.codegame.ServerMessage.readFrom(stream);
            return new Server(message);
        }
    
        /**
         * Write Server to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            message.writeTo(stream);
        }
    
        /**
         * Get string representation of Server
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("Server { ");
            stringBuilder.append("message: ");
            stringBuilder.append(String.valueOf(message));
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }
}