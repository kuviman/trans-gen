package trans_gen_test.codegame

import trans_gen_test.util.StreamUtil

/**
 * Client or server message
 */
abstract class MessageGameModel {
    /**
     * Write MessageGameModel to output stream
     */
    @Throws(java.io.IOException::class)
    abstract fun writeTo(stream: java.io.OutputStream)

    companion object {
        /**
         * Read MessageGameModel from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): MessageGameModel {
            when (StreamUtil.readInt(stream)) {
                Client.TAG -> return Client.readFrom(stream)
                Server.TAG -> return Server.readFrom(stream)
                else -> throw java.io.IOException("Unexpected tag value")
            }
        }
    }

    /**
     * Client message
     */
    class Client : MessageGameModel {
        /**
         * Message
         */
        var message: trans_gen_test.codegame.ClientMessage
    
        constructor(message: trans_gen_test.codegame.ClientMessage) {
            this.message = message
        }
    
        /**
         * Write Client to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            message.writeTo(stream)
        }
    
        /**
         * Get string representation of Client
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("Client { ")
            stringBuilder.append("message: ")
            stringBuilder.append(message)
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 0
    
            /**
             * Read Client from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): Client {
                var message: trans_gen_test.codegame.ClientMessage
                message = trans_gen_test.codegame.ClientMessage.readFrom(stream)
                return Client(message)
            }
        }
    }

    /**
     * Server message
     */
    class Server : MessageGameModel {
        /**
         * Message
         */
        var message: trans_gen_test.codegame.ServerMessage
    
        constructor(message: trans_gen_test.codegame.ServerMessage) {
            this.message = message
        }
    
        /**
         * Write Server to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            message.writeTo(stream)
        }
    
        /**
         * Get string representation of Server
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("Server { ")
            stringBuilder.append("message: ")
            stringBuilder.append(message)
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 1
    
            /**
             * Read Server from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): Server {
                var message: trans_gen_test.codegame.ServerMessage
                message = trans_gen_test.codegame.ServerMessage.readFrom(stream)
                return Server(message)
            }
        }
    }
}