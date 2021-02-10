package trans_gen_test.codegame

import trans_gen_test.util.StreamUtil

/**
 * Client or server message
 */
sealed trait MessageGameModel {
    /**
     * Write MessageGameModel to output stream
     */
    def writeTo(stream: java.io.OutputStream)
}

object MessageGameModel {
    /**
     * Client message
     *
     * @param message Message
     */
    case class Client(message: trans_gen_test.codegame.ClientMessage) extends MessageGameModel {
        /**
         * Write Client to output stream
         */
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, Client.TAG)
            message.writeTo(stream)
        }
    
        /**
         * Get string representation of Client
         */
        override def toString(): String = {
            var stringBuilder = new StringBuilder("Client { ")
            stringBuilder.append("message: ")
            stringBuilder.append(message)
            stringBuilder.append(" }")
            stringBuilder.toString()
        }
    }
    
    object Client {
        val TAG: Int = 0
    
        /**
         * Read Client from input stream
         */
        def readFrom(stream: java.io.InputStream): Client = Client(
            trans_gen_test.codegame.ClientMessage.readFrom(stream)
        )
    }

    /**
     * Server message
     *
     * @param message Message
     */
    case class Server(message: trans_gen_test.codegame.ServerMessage) extends MessageGameModel {
        /**
         * Write Server to output stream
         */
        override def writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, Server.TAG)
            message.writeTo(stream)
        }
    
        /**
         * Get string representation of Server
         */
        override def toString(): String = {
            var stringBuilder = new StringBuilder("Server { ")
            stringBuilder.append("message: ")
            stringBuilder.append(message)
            stringBuilder.append(" }")
            stringBuilder.toString()
        }
    }
    
    object Server {
        val TAG: Int = 1
    
        /**
         * Read Server from input stream
         */
        def readFrom(stream: java.io.InputStream): Server = Server(
            trans_gen_test.codegame.ServerMessage.readFrom(stream)
        )
    }

    /**
     * Read MessageGameModel from input stream
     */
    def readFrom(stream: java.io.InputStream): MessageGameModel = {
        StreamUtil.readInt(stream) match {
            case Client.TAG => Client.readFrom(stream)
            case Server.TAG => Server.readFrom(stream)
            case _ => throw new java.io.IOException("Unexpected tag value")
        }
    }
}