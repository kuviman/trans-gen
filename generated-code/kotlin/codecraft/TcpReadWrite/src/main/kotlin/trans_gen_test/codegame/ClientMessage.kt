package trans_gen_test.codegame

import trans_gen_test.util.StreamUtil

/**
 * Message sent from client
 */
abstract class ClientMessage {
    /**
     * Write ClientMessage to output stream
     */
    @Throws(java.io.IOException::class)
    abstract fun writeTo(stream: java.io.OutputStream)

    companion object {
        /**
         * Read ClientMessage from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): ClientMessage {
            when (StreamUtil.readInt(stream)) {
                DebugMessage.TAG -> return DebugMessage.readFrom(stream)
                ActionMessage.TAG -> return ActionMessage.readFrom(stream)
                DebugUpdateDone.TAG -> return DebugUpdateDone.readFrom(stream)
                RequestDebugState.TAG -> return RequestDebugState.readFrom(stream)
                else -> throw java.io.IOException("Unexpected tag value")
            }
        }
    }

    /**
     * Ask app to perform new debug command
     */
    class DebugMessage : ClientMessage {
        /**
         * Command to perform
         */
        var command: trans_gen_test.codegame.DebugCommand
    
        constructor(command: trans_gen_test.codegame.DebugCommand) {
            this.command = command
        }
    
        /**
         * Write DebugMessage to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            command.writeTo(stream)
        }
    
        /**
         * Get string representation of DebugMessage
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("DebugMessage { ")
            stringBuilder.append("command: ")
            stringBuilder.append(command)
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 0
    
            /**
             * Read DebugMessage from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): DebugMessage {
                var command: trans_gen_test.codegame.DebugCommand
                command = trans_gen_test.codegame.DebugCommand.readFrom(stream)
                return DebugMessage(command)
            }
        }
    }

    /**
     * Reply for ServerMessage::GetAction
     */
    class ActionMessage : ClientMessage {
        /**
         * Player's action
         */
        var action: trans_gen_test.model.Action
    
        constructor(action: trans_gen_test.model.Action) {
            this.action = action
        }
    
        /**
         * Write ActionMessage to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            action.writeTo(stream)
        }
    
        /**
         * Get string representation of ActionMessage
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("ActionMessage { ")
            stringBuilder.append("action: ")
            stringBuilder.append(action)
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 1
    
            /**
             * Read ActionMessage from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): ActionMessage {
                var action: trans_gen_test.model.Action
                action = trans_gen_test.model.Action.readFrom(stream)
                return ActionMessage(action)
            }
        }
    }

    /**
     * Signifies finish of the debug update
     */
    class DebugUpdateDone : ClientMessage {
    
        constructor() {
        }
    
        /**
         * Write DebugUpdateDone to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
        }
    
        /**
         * Get string representation of DebugUpdateDone
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("DebugUpdateDone { ")
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 2
    
            /**
             * Read DebugUpdateDone from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): DebugUpdateDone {
                return DebugUpdateDone()
            }
        }
    }

    /**
     * Request debug state from the app
     */
    class RequestDebugState : ClientMessage {
    
        constructor() {
        }
    
        /**
         * Write RequestDebugState to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
        }
    
        /**
         * Get string representation of RequestDebugState
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("RequestDebugState { ")
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 3
    
            /**
             * Read RequestDebugState from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): RequestDebugState {
                return RequestDebugState()
            }
        }
    }
}