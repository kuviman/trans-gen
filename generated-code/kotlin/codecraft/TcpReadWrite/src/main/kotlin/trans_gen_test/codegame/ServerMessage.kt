package trans_gen_test.codegame

import trans_gen_test.util.StreamUtil

/**
 * Message sent from server
 */
abstract class ServerMessage {
    /**
     * Write ServerMessage to output stream
     */
    @Throws(java.io.IOException::class)
    abstract fun writeTo(stream: java.io.OutputStream)

    companion object {
        /**
         * Read ServerMessage from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): ServerMessage {
            when (StreamUtil.readInt(stream)) {
                GetAction.TAG -> return GetAction.readFrom(stream)
                Finish.TAG -> return Finish.readFrom(stream)
                DebugUpdate.TAG -> return DebugUpdate.readFrom(stream)
                else -> throw java.io.IOException("Unexpected tag value")
            }
        }
    }

    /**
     * Get action for next tick
     */
    class GetAction : ServerMessage {
        /**
         * Player's view
         */
        var playerView: trans_gen_test.model.PlayerView
        /**
         * Whether app is running with debug interface available
         */
        var debugAvailable: Boolean
    
        constructor(playerView: trans_gen_test.model.PlayerView, debugAvailable: Boolean) {
            this.playerView = playerView
            this.debugAvailable = debugAvailable
        }
    
        /**
         * Write GetAction to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            playerView.writeTo(stream)
            StreamUtil.writeBoolean(stream, debugAvailable)
        }
    
        /**
         * Get string representation of GetAction
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("GetAction { ")
            stringBuilder.append("playerView: ")
            stringBuilder.append(playerView)
            stringBuilder.append(", ")
            stringBuilder.append("debugAvailable: ")
            stringBuilder.append(debugAvailable)
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 0
    
            /**
             * Read GetAction from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): GetAction {
                var playerView: trans_gen_test.model.PlayerView
                playerView = trans_gen_test.model.PlayerView.readFrom(stream)
                var debugAvailable: Boolean
                debugAvailable = StreamUtil.readBoolean(stream)
                return GetAction(playerView, debugAvailable)
            }
        }
    }

    /**
     * Signifies end of the game
     */
    class Finish : ServerMessage {
    
        constructor() {
        }
    
        /**
         * Write Finish to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
        }
    
        /**
         * Get string representation of Finish
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("Finish { ")
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 1
    
            /**
             * Read Finish from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): Finish {
                return Finish()
            }
        }
    }

    /**
     * Debug update
     */
    class DebugUpdate : ServerMessage {
        /**
         * Player's view
         */
        var playerView: trans_gen_test.model.PlayerView
    
        constructor(playerView: trans_gen_test.model.PlayerView) {
            this.playerView = playerView
        }
    
        /**
         * Write DebugUpdate to output stream
         */
        @Throws(java.io.IOException::class)
        override fun writeTo(stream: java.io.OutputStream) {
            StreamUtil.writeInt(stream, TAG)
            playerView.writeTo(stream)
        }
    
        /**
         * Get string representation of DebugUpdate
         */
        override fun toString(): String {
            var stringBuilder = StringBuilder("DebugUpdate { ")
            stringBuilder.append("playerView: ")
            stringBuilder.append(playerView)
            stringBuilder.append(" }")
            return stringBuilder.toString()
        }
    
        companion object {
            val TAG = 2
    
            /**
             * Read DebugUpdate from input stream
             */
            @Throws(java.io.IOException::class)
            fun readFrom(stream: java.io.InputStream): DebugUpdate {
                var playerView: trans_gen_test.model.PlayerView
                playerView = trans_gen_test.model.PlayerView.readFrom(stream)
                return DebugUpdate(playerView)
            }
        }
    }
}