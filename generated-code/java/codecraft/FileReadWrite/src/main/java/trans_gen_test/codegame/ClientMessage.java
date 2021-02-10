package trans_gen_test.codegame;

import trans_gen_test.util.StreamUtil;

/**
 * Message sent from client
 */
public abstract class ClientMessage {
    /**
     * Write ClientMessage to output stream
     */
    public abstract void writeTo(java.io.OutputStream stream) throws java.io.IOException;

    /**
     * Read ClientMessage from input stream
     */
    public static ClientMessage readFrom(java.io.InputStream stream) throws java.io.IOException {
        switch (StreamUtil.readInt(stream)) {
            case DebugMessage.TAG:
                return DebugMessage.readFrom(stream);
            case ActionMessage.TAG:
                return ActionMessage.readFrom(stream);
            case DebugUpdateDone.TAG:
                return DebugUpdateDone.readFrom(stream);
            case RequestDebugState.TAG:
                return RequestDebugState.readFrom(stream);
            default:
                throw new java.io.IOException("Unexpected tag value");
        }
    }

    /**
     * Ask app to perform new debug command
     */
    public static class DebugMessage extends ClientMessage {
        public static final int TAG = 0;
    
        /**
         * Command to perform
         */
        private trans_gen_test.codegame.DebugCommand command;
    
        /**
         * Command to perform
         */
        public trans_gen_test.codegame.DebugCommand getCommand() {
            return command;
        }
    
        /**
         * Command to perform
         */
        public void setCommand(trans_gen_test.codegame.DebugCommand value) {
            this.command = value;
        }
    
        public DebugMessage(trans_gen_test.codegame.DebugCommand command) {
            this.command = command;
        }
    
        /**
         * Read DebugMessage from input stream
         */
        public static DebugMessage readFrom(java.io.InputStream stream) throws java.io.IOException {
            trans_gen_test.codegame.DebugCommand command;
            command = trans_gen_test.codegame.DebugCommand.readFrom(stream);
            return new DebugMessage(command);
        }
    
        /**
         * Write DebugMessage to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            command.writeTo(stream);
        }
    
        /**
         * Get string representation of DebugMessage
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("DebugMessage { ");
            stringBuilder.append("command: ");
            stringBuilder.append(String.valueOf(command));
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }

    /**
     * Reply for ServerMessage::GetAction
     */
    public static class ActionMessage extends ClientMessage {
        public static final int TAG = 1;
    
        /**
         * Player's action
         */
        private trans_gen_test.model.Action action;
    
        /**
         * Player's action
         */
        public trans_gen_test.model.Action getAction() {
            return action;
        }
    
        /**
         * Player's action
         */
        public void setAction(trans_gen_test.model.Action value) {
            this.action = value;
        }
    
        public ActionMessage(trans_gen_test.model.Action action) {
            this.action = action;
        }
    
        /**
         * Read ActionMessage from input stream
         */
        public static ActionMessage readFrom(java.io.InputStream stream) throws java.io.IOException {
            trans_gen_test.model.Action action;
            action = trans_gen_test.model.Action.readFrom(stream);
            return new ActionMessage(action);
        }
    
        /**
         * Write ActionMessage to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
            action.writeTo(stream);
        }
    
        /**
         * Get string representation of ActionMessage
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("ActionMessage { ");
            stringBuilder.append("action: ");
            stringBuilder.append(String.valueOf(action));
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }

    /**
     * Signifies finish of the debug update
     */
    public static class DebugUpdateDone extends ClientMessage {
        public static final int TAG = 2;
    
    
        public DebugUpdateDone() {
        }
    
        /**
         * Read DebugUpdateDone from input stream
         */
        public static DebugUpdateDone readFrom(java.io.InputStream stream) throws java.io.IOException {
            return new DebugUpdateDone();
        }
    
        /**
         * Write DebugUpdateDone to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
        }
    
        /**
         * Get string representation of DebugUpdateDone
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("DebugUpdateDone { ");
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }

    /**
     * Request debug state from the app
     */
    public static class RequestDebugState extends ClientMessage {
        public static final int TAG = 3;
    
    
        public RequestDebugState() {
        }
    
        /**
         * Read RequestDebugState from input stream
         */
        public static RequestDebugState readFrom(java.io.InputStream stream) throws java.io.IOException {
            return new RequestDebugState();
        }
    
        /**
         * Write RequestDebugState to output stream
         */
        @Override
        public void writeTo(java.io.OutputStream stream) throws java.io.IOException {
            StreamUtil.writeInt(stream, TAG);
        }
    
        /**
         * Get string representation of RequestDebugState
         */
        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder("RequestDebugState { ");
            stringBuilder.append(" }");
            return stringBuilder.toString();
        }
    }
}