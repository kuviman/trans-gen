/// Message sent from client
enum ClientMessage {
    /// Ask app to perform new debug command
    ///
    /// - command: Command to perform
    case debugMessage(command: DebugCommand)

    /// Reply for ServerMessage::GetAction
    ///
    /// - action: Player's action
    case actionMessage(action: Action)

    /// Signifies finish of the debug update
    ///
    case debugUpdateDone

    /// Request debug state from the app
    ///
    case requestDebugState

    /// Read ClientMessage from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> ClientMessage {
        switch stream.readInt32() {
            case 0:
                var command: DebugCommand
                command = DebugCommand.readFrom(stream)
                return ClientMessage.debugMessage(command: command)
            case 1:
                var action: Action
                action = Action.readFrom(stream)
                return ClientMessage.actionMessage(action: action)
            case 2:
                return ClientMessage.debugUpdateDone
            case 3:
                return ClientMessage.requestDebugState
            default:
                fatalError("Unexpected tag value")
        }
    }

    /// Write ClientMessage to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        switch self {
            case let .debugMessage(command):
                stream.writeInt32(0)
                command.writeTo(stream)
            case let .actionMessage(action):
                stream.writeInt32(1)
                action.writeTo(stream)
            case .debugUpdateDone:
                stream.writeInt32(2)
            case .requestDebugState:
                stream.writeInt32(3)
        }
    }
}