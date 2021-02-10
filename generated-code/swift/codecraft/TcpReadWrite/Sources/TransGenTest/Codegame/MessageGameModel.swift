/// Client or server message
enum MessageGameModel {
    /// Client message
    ///
    /// - message: Message
    case client(message: ClientMessage)

    /// Server message
    ///
    /// - message: Message
    case server(message: ServerMessage)

    /// Read MessageGameModel from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> MessageGameModel {
        switch stream.readInt32() {
            case 0:
                var message: ClientMessage
                message = ClientMessage.readFrom(stream)
                return MessageGameModel.client(message: message)
            case 1:
                var message: ServerMessage
                message = ServerMessage.readFrom(stream)
                return MessageGameModel.server(message: message)
            default:
                fatalError("Unexpected tag value")
        }
    }

    /// Write MessageGameModel to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        switch self {
            case let .client(message):
                stream.writeInt32(0)
                message.writeTo(stream)
            case let .server(message):
                stream.writeInt32(1)
                message.writeTo(stream)
        }
    }
}