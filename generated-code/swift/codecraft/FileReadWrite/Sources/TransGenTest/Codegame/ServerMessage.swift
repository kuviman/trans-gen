/// Message sent from server
enum ServerMessage {
    /// Get action for next tick
    ///
    /// - playerView: Player's view
    /// - debugAvailable: Whether app is running with debug interface available
    case getAction(playerView: PlayerView, debugAvailable: Bool)

    /// Signifies end of the game
    ///
    case finish

    /// Debug update
    ///
    /// - playerView: Player's view
    case debugUpdate(playerView: PlayerView)

    /// Read ServerMessage from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> ServerMessage {
        switch stream.readInt32() {
            case 0:
                var playerView: PlayerView
                playerView = PlayerView.readFrom(stream)
                var debugAvailable: Bool
                debugAvailable = stream.readBool()
                return ServerMessage.getAction(playerView: playerView, debugAvailable: debugAvailable)
            case 1:
                return ServerMessage.finish
            case 2:
                var playerView: PlayerView
                playerView = PlayerView.readFrom(stream)
                return ServerMessage.debugUpdate(playerView: playerView)
            default:
                fatalError("Unexpected tag value")
        }
    }

    /// Write ServerMessage to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        switch self {
            case let .getAction(playerView, debugAvailable):
                stream.writeInt32(0)
                playerView.writeTo(stream)
                stream.writeBool(debugAvailable)
            case .finish:
                stream.writeInt32(1)
            case let .debugUpdate(playerView):
                stream.writeInt32(2)
                playerView.writeTo(stream)
        }
    }
}