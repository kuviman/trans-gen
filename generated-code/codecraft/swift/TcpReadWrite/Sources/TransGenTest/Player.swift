/// Player (strategy, client)
public struct Player {
    /// Player's ID
    let id: Int32

    /// Current score
    let score: Int32

    /// Current amount of resource
    let resource: Int32

    /// Read Player from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> Player {
        var id: Int32
        id = stream.readInt32()
        var score: Int32
        score = stream.readInt32()
        var resource: Int32
        resource = stream.readInt32()
        return Player(id: id, score: score, resource: resource)
    }

    /// Write Player to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(id)
        stream.writeInt32(score)
        stream.writeInt32(resource)
    }
}