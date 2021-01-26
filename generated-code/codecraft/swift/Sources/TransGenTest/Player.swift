public struct Player {
    let id: Int32
    let score: Int32
    let resource: Int32

    static func readFrom<S: InputStream>(_ stream: S) -> Player {
        var id: Int32
        id = stream.readInt32()
        var score: Int32
        score = stream.readInt32()
        var resource: Int32
        resource = stream.readInt32()
        return Player(id: id, score: score, resource: resource)
    }

    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(id)
        stream.writeInt32(score)
        stream.writeInt32(resource)
    }
}