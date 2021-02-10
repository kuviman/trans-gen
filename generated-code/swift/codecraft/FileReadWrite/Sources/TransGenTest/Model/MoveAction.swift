/// Move action
public struct MoveAction {
    /// Target position
    let target: Vec2Int

    /// Whether to try find closest position, if path to target is not found
    let findClosestPosition: Bool

    /// Whether to destroy other entities on the way
    let breakThrough: Bool

    /// Read MoveAction from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> MoveAction {
        var target: Vec2Int
        target = Vec2Int.readFrom(stream)
        var findClosestPosition: Bool
        findClosestPosition = stream.readBool()
        var breakThrough: Bool
        breakThrough = stream.readBool()
        return MoveAction(target: target, findClosestPosition: findClosestPosition, breakThrough: breakThrough)
    }

    /// Write MoveAction to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        target.writeTo(stream)
        stream.writeBool(findClosestPosition)
        stream.writeBool(breakThrough)
    }
}