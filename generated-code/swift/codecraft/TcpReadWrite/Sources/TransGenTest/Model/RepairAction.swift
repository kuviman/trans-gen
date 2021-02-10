/// Repair action
public struct RepairAction {
    /// Target entity's ID
    let target: Int32

    /// Read RepairAction from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> RepairAction {
        var target: Int32
        target = stream.readInt32()
        return RepairAction(target: target)
    }

    /// Write RepairAction to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(target)
    }
}