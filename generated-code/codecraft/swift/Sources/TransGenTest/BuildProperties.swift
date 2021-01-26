public struct BuildProperties {
    let options: [EntityType]
    let initHealth: Int32?

    static func readFrom<S: InputStream>(_ stream: S) -> BuildProperties {
        var options: [EntityType]
        let optionsSize = stream.readInt32()
        options = (0..<optionsSize).map{ _ in
            var optionsSize: EntityType
            optionsSize = EntityType.readFrom(stream)
            return optionsSize
        }
        var initHealth: Int32?
        if stream.readBool() {
            initHealth = stream.readInt32()
        } else {
            initHealth = nil
        }
        return BuildProperties(options: options, initHealth: initHealth)
    }

    func writeTo<S: OutputStream>(_ stream: S) {
        stream.writeInt32(Int32(options.count))
        for optionsElement in options {
            optionsElement.writeTo(stream)
        }
        if initHealth == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let initHealthValue = initHealth!
            stream.writeInt32(initHealthValue)
        }
    }
}