/// Example
public struct Example {
    /// OneOf
    let oneOf: OneOf

    /// Dictionary
    let hashMap: [Enumeration: Int32]

    /// Optional int
    let optionalInt: Int32?

    /// Optional boolean
    let optionalBool: Bool?

    /// Optional OneOf
    let optionalOneOf: OneOf?

    /// Optional struct
    let optionalStruct: Structure?

    /// Optional enum
    let optionalEnum: Enumeration?

    /// Read Example from input stream
    static func readFrom<S: InputStream>(_ stream: S) -> Example {
        var oneOf: OneOf
        oneOf = OneOf.readFrom(stream)
        var hashMap: [Enumeration: Int32]
        let hashMapSize = stream.readInt32()
        hashMap = [:]
        for _ in 0..<hashMapSize {
            let hashMapKey: Enumeration
            let hashMapValue: Int32
            hashMapKey = Enumeration.readFrom(stream)
            hashMapValue = stream.readInt32()
            hashMap[hashMapKey] = hashMapValue
        }
        var optionalInt: Int32?
        if stream.readBool() {
            optionalInt = stream.readInt32()
        } else {
            optionalInt = nil
        }
        var optionalBool: Bool?
        if stream.readBool() {
            optionalBool = stream.readBool()
        } else {
            optionalBool = nil
        }
        var optionalOneOf: OneOf?
        if stream.readBool() {
            optionalOneOf = OneOf.readFrom(stream)
        } else {
            optionalOneOf = nil
        }
        var optionalStruct: Structure?
        if stream.readBool() {
            optionalStruct = Structure.readFrom(stream)
        } else {
            optionalStruct = nil
        }
        var optionalEnum: Enumeration?
        if stream.readBool() {
            optionalEnum = Enumeration.readFrom(stream)
        } else {
            optionalEnum = nil
        }
        return Example(oneOf: oneOf, hashMap: hashMap, optionalInt: optionalInt, optionalBool: optionalBool, optionalOneOf: optionalOneOf, optionalStruct: optionalStruct, optionalEnum: optionalEnum)
    }

    /// Write Example to output stream
    func writeTo<S: OutputStream>(_ stream: S) {
        oneOf.writeTo(stream)
        stream.writeInt32(Int32(hashMap.count))
        for (hashMapKey, hashMapValue) in hashMap {
            hashMapKey.writeTo(stream)
            stream.writeInt32(hashMapValue)
        }
        if optionalInt == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let optionalIntValue = optionalInt!
            stream.writeInt32(optionalIntValue)
        }
        if optionalBool == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let optionalBoolValue = optionalBool!
            stream.writeBool(optionalBoolValue)
        }
        if optionalOneOf == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let optionalOneOfValue = optionalOneOf!
            optionalOneOfValue.writeTo(stream)
        }
        if optionalStruct == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let optionalStructValue = optionalStruct!
            optionalStructValue.writeTo(stream)
        }
        if optionalEnum == nil {
            stream.writeBool(false)
        } else {
            stream.writeBool(true)
            let optionalEnumValue = optionalEnum!
            optionalEnumValue.writeTo(stream)
        }
    }
}