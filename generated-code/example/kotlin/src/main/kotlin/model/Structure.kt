package model

import util.StreamUtil

class Structure {
    lateinit var oneOfOne: model.OneOf
    lateinit var oneOfTwo: model.OneOf
    lateinit var hashMap: MutableMap<model.Enumeration, Int>
    lateinit var text: String
    var realNumber: Double = 0.0
    constructor() {}
    constructor(oneOfOne: model.OneOf, oneOfTwo: model.OneOf, hashMap: MutableMap<model.Enumeration, Int>, text: String, realNumber: Double) {
        this.oneOfOne = oneOfOne
        this.oneOfTwo = oneOfTwo
        this.hashMap = hashMap
        this.text = text
        this.realNumber = realNumber
    }
    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Structure {
            val result = Structure()
            result.oneOfOne = model.OneOf.readFrom(stream)
            result.oneOfTwo = model.OneOf.readFrom(stream)
            val hashMapSize = StreamUtil.readInt(stream)
            result.hashMap = mutableMapOf()
            for (i in 0 until hashMapSize) {
                var hashMapKey: model.Enumeration
                when (StreamUtil.readInt(stream)) {
                0 ->hashMapKey = model.Enumeration.VALUE_ONE
                1 ->hashMapKey = model.Enumeration.VALUE_TWO
                else -> throw java.io.IOException("Unexpected tag value")
                }
                var hashMapValue: Int
                hashMapValue = StreamUtil.readInt(stream)
                result.hashMap.put(hashMapKey, hashMapValue)
            }
            result.text = StreamUtil.readString(stream)
            result.realNumber = StreamUtil.readDouble(stream)
            return result
        }
    }
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        oneOfOne.writeTo(stream)
        oneOfTwo.writeTo(stream)
        StreamUtil.writeInt(stream, hashMap.size)
        for (hashMapEntry in hashMap) {
            StreamUtil.writeInt(stream, hashMapEntry.key.tag)
            StreamUtil.writeInt(stream, hashMapEntry.value)
        }
        StreamUtil.writeString(stream, text)
        StreamUtil.writeDouble(stream, realNumber)
    }
}
