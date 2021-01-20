package model

import util.StreamUtil

class Structure {
    lateinit var oneOfOne: model.OneOf
    lateinit var oneOfTwo: model.OneOf
    lateinit var hashMap: MutableMap<model.Enumeration, Int>
    lateinit var text: String
    var floatNumber: Float = 0.0f
    var doubleNumber: Double = 0.0

    constructor(oneOfOne: model.OneOf, oneOfTwo: model.OneOf, hashMap: MutableMap<model.Enumeration, Int>, text: String, floatNumber: Float, doubleNumber: Double) {
        this.oneOfOne = oneOfOne
        this.oneOfTwo = oneOfTwo
        this.hashMap = hashMap
        this.text = text
        this.floatNumber = floatNumber
        this.doubleNumber = doubleNumber
    }

    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        oneOfOne.writeTo(stream)
        oneOfTwo.writeTo(stream)
        StreamUtil.writeInt(stream, hashMap.size)
        for (hashMapEntry in hashMap) {
            val hashMapKey = hashMapEntry.key
            StreamUtil.writeInt(stream, hashMapKey.tag)
            val hashMapValue = hashMapEntry.value
            StreamUtil.writeInt(stream, hashMapValue)
        }
        StreamUtil.writeString(stream, text)
        StreamUtil.writeFloat(stream, floatNumber)
        StreamUtil.writeDouble(stream, doubleNumber)
    }

    companion object {
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Structure {
            var oneOfOne: model.OneOf
            oneOfOne = model.OneOf.readFrom(stream)
            var oneOfTwo: model.OneOf
            oneOfTwo = model.OneOf.readFrom(stream)
            var hashMap: MutableMap<model.Enumeration, Int>
            val hashMapSize = StreamUtil.readInt(stream)
            hashMap = mutableMapOf();
            for (hashMapIndex in 0 until hashMapSize) {
                var hashMapKey: model.Enumeration
                when (StreamUtil.readInt(stream)) {
                0 -> hashMapKey = model.Enumeration.VALUE_ONE
                1 -> hashMapKey = model.Enumeration.VALUE_TWO
                else -> throw java.io.IOException("Unexpected tag value")
                }
                var hashMapValue: Int
                hashMapValue = StreamUtil.readInt(stream)
                hashMap.put(hashMapKey, hashMapValue)
            }
            var text: String
            text = StreamUtil.readString(stream)
            var floatNumber: Float
            floatNumber = StreamUtil.readFloat(stream)
            var doubleNumber: Double
            doubleNumber = StreamUtil.readDouble(stream)
            return Structure(oneOfOne, oneOfTwo, hashMap, text, floatNumber, doubleNumber)
        }
    }
}