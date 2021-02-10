package trans_gen_test

import trans_gen_test.util.StreamUtil

/**
 * Example
 */
class Example {
    /**
     * OneOf
     */
    var oneOf: trans_gen_test.OneOf
    /**
     * Dictionary
     */
    var hashMap: MutableMap<trans_gen_test.Enumeration, Int>
    /**
     * Optional int
     */
    var optionalInt: Int?
    /**
     * Optional boolean
     */
    var optionalBoolean: Boolean?
    /**
     * Optional OneOf
     */
    var optionalOneOf: trans_gen_test.OneOf?
    /**
     * Optional struct
     */
    var optionalStruct: trans_gen_test.Structure?
    /**
     * Optional enum
     */
    var optionalEnum: trans_gen_test.Enumeration?

    constructor(oneOf: trans_gen_test.OneOf, hashMap: MutableMap<trans_gen_test.Enumeration, Int>, optionalInt: Int?, optionalBoolean: Boolean?, optionalOneOf: trans_gen_test.OneOf?, optionalStruct: trans_gen_test.Structure?, optionalEnum: trans_gen_test.Enumeration?) {
        this.oneOf = oneOf
        this.hashMap = hashMap
        this.optionalInt = optionalInt
        this.optionalBoolean = optionalBoolean
        this.optionalOneOf = optionalOneOf
        this.optionalStruct = optionalStruct
        this.optionalEnum = optionalEnum
    }

    /**
     * Write Example to output stream
     */
    @Throws(java.io.IOException::class)
    fun writeTo(stream: java.io.OutputStream) {
        oneOf.writeTo(stream)
        StreamUtil.writeInt(stream, hashMap.size)
        for (hashMapEntry in hashMap) {
            val hashMapKey = hashMapEntry.key
            StreamUtil.writeInt(stream, hashMapKey.tag)
            val hashMapValue = hashMapEntry.value
            StreamUtil.writeInt(stream, hashMapValue)
        }
        val optionalIntValue = optionalInt
        if (optionalIntValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            StreamUtil.writeInt(stream, optionalIntValue)
        }
        val optionalBooleanValue = optionalBoolean
        if (optionalBooleanValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            StreamUtil.writeBoolean(stream, optionalBooleanValue)
        }
        val optionalOneOfValue = optionalOneOf
        if (optionalOneOfValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            optionalOneOfValue.writeTo(stream)
        }
        val optionalStructValue = optionalStruct
        if (optionalStructValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            optionalStructValue.writeTo(stream)
        }
        val optionalEnumValue = optionalEnum
        if (optionalEnumValue == null) {
            StreamUtil.writeBoolean(stream, false)
        } else {
            StreamUtil.writeBoolean(stream, true)
            StreamUtil.writeInt(stream, optionalEnumValue.tag)
        }
    }

    /**
     * Get string representation of Example
     */
    override fun toString(): String {
        var stringBuilder = StringBuilder("Example { ")
        stringBuilder.append("oneOf: ")
        stringBuilder.append(oneOf)
        stringBuilder.append(", ")
        stringBuilder.append("hashMap: ")
        stringBuilder.append(hashMap)
        stringBuilder.append(", ")
        stringBuilder.append("optionalInt: ")
        stringBuilder.append(optionalInt)
        stringBuilder.append(", ")
        stringBuilder.append("optionalBoolean: ")
        stringBuilder.append(optionalBoolean)
        stringBuilder.append(", ")
        stringBuilder.append("optionalOneOf: ")
        stringBuilder.append(optionalOneOf)
        stringBuilder.append(", ")
        stringBuilder.append("optionalStruct: ")
        stringBuilder.append(optionalStruct)
        stringBuilder.append(", ")
        stringBuilder.append("optionalEnum: ")
        stringBuilder.append(optionalEnum)
        stringBuilder.append(" }")
        return stringBuilder.toString()
    }

    companion object {
        /**
         * Read Example from input stream
         */
        @Throws(java.io.IOException::class)
        fun readFrom(stream: java.io.InputStream): Example {
            var oneOf: trans_gen_test.OneOf
            oneOf = trans_gen_test.OneOf.readFrom(stream)
            var hashMap: MutableMap<trans_gen_test.Enumeration, Int>
            val hashMapSize = StreamUtil.readInt(stream)
            hashMap = mutableMapOf();
            for (hashMapIndex in 0 until hashMapSize) {
                var hashMapKey: trans_gen_test.Enumeration
                hashMapKey = trans_gen_test.Enumeration.readFrom(stream)
                var hashMapValue: Int
                hashMapValue = StreamUtil.readInt(stream)
                hashMap.put(hashMapKey, hashMapValue)
            }
            var optionalInt: Int?
            if (StreamUtil.readBoolean(stream)) {
                optionalInt = StreamUtil.readInt(stream)
            } else {
                optionalInt = null
            }
            var optionalBoolean: Boolean?
            if (StreamUtil.readBoolean(stream)) {
                optionalBoolean = StreamUtil.readBoolean(stream)
            } else {
                optionalBoolean = null
            }
            var optionalOneOf: trans_gen_test.OneOf?
            if (StreamUtil.readBoolean(stream)) {
                optionalOneOf = trans_gen_test.OneOf.readFrom(stream)
            } else {
                optionalOneOf = null
            }
            var optionalStruct: trans_gen_test.Structure?
            if (StreamUtil.readBoolean(stream)) {
                optionalStruct = trans_gen_test.Structure.readFrom(stream)
            } else {
                optionalStruct = null
            }
            var optionalEnum: trans_gen_test.Enumeration?
            if (StreamUtil.readBoolean(stream)) {
                optionalEnum = trans_gen_test.Enumeration.readFrom(stream)
            } else {
                optionalEnum = null
            }
            return Example(oneOf, hashMap, optionalInt, optionalBoolean, optionalOneOf, optionalStruct, optionalEnum)
        }
    }
}