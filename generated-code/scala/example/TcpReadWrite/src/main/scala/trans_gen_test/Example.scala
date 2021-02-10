package trans_gen_test

import trans_gen_test.util.StreamUtil

/**
 * Example
 *
 * @param oneOf OneOf
 * @param hashMap Dictionary
 * @param optionalInt Optional int
 * @param optionalBoolean Optional boolean
 * @param optionalOneOf Optional OneOf
 * @param optionalStruct Optional struct
 * @param optionalEnum Optional enum
 */
case class Example(oneOf: trans_gen_test.OneOf, hashMap: Map[trans_gen_test.Enumeration, Int], optionalInt: Option[Int], optionalBoolean: Option[Boolean], optionalOneOf: Option[trans_gen_test.OneOf], optionalStruct: Option[trans_gen_test.Structure], optionalEnum: Option[trans_gen_test.Enumeration]) {
    /**
     * Write Example to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        oneOf.writeTo(stream)
        StreamUtil.writeInt(stream, hashMap.size)
        hashMap.foreach { case (key, value) =>
            key.writeTo(stream)
            StreamUtil.writeInt(stream, value)
        }
        optionalInt match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                StreamUtil.writeInt(stream, value)
            }
        }
        optionalBoolean match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                StreamUtil.writeBoolean(stream, value)
            }
        }
        optionalOneOf match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
        optionalStruct match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
        optionalEnum match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
    }

    /**
     * Get string representation of Example
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("Example { ")
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
        stringBuilder.toString()
    }
}

object Example {
    /**
     * Read Example from input stream
     */
    def readFrom(stream: java.io.InputStream): Example = Example(
        trans_gen_test.OneOf.readFrom(stream),
        (0 until StreamUtil.readInt(stream)).map { _ => (
            trans_gen_test.Enumeration.readFrom(stream),
            StreamUtil.readInt(stream)
        )}.toMap,
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readInt(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readBoolean(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            trans_gen_test.OneOf.readFrom(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            trans_gen_test.Structure.readFrom(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            trans_gen_test.Enumeration.readFrom(stream)
        ) else None
    )
}