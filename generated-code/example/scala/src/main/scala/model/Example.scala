package model

import util.StreamUtil

case class Example(oneOf: model.OneOf, hashMap: Map[model.Enumeration, Int], optionalInt: Option[Int], optionalBoolean: Option[Boolean], optionalOneOf: Option[model.OneOf], optionalStruct: Option[model.Structure], optionalEnum: Option[model.Enumeration]) {
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
    def readFrom(stream: java.io.InputStream): Example = Example(
        model.OneOf.readFrom(stream),
        (0 until StreamUtil.readInt(stream)).map { _ => (
            model.Enumeration.readFrom(stream),
            StreamUtil.readInt(stream)
        )}.toMap,
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readInt(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readBoolean(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            model.OneOf.readFrom(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            model.Structure.readFrom(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            model.Enumeration.readFrom(stream)
        ) else None
    )
}