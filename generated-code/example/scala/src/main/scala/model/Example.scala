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