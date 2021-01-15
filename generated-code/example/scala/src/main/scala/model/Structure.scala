package model

import util.StreamUtil

case class Structure(oneOfOne: model.OneOf, oneOfTwo: model.OneOf, hashMap: Map[model.Enumeration, Int], text: String, realNumber: Double) {
    def writeTo(stream: java.io.OutputStream) {
        oneOfOne.writeTo(stream)
        oneOfTwo.writeTo(stream)
        StreamUtil.writeInt(stream, hashMap.size)
        hashMap.foreach { case (key, value) =>
            key.writeTo(stream)
            StreamUtil.writeInt(stream, value)
        }
        StreamUtil.writeString(stream, text)
        StreamUtil.writeDouble(stream, realNumber)
    }
}
object Structure {
    def readFrom(stream: java.io.InputStream): Structure = Structure(
        model.OneOf.readFrom(stream)
        ,
        model.OneOf.readFrom(stream)
        ,
        (0 until StreamUtil.readInt(stream)).map { _ => (
            model.Enumeration.readFrom(stream)
            ,
            StreamUtil.readInt(stream)
        )}.toMap
        ,
        StreamUtil.readString(stream)
        ,
        StreamUtil.readDouble(stream)
        )
}
