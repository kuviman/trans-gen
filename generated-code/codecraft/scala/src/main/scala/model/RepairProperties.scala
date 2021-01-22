package model

import util.StreamUtil

case class RepairProperties(validTargets: Seq[model.EntityType], power: Int) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, validTargets.length)
        validTargets.foreach { value =>
            value.writeTo(stream)
        }
        StreamUtil.writeInt(stream, power)
    }

    override def toString(): String = {
        var stringBuilder = new StringBuilder("RepairProperties { ")
        stringBuilder.append("validTargets: ")
        stringBuilder.append(validTargets)
        stringBuilder.append(", ")
        stringBuilder.append("power: ")
        stringBuilder.append(power)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object RepairProperties {
    def readFrom(stream: java.io.InputStream): RepairProperties = RepairProperties(
        (0 until StreamUtil.readInt(stream)).map { _ =>
            model.EntityType.readFrom(stream)
        },
        StreamUtil.readInt(stream)
    )
}