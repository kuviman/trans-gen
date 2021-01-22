package model

import util.StreamUtil

case class EntityProperties(size: Int, buildScore: Int, destroyScore: Int, canMove: Boolean, populationProvide: Int, populationUse: Int, maxHealth: Int, initialCost: Int, sightRange: Int, resourcePerHealth: Int, build: Option[model.BuildProperties], attack: Option[model.AttackProperties], repair: Option[model.RepairProperties]) {
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, size)
        StreamUtil.writeInt(stream, buildScore)
        StreamUtil.writeInt(stream, destroyScore)
        StreamUtil.writeBoolean(stream, canMove)
        StreamUtil.writeInt(stream, populationProvide)
        StreamUtil.writeInt(stream, populationUse)
        StreamUtil.writeInt(stream, maxHealth)
        StreamUtil.writeInt(stream, initialCost)
        StreamUtil.writeInt(stream, sightRange)
        StreamUtil.writeInt(stream, resourcePerHealth)
        build match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
        attack match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
        repair match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                value.writeTo(stream)
            }
        }
    }

    override def toString(): String = {
        var stringBuilder = new StringBuilder("EntityProperties { ")
        stringBuilder.append("size: ")
        stringBuilder.append(size)
        stringBuilder.append(", ")
        stringBuilder.append("buildScore: ")
        stringBuilder.append(buildScore)
        stringBuilder.append(", ")
        stringBuilder.append("destroyScore: ")
        stringBuilder.append(destroyScore)
        stringBuilder.append(", ")
        stringBuilder.append("canMove: ")
        stringBuilder.append(canMove)
        stringBuilder.append(", ")
        stringBuilder.append("populationProvide: ")
        stringBuilder.append(populationProvide)
        stringBuilder.append(", ")
        stringBuilder.append("populationUse: ")
        stringBuilder.append(populationUse)
        stringBuilder.append(", ")
        stringBuilder.append("maxHealth: ")
        stringBuilder.append(maxHealth)
        stringBuilder.append(", ")
        stringBuilder.append("initialCost: ")
        stringBuilder.append(initialCost)
        stringBuilder.append(", ")
        stringBuilder.append("sightRange: ")
        stringBuilder.append(sightRange)
        stringBuilder.append(", ")
        stringBuilder.append("resourcePerHealth: ")
        stringBuilder.append(resourcePerHealth)
        stringBuilder.append(", ")
        stringBuilder.append("build: ")
        stringBuilder.append(build)
        stringBuilder.append(", ")
        stringBuilder.append("attack: ")
        stringBuilder.append(attack)
        stringBuilder.append(", ")
        stringBuilder.append("repair: ")
        stringBuilder.append(repair)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object EntityProperties {
    def readFrom(stream: java.io.InputStream): EntityProperties = EntityProperties(
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readBoolean(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        StreamUtil.readInt(stream),
        if (StreamUtil.readBoolean(stream)) Some(
            model.BuildProperties.readFrom(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            model.AttackProperties.readFrom(stream)
        ) else None,
        if (StreamUtil.readBoolean(stream)) Some(
            model.RepairProperties.readFrom(stream)
        ) else None
    )
}