package trans_gen_test.model

import trans_gen_test.util.StreamUtil

/**
 * Entity's build properties
 *
 * @param options Valid new entity types
 * @param initHealth Initial health of new entity. If absent, it will have full health
 */
case class BuildProperties(options: Seq[trans_gen_test.model.EntityType], initHealth: Option[Int]) {
    /**
     * Write BuildProperties to output stream
     */
    def writeTo(stream: java.io.OutputStream) {
        StreamUtil.writeInt(stream, options.length)
        options.foreach { value =>
            value.writeTo(stream)
        }
        initHealth match {
            case None => StreamUtil.writeBoolean(stream, false)
            case Some(value) => {
                StreamUtil.writeBoolean(stream, true)
                StreamUtil.writeInt(stream, value)
            }
        }
    }

    /**
     * Get string representation of BuildProperties
     */
    override def toString(): String = {
        var stringBuilder = new StringBuilder("BuildProperties { ")
        stringBuilder.append("options: ")
        stringBuilder.append(options)
        stringBuilder.append(", ")
        stringBuilder.append("initHealth: ")
        stringBuilder.append(initHealth)
        stringBuilder.append(" }")
        stringBuilder.toString()
    }
}

object BuildProperties {
    /**
     * Read BuildProperties from input stream
     */
    def readFrom(stream: java.io.InputStream): BuildProperties = BuildProperties(
        (0 until StreamUtil.readInt(stream)).map { _ =>
            trans_gen_test.model.EntityType.readFrom(stream)
        },
        if (StreamUtil.readBoolean(stream)) Some(
            StreamUtil.readInt(stream)
        ) else None
    )
}