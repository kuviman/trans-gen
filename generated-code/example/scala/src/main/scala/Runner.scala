import java.io._
import util.StreamUtil

object Runner extends App {
    if (args.size != 2) {
        throw new Exception("Pass input and output as parameters")
    }
    val inputFile = args(0)
    val outputFile = args(1)

    val inputStream: InputStream = new BufferedInputStream(new FileInputStream(inputFile))
    val input: model.Example = model.Example.readFrom(inputStream)

    val outputStream: OutputStream = new BufferedOutputStream(new FileOutputStream(outputFile))
    input.writeTo(outputStream)
    outputStream.flush()
    outputStream.close()
}