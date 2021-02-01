import java.io._
import java.net.Socket
import util.StreamUtil

object Runner extends App {
    if (args.size != 2) {
        throw new Exception("Pass host and port as parameters")
    }
    val host = args(0)
    val port = Integer.parseInt(args(1))

    val socket = new Socket(host, port)
    socket.setTcpNoDelay(true)

    val inputStream: InputStream = new BufferedInputStream(socket.getInputStream())
    val input: model.PlayerView = model.PlayerView.readFrom(inputStream)

    println(input)

    val outputStream: OutputStream = new BufferedOutputStream(socket.getOutputStream())
    input.writeTo(outputStream)
    outputStream.flush()

    socket.close()
}