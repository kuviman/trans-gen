import java.io._
import java.net.Socket
import trans_gen_test.util.StreamUtil

object Runner extends App {
    val host = args(0)
    val port = Integer.parseInt(args(1))
    val stdout = args(2).toBoolean

    val socket = new Socket(host, port)
    socket.setTcpNoDelay(true)

    val inputStream: InputStream = new BufferedInputStream(socket.getInputStream())
    val outputStream: OutputStream = new BufferedOutputStream(socket.getOutputStream())

    while (StreamUtil.readBoolean(inputStream)) {
        val input: trans_gen_test.Example = trans_gen_test.Example.readFrom(inputStream)
        if (stdout) {
            println(input)
        }
        input.writeTo(outputStream)
        outputStream.flush()
    }

    socket.close()
}