import java.io.*
import java.net.Socket
import trans_gen_test.util.StreamUtil;

fun main(args: Array<String>) {
    val host = args[0]
    val port = Integer.parseInt(args[1])
    val stdout = args[2].toBoolean()

    val socket = Socket(host, port)
    socket.setTcpNoDelay(true)

    val inputStream: InputStream = BufferedInputStream(socket.getInputStream())
    val outputStream: OutputStream = BufferedOutputStream(socket.getOutputStream())
    while (StreamUtil.readBoolean(inputStream)) {
        val input: trans_gen_test.codegame.MessageGameModel = trans_gen_test.codegame.MessageGameModel.readFrom(inputStream)
        if (stdout) {
            println(input)
        }
        input.writeTo(outputStream)
        outputStream.flush()
    }
    socket.close()
}