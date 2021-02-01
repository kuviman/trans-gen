import java.io.*;
import java.net.Socket;

import util.StreamUtil;

public class Runner {
    public static void main(String[] args) throws IOException {
        if (args.length != 2) {
            throw new RuntimeException("Pass host and port as parameters");
        }
        String host = args[0];
        int port = Integer.parseInt(args[1]);

        Socket socket = new Socket(host, port);
        socket.setTcpNoDelay(true);

        InputStream inputStream = new BufferedInputStream(socket.getInputStream());
        model.Example input = model.Example.readFrom(inputStream);

        System.out.println(input);

        OutputStream outputStream = new BufferedOutputStream(socket.getOutputStream());
        input.writeTo(outputStream);
        outputStream.flush();

        socket.close();
    }
}