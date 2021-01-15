import java.io.*;

import util.StreamUtil;

public class Runner {
    public static void main(String[] args) throws IOException {
        if (args.length != 2) {
            throw new RuntimeException("Pass input and output as parameters");
        }
        String inputFile = args[0];
        String outputFile = args[1];

        InputStream inputStream = new BufferedInputStream(new FileInputStream(inputFile));
        model.Structure input = model.Structure.readFrom(inputStream);

        OutputStream outputStream = new BufferedOutputStream(new FileOutputStream(outputFile));
        input.writeTo(outputStream);
        outputStream.flush();
        outputStream.close();
    }
}
