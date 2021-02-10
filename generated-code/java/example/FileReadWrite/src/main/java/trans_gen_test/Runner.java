import java.io.*;

import trans_gen_test.util.StreamUtil;

public class Runner {
    public static void main(String[] args) throws IOException {
        String inputFile = args[0];
        String outputFile = args[1];
        int repeat = Integer.parseInt(args[2]);

        for (int i = 0; i < repeat; i++) {
            InputStream inputStream = new BufferedInputStream(new FileInputStream(inputFile));
            trans_gen_test.Example input = trans_gen_test.Example.readFrom(inputStream);
            if (repeat == 1) {
                System.out.println(input);
            }
            OutputStream outputStream = new BufferedOutputStream(new FileOutputStream(outputFile));
            input.writeTo(outputStream);
            outputStream.flush();
            outputStream.close();
        }
    }
}