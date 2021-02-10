import stream;
import std.stdio;
import std.conv;
import std.exception;
import example;

class FileStream : Stream
{
    this(File file)
    {
        this.file = file;
    }

    override ubyte[] readBytes(size_t byteCount)
    {
        return this.file.rawRead(new ubyte[byteCount]);
    }

    override void writeBytes(const ubyte[] data)
    {
        this.file.rawWrite(data);
    }

    override void flush()
    {
        this.file.flush();
    }

private:
    File file;
}

void main(string[] args)
{
    string inputFile = args[1];
    string outputFile = args[2];
    int repeat = parse!int(args[3]);

    for (int i = 0; i < repeat; i++) {
        Example input = Example.readFrom(new FileStream(File(inputFile, "rb")));
        if (repeat == 1) {
            writeln(input);
        }
        input.writeTo(new FileStream(File(outputFile, "wb")));
    }
}