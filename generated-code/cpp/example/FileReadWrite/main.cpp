#include <fstream>
#include <iostream>
#include <stdexcept>

#include "Example.hpp"

class FileInputStream : public InputStream {
public:
    FileInputStream(const char* path)
        : stream(path, std::ios::in | std::ios::binary)
    {
    }
    void readBytes(char* buffer, size_t byteCount)
    {
        stream.read(buffer, byteCount);
    }

private:
    std::ifstream stream;
};

class FileOutputStream : public OutputStream {
public:
    FileOutputStream(const char* path)
        : stream(path, std::ios::out | std::ios::binary)
    {
    }
    void writeBytes(const char* buffer, size_t byteCount)
    {
        stream.write(buffer, byteCount);
    }
    void flush()
    {
        stream.flush();
    }

private:
    std::ofstream stream;
};

int main(int argc, char* argv[])
{
    char* input_file = argv[1];
    char* output_file = argv[2];
    int repeat = atoi(argv[3]);

    for (int i = 0; i < repeat; i++) {
        FileInputStream fileInputStream(input_file);
        Example input = Example::readFrom(fileInputStream);
        if (repeat == 1) {
            std::cout << input.toString() << std::endl;
        }
        FileOutputStream fileOutputStream(output_file);
        input.writeTo(fileOutputStream);
    }

    return 0;
}