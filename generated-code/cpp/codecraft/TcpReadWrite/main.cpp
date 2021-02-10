#include <iostream>
#include <stdexcept>
#include <cstring>

#include "codegame/MessageGameModel.hpp"
#include "TcpStream.hpp"

int main(int argc, char* argv[])
{
    char* host = argv[1];
    int port = atoi(argv[2]);
    bool show_stdout = strcmp(argv[3], "true") == 0;

    TcpStream tcpStream(host, port);
    while (tcpStream.readBool()) {
        std::shared_ptr<codegame::MessageGameModel> input = codegame::MessageGameModel::readFrom(tcpStream);
        if (show_stdout) {
            std::cout << input->toString() << std::endl;
        }
        input->writeTo(tcpStream);
        tcpStream.flush();
    }

    return 0;
}