#include <iostream>
#include <stdexcept>

#include "model/Model.hpp"
#include "TcpStream.hpp"

int main(int argc, char* argv[])
{
    if (argc != 3) {
        throw std::runtime_error("Pass host and port as parameters");
    }
    char* host = argv[1];
    int port = atoi(argv[2]);

    TcpStream tcpStream(host, port);
    PlayerView input = PlayerView::readFrom(tcpStream);
    std::cout << input.toString() << std::endl;
    input.writeTo(tcpStream);
    tcpStream.flush();

    return 0;
}