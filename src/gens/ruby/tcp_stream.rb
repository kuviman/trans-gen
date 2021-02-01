require 'stringio'
require 'socket'
require_relative 'stream'

class TcpStream < Stream
    def initialize(host, port)
        @socket = TCPSocket.open(host, port)
        @socket.setsockopt(Socket::IPPROTO_TCP, Socket::TCP_NODELAY, 1)
        @read_buffer = StringIO.new('', 'a+b')
        @write_buffer = ''
    end

    def close
        @socket.close
    end

    def read_bytes(byte_count)
        @read_buffer = StringIO.new('', 'a+b') unless @read_buffer.is_a? StringIO

        data = @read_buffer.read(byte_count) || ''

        while data.length < byte_count
            @read_buffer = StringIO.new(@socket.recv(102_400), 'a+b')
            data << @read_buffer.read(byte_count - data.length)
        end

        data
    end

    def write_bytes(data)
        @write_buffer << data
    end

    def flush
        @socket.write(@write_buffer)
        @write_buffer = ''
        @read_buffer = ''
        @socket.flush
    end
end