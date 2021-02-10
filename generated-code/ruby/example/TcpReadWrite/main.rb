require_relative 'tcp_stream'
require_relative 'example'

host = ARGV[0]
port = ARGV[1].to_i
stdout = ARGV[2] == "true"

stream = TcpStream.new(host, port)
while stream.read_bool()
    input = Example.read_from(stream)
    if stdout
        puts input
    end
    input.write_to(stream)
    stream.flush
end

stream.close