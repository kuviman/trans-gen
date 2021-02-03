require_relative 'tcp_stream'
require_relative 'model'

if ARGV.length != 2
    raise "Pass host and port as parameters"
end
host = ARGV[0]
port = ARGV[1].to_i

stream = TcpStream.new(host, port)
input = PlayerView.read_from(stream)

puts input

input.write_to(stream)
stream.flush

stream.close