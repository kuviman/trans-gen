if CommandLine.arguments.count != 3 {
	fatalError("Pass host and port as parameters")
}

let host = CommandLine.arguments[1]
let port = Int(CommandLine.arguments[2])!

let tcpStream = TcpStream(host, port)

let input = PlayerView.readFrom(tcpStream)

print(input)

input.writeTo(tcpStream)
tcpStream.flush()