let host = CommandLine.arguments[1]
let port = Int(CommandLine.arguments[2])!
let stdout = CommandLine.arguments[3] == "true"

let tcpStream = TcpStream(host, port)

while tcpStream.readBool() {
	let input = MessageGameModel.readFrom(tcpStream)
	if stdout {
		print(input)
	}
	input.writeTo(tcpStream)
	tcpStream.flush()
}