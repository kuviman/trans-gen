package codegame

import "io"
import . "trans_gen_test/stream"

// Client or server message
type MessageGameModel interface {
    // Write MessageGameModel to writer
    Write(writer io.Writer)

    // Get string representation of MessageGameModel
    String() string
}

// Read MessageGameModel from reader
func ReadMessageGameModel(reader io.Reader) MessageGameModel {
    switch ReadInt32(reader) {
    case 0:
        return ReadMessageGameModelClient(reader)
    case 1:
        return ReadMessageGameModelServer(reader)
    }
    panic("Unexpected tag value")
}

// Client message
type MessageGameModelClient struct {
    // Message
    Message ClientMessage
}

func NewMessageGameModelClient(message ClientMessage) MessageGameModelClient {
    return MessageGameModelClient {
        Message: message,
    }
}

// Read Client from reader
func ReadMessageGameModelClient(reader io.Reader) MessageGameModelClient {
    var message ClientMessage
    message = ReadClientMessage(reader)
    return MessageGameModelClient {
        Message: message,
    }
}

// Write Client to writer
func (messageGameModelClient MessageGameModelClient) Write(writer io.Writer) {
    WriteInt32(writer, 0)
    message := messageGameModelClient.Message
    message.Write(writer)
}

// Get string representation of Client
func (messageGameModelClient MessageGameModelClient) String() string {
    stringResult := "{ "
    stringResult += "Message: "
    message := messageGameModelClient.Message
    stringResult += message.String()
    stringResult += " }"
    return stringResult
}

// Server message
type MessageGameModelServer struct {
    // Message
    Message ServerMessage
}

func NewMessageGameModelServer(message ServerMessage) MessageGameModelServer {
    return MessageGameModelServer {
        Message: message,
    }
}

// Read Server from reader
func ReadMessageGameModelServer(reader io.Reader) MessageGameModelServer {
    var message ServerMessage
    message = ReadServerMessage(reader)
    return MessageGameModelServer {
        Message: message,
    }
}

// Write Server to writer
func (messageGameModelServer MessageGameModelServer) Write(writer io.Writer) {
    WriteInt32(writer, 1)
    message := messageGameModelServer.Message
    message.Write(writer)
}

// Get string representation of Server
func (messageGameModelServer MessageGameModelServer) String() string {
    stringResult := "{ "
    stringResult += "Message: "
    message := messageGameModelServer.Message
    stringResult += message.String()
    stringResult += " }"
    return stringResult
}