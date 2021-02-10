#nowarn "0058"

namespace TransGenTest.Codegame

open TransGenTest

/// Client message
type MessageGameModelClient = {
    /// Message
    Message: Codegame.ClientMessage;
} with

    /// Write Client to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 0
        this.Message.writeTo writer

    /// Read Client from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Message = Codegame.ClientMessage.readFrom reader;
    }

/// Server message
type MessageGameModelServer = {
    /// Message
    Message: Codegame.ServerMessage;
} with

    /// Write Server to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        writer.Write 1
        this.Message.writeTo writer

    /// Read Server from reader
    static member readFrom(reader: System.IO.BinaryReader) = {
        Message = Codegame.ServerMessage.readFrom reader;
    }

/// Client or server message
type MessageGameModel =
    /// Client message
    | Client of MessageGameModelClient
    /// Server message
    | Server of MessageGameModelServer
    with

    /// Write MessageGameModel to writer
    member this.writeTo(writer: System.IO.BinaryWriter) =
        match this with
            | Client value -> value.writeTo writer
            | Server value -> value.writeTo writer

    /// Read MessageGameModel from reader
    static member readFrom(reader: System.IO.BinaryReader) =
        match reader.ReadInt32() with
            | 0 -> Client (MessageGameModelClient.readFrom reader)
            | 1 -> Server (MessageGameModelServer.readFrom reader)
            | x -> failwith (sprintf "Unexpected tag %d" x)