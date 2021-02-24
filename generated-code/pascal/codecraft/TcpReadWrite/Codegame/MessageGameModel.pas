unit MessageGameModel;

{$mode delphi}{$H+}

interface

uses
    ClientMessage in 'Codegame/ClientMessage.pas',
    ServerMessage in 'Codegame/ServerMessage.pas',
    Stream,
    SysUtils;

type
    // Client or server message
    TMessageGameModel = class
        // Write MessageGameModel to output stream
        procedure WriteTo(stream: TStream); virtual; abstract;
        // Read MessageGameModel from input stream
        class function ReadFrom(stream: TStream): TMessageGameModel; static;
    end;

type
    // Client message
    TMessageGameModelClient = class (TMessageGameModel)
        // Message
        message: TClientMessage;
        constructor Create(message: TClientMessage);
        // Read MessageGameModelClient from input stream
        class function ReadFrom(stream: TStream): TMessageGameModelClient; static;
        // Write MessageGameModelClient to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

type
    // Server message
    TMessageGameModelServer = class (TMessageGameModel)
        // Message
        message: TServerMessage;
        constructor Create(message: TServerMessage);
        // Read MessageGameModelServer from input stream
        class function ReadFrom(stream: TStream): TMessageGameModelServer; static;
        // Write MessageGameModelServer to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

implementation

class function TMessageGameModel.ReadFrom(stream: TStream): TMessageGameModel;
var tag: Int32;
begin
    tag := stream.ReadInt32;
    case tag of
        0: result := TMessageGameModelClient.ReadFrom(stream);
        1: result := TMessageGameModelServer.ReadFrom(stream);
        else raise Exception.Create('Unexpected tag value');
    end;
end;

constructor TMessageGameModelClient.Create(message: TClientMessage);
begin
    self.message := message;
end;

class function TMessageGameModelClient.ReadFrom(stream: TStream): TMessageGameModelClient;
var message: TClientMessage;
begin
    message := TClientMessage.ReadFrom(stream);
    result := TMessageGameModelClient.Create(message);
end;

procedure TMessageGameModelClient.WriteTo(stream: TStream);
begin
    stream.WriteInt32(0);
    message.WriteTo(stream);
end;

function TMessageGameModelClient.ToString: ansistring;
begin
    result := 'Client {';
    result += 'message=';
    result += message.ToString;
    result += '}';
end;

constructor TMessageGameModelServer.Create(message: TServerMessage);
begin
    self.message := message;
end;

class function TMessageGameModelServer.ReadFrom(stream: TStream): TMessageGameModelServer;
var message: TServerMessage;
begin
    message := TServerMessage.ReadFrom(stream);
    result := TMessageGameModelServer.Create(message);
end;

procedure TMessageGameModelServer.WriteTo(stream: TStream);
begin
    stream.WriteInt32(1);
    message.WriteTo(stream);
end;

function TMessageGameModelServer.ToString: ansistring;
begin
    result := 'Server {';
    result += 'message=';
    result += message.ToString;
    result += '}';
end;

end.