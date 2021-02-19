unit ClientMessage;

interface

uses
    Action in 'Model/Action.pas',
    DebugCommand in 'Codegame/DebugCommand.pas',
    Stream,
    SysUtils;

type
    // Message sent from client
    TClientMessage = class
        // Write ClientMessage to output stream
        procedure WriteTo(stream: TStream); virtual; abstract;
        // Read ClientMessage from input stream
        class function ReadFrom(stream: TStream): TClientMessage; static;
    end;

type
    // Ask app to perform new debug command
    TClientMessageDebugMessage = class (TClientMessage)
        // Command to perform
        command: TDebugCommand;
        constructor Create(command: TDebugCommand);
        // Read ClientMessageDebugMessage from input stream
        class function ReadFrom(stream: TStream): TClientMessageDebugMessage; static;
        // Write ClientMessageDebugMessage to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

type
    // Reply for ServerMessage::GetAction
    TClientMessageActionMessage = class (TClientMessage)
        // Player's action
        action: TAction;
        constructor Create(action: TAction);
        // Read ClientMessageActionMessage from input stream
        class function ReadFrom(stream: TStream): TClientMessageActionMessage; static;
        // Write ClientMessageActionMessage to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

type
    // Signifies finish of the debug update
    TClientMessageDebugUpdateDone = class (TClientMessage)
        constructor Create();
        // Read ClientMessageDebugUpdateDone from input stream
        class function ReadFrom(stream: TStream): TClientMessageDebugUpdateDone; static;
        // Write ClientMessageDebugUpdateDone to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

type
    // Request debug state from the app
    TClientMessageRequestDebugState = class (TClientMessage)
        constructor Create();
        // Read ClientMessageRequestDebugState from input stream
        class function ReadFrom(stream: TStream): TClientMessageRequestDebugState; static;
        // Write ClientMessageRequestDebugState to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

implementation

class function TClientMessage.ReadFrom(stream: TStream): TClientMessage;
var tag: Int32;
begin
    tag := stream.ReadInt32;
    case tag of
        0: result := TClientMessageDebugMessage.ReadFrom(stream);
        1: result := TClientMessageActionMessage.ReadFrom(stream);
        2: result := TClientMessageDebugUpdateDone.ReadFrom(stream);
        3: result := TClientMessageRequestDebugState.ReadFrom(stream);
        else raise Exception.Create('Unexpected tag value');
    end;
end;

constructor TClientMessageDebugMessage.Create(command: TDebugCommand);
begin
    self.command := command;
end;

class function TClientMessageDebugMessage.ReadFrom(stream: TStream): TClientMessageDebugMessage;
var command: TDebugCommand;
begin
    command := TDebugCommand.ReadFrom(stream);
    result := TClientMessageDebugMessage.Create(command);
end;

procedure TClientMessageDebugMessage.WriteTo(stream: TStream);
begin
    stream.WriteInt32(0);
    command.WriteTo(stream);
end;

function TClientMessageDebugMessage.ToString: ansistring;
begin
    result := 'DebugMessage {';
    result += 'command=';
    result += command.ToString;
    result += '}';
end;

constructor TClientMessageActionMessage.Create(action: TAction);
begin
    self.action := action;
end;

class function TClientMessageActionMessage.ReadFrom(stream: TStream): TClientMessageActionMessage;
var action: TAction;
begin
    action := TAction.ReadFrom(stream);
    result := TClientMessageActionMessage.Create(action);
end;

procedure TClientMessageActionMessage.WriteTo(stream: TStream);
begin
    stream.WriteInt32(1);
    action.WriteTo(stream);
end;

function TClientMessageActionMessage.ToString: ansistring;
begin
    result := 'ActionMessage {';
    result += 'action=';
    result += action.ToString;
    result += '}';
end;

constructor TClientMessageDebugUpdateDone.Create();
begin
end;

class function TClientMessageDebugUpdateDone.ReadFrom(stream: TStream): TClientMessageDebugUpdateDone;
begin
    result := TClientMessageDebugUpdateDone.Create();
end;

procedure TClientMessageDebugUpdateDone.WriteTo(stream: TStream);
begin
    stream.WriteInt32(2);
end;

function TClientMessageDebugUpdateDone.ToString: ansistring;
begin
    result := 'DebugUpdateDone {';
    result += '}';
end;

constructor TClientMessageRequestDebugState.Create();
begin
end;

class function TClientMessageRequestDebugState.ReadFrom(stream: TStream): TClientMessageRequestDebugState;
begin
    result := TClientMessageRequestDebugState.Create();
end;

procedure TClientMessageRequestDebugState.WriteTo(stream: TStream);
begin
    stream.WriteInt32(3);
end;

function TClientMessageRequestDebugState.ToString: ansistring;
begin
    result := 'RequestDebugState {';
    result += '}';
end;

end.