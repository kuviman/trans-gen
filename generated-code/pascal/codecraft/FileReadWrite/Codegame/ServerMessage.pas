unit ServerMessage;

interface

uses
    PlayerView in 'Model/PlayerView.pas',
    Stream,
    SysUtils;

type
    // Message sent from server
    TServerMessage = class
        // Write ServerMessage to output stream
        procedure WriteTo(stream: TStream); virtual; abstract;
        // Read ServerMessage from input stream
        class function ReadFrom(stream: TStream): TServerMessage; static;
    end;

type
    // Get action for next tick
    TServerMessageGetAction = class (TServerMessage)
        // Player's view
        playerView: TPlayerView;
        // Whether app is running with debug interface available
        debugAvailable: Boolean;
        constructor Create(playerView: TPlayerView; debugAvailable: Boolean);
        // Read ServerMessageGetAction from input stream
        class function ReadFrom(stream: TStream): TServerMessageGetAction; static;
        // Write ServerMessageGetAction to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

type
    // Signifies end of the game
    TServerMessageFinish = class (TServerMessage)
        constructor Create();
        // Read ServerMessageFinish from input stream
        class function ReadFrom(stream: TStream): TServerMessageFinish; static;
        // Write ServerMessageFinish to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

type
    // Debug update
    TServerMessageDebugUpdate = class (TServerMessage)
        // Player's view
        playerView: TPlayerView;
        constructor Create(playerView: TPlayerView);
        // Read ServerMessageDebugUpdate from input stream
        class function ReadFrom(stream: TStream): TServerMessageDebugUpdate; static;
        // Write ServerMessageDebugUpdate to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

implementation

class function TServerMessage.ReadFrom(stream: TStream): TServerMessage;
var tag: Int32;
begin
    tag := stream.ReadInt32;
    case tag of
        0: result := TServerMessageGetAction.ReadFrom(stream);
        1: result := TServerMessageFinish.ReadFrom(stream);
        2: result := TServerMessageDebugUpdate.ReadFrom(stream);
        else raise Exception.Create('Unexpected tag value');
    end;
end;

constructor TServerMessageGetAction.Create(playerView: TPlayerView; debugAvailable: Boolean);
begin
    self.playerView := playerView;
    self.debugAvailable := debugAvailable;
end;

class function TServerMessageGetAction.ReadFrom(stream: TStream): TServerMessageGetAction;
var debugAvailable: Boolean;
var playerView: TPlayerView;
begin
    playerView := TPlayerView.ReadFrom(stream);
    debugAvailable := stream.ReadBoolean;
    result := TServerMessageGetAction.Create(playerView, debugAvailable);
end;

procedure TServerMessageGetAction.WriteTo(stream: TStream);
begin
    stream.WriteInt32(0);
    playerView.WriteTo(stream);
    stream.WriteBoolean(debugAvailable);
end;

function TServerMessageGetAction.ToString: ansistring;
begin
    result := 'GetAction {';
    result += 'playerView=';
    result += playerView.ToString;
    result += ', ';  
    result += 'debugAvailable=';
    result += BoolToStr(debugAvailable);
    result += '}';
end;

constructor TServerMessageFinish.Create();
begin
end;

class function TServerMessageFinish.ReadFrom(stream: TStream): TServerMessageFinish;
begin
    result := TServerMessageFinish.Create();
end;

procedure TServerMessageFinish.WriteTo(stream: TStream);
begin
    stream.WriteInt32(1);
end;

function TServerMessageFinish.ToString: ansistring;
begin
    result := 'Finish {';
    result += '}';
end;

constructor TServerMessageDebugUpdate.Create(playerView: TPlayerView);
begin
    self.playerView := playerView;
end;

class function TServerMessageDebugUpdate.ReadFrom(stream: TStream): TServerMessageDebugUpdate;
var playerView: TPlayerView;
begin
    playerView := TPlayerView.ReadFrom(stream);
    result := TServerMessageDebugUpdate.Create(playerView);
end;

procedure TServerMessageDebugUpdate.WriteTo(stream: TStream);
begin
    stream.WriteInt32(2);
    playerView.WriteTo(stream);
end;

function TServerMessageDebugUpdate.ToString: ansistring;
begin
    result := 'DebugUpdate {';
    result += 'playerView=';
    result += playerView.ToString;
    result += '}';
end;

end.