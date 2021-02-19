unit Player;

interface

uses
    Stream,
    SysUtils;

type
    // Player (strategy, client)
    TPlayer = class
        // Player's ID
        id: Int32;
        // Current score
        score: Int32;
        // Current amount of resource
        resource: Int32;
        constructor Create(id: Int32; score: Int32; resource: Int32);
        // Read Player from input stream
        class function ReadFrom(stream: TStream): TPlayer; static;
        // Write Player to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TPlayer.Create(id: Int32; score: Int32; resource: Int32);
begin
    self.id := id;
    self.score := score;
    self.resource := resource;
end;

class function TPlayer.ReadFrom(stream: TStream): TPlayer;
var id: Int32;
var resource: Int32;
var score: Int32;
begin
    id := stream.ReadInt32;
    score := stream.ReadInt32;
    resource := stream.ReadInt32;
    result := TPlayer.Create(id, score, resource);
end;

procedure TPlayer.WriteTo(stream: TStream);
begin
    stream.WriteInt32(id);
    stream.WriteInt32(score);
    stream.WriteInt32(resource);
end;

function TPlayer.ToString: ansistring;
begin
    result := 'Player {';
    result += 'id=';
    result += IntToStr(id);
    result += ', ';  
    result += 'score=';
    result += IntToStr(score);
    result += ', ';  
    result += 'resource=';
    result += IntToStr(resource);
    result += '}';
end;

end.