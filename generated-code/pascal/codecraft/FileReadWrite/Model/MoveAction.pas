unit MoveAction;

{$mode delphi}{$H+}

interface

uses
    Stream,
    SysUtils,
    Vec2Int32 in 'Vec2Int32.pas';

type
    // Move action
    TMoveAction = class
        // Target position
        target: TVec2Int32;
        // Whether to try find closest position, if path to target is not found
        findClosestPosition: Boolean;
        // Whether to destroy other entities on the way
        breakThrough: Boolean;
        constructor Create(target: TVec2Int32; findClosestPosition: Boolean; breakThrough: Boolean);
        // Read MoveAction from input stream
        class function ReadFrom(stream: TStream): TMoveAction; static;
        // Write MoveAction to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TMoveAction.Create(target: TVec2Int32; findClosestPosition: Boolean; breakThrough: Boolean);
begin
    self.target := target;
    self.findClosestPosition := findClosestPosition;
    self.breakThrough := breakThrough;
end;

class function TMoveAction.ReadFrom(stream: TStream): TMoveAction;
var breakThrough: Boolean;
var findClosestPosition: Boolean;
var target: TVec2Int32;
begin
    target := TVec2Int32.ReadFrom(stream);
    findClosestPosition := stream.ReadBoolean;
    breakThrough := stream.ReadBoolean;
    result := TMoveAction.Create(target, findClosestPosition, breakThrough);
end;

procedure TMoveAction.WriteTo(stream: TStream);
begin
    target.WriteTo(stream);
    stream.WriteBoolean(findClosestPosition);
    stream.WriteBoolean(breakThrough);
end;

function TMoveAction.ToString: ansistring;
begin
    result := 'MoveAction {';
    result += 'target=';
    result += target.ToString;
    result += ', ';  
    result += 'findClosestPosition=';
    result += BoolToStr(findClosestPosition);
    result += ', ';  
    result += 'breakThrough=';
    result += BoolToStr(breakThrough);
    result += '}';
end;

end.