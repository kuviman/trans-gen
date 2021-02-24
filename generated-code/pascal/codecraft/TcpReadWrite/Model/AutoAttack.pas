unit AutoAttack;

{$mode delphi}{$H+}

interface

uses
    EntityType in 'Model/EntityType.pas',
    Stream,
    SysUtils;

type
    // Auto attack options
    TAutoAttack = class
        // Maximum distance to pathfind
        pathfindRange: Int32;
        // List of target entity types to try to attack. If empty, all types but resource are considered
        validTargets: TArray<TEntityType>;
        constructor Create(pathfindRange: Int32; validTargets: TArray<TEntityType>);
        // Read AutoAttack from input stream
        class function ReadFrom(stream: TStream): TAutoAttack; static;
        // Write AutoAttack to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TAutoAttack.Create(pathfindRange: Int32; validTargets: TArray<TEntityType>);
begin
    self.pathfindRange := pathfindRange;
    self.validTargets := validTargets;
end;

class function TAutoAttack.ReadFrom(stream: TStream): TAutoAttack;
var pathfindRange: Int32;
var validTargets: TArray<TEntityType>;
var validTargetsElement: TEntityType;
var validTargetsIndex: Int32;
begin
    pathfindRange := stream.ReadInt32;
    validTargets := TArray<TEntityType>.Create;
    SetLength(validTargets, stream.ReadInt32);
    for validTargetsIndex := 0 to Length(validTargets) - 1 do begin
        validTargetsElement := TEntityType(stream.ReadInt32);
        validTargets[validTargetsIndex] := validTargetsElement;
    end;
    result := TAutoAttack.Create(pathfindRange, validTargets);
end;

procedure TAutoAttack.WriteTo(stream: TStream);
var validTargetsElement: TEntityType;
begin
    stream.WriteInt32(pathfindRange);
    stream.WriteInt32(Length(validTargets));
    for validTargetsElement in validTargets do begin
        stream.WriteInt32(ord(validTargetsElement));
    end;
end;

function TAutoAttack.ToString: ansistring;
var validTargetsElement: TEntityType;
var validTargetsElementName: String;
var validTargetsIndex: Int32;
begin
    result := 'AutoAttack {';
    result += 'pathfindRange=';
    result += IntToStr(pathfindRange);
    result += ', ';  
    result += 'validTargets=';
    result += '[';
    for validTargetsIndex := 0 to Length(validTargets) - 1 do begin
        if validTargetsIndex <> 0 then
            result += ', ';
        validTargetsElement := validTargets[validTargetsIndex];
        WriteStr(validTargetsElementName, validTargetsElement);
        result += validTargetsElementName;;
    end;
    result += ']';
    result += '}';
end;

end.