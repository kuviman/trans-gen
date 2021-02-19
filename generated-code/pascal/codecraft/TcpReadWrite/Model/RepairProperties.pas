unit RepairProperties;

interface

uses
    EntityType in 'Model/EntityType.pas',
    Stream,
    SysUtils;

type
    // Entity's repair properties
    TRepairProperties = class
        // Valid target entity types
        validTargets: TArray<TEntityType>;
        // Health restored in one tick
        power: Int32;
        constructor Create(validTargets: TArray<TEntityType>; power: Int32);
        // Read RepairProperties from input stream
        class function ReadFrom(stream: TStream): TRepairProperties; static;
        // Write RepairProperties to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TRepairProperties.Create(validTargets: TArray<TEntityType>; power: Int32);
begin
    self.validTargets := validTargets;
    self.power := power;
end;

class function TRepairProperties.ReadFrom(stream: TStream): TRepairProperties;
var power: Int32;
var validTargets: TArray<TEntityType>;
var validTargetsElement: TEntityType;
var validTargetsIndex: Int32;
begin
    validTargets := TArray<TEntityType>.Create;
    SetLength(validTargets, stream.ReadInt32);
    for validTargetsIndex := 0 to Length(validTargets) - 1 do begin
        validTargetsElement := TEntityType(stream.ReadInt32);
        validTargets[validTargetsIndex] := validTargetsElement;
    end;
    power := stream.ReadInt32;
    result := TRepairProperties.Create(validTargets, power);
end;

procedure TRepairProperties.WriteTo(stream: TStream);
var validTargetsElement: TEntityType;
begin
    stream.WriteInt32(Length(validTargets));
    for validTargetsElement in validTargets do begin
        stream.WriteInt32(ord(validTargetsElement));
    end;
    stream.WriteInt32(power);
end;

function TRepairProperties.ToString: ansistring;
var validTargetsElement: TEntityType;
var validTargetsElementName: String;
var validTargetsIndex: Int32;
begin
    result := 'RepairProperties {';
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
    result += ', ';  
    result += 'power=';
    result += IntToStr(power);
    result += '}';
end;

end.