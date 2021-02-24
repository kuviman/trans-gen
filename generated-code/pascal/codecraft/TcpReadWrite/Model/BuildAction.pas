unit BuildAction;

{$mode delphi}{$H+}

interface

uses
    EntityType in 'Model/EntityType.pas',
    Stream,
    SysUtils,
    Vec2Int32 in 'Vec2Int32.pas';

type
    // Build action
    TBuildAction = class
        // Type of an entity to build
        entityType: TEntityType;
        // Desired position of new entity
        position: TVec2Int32;
        constructor Create(entityType: TEntityType; position: TVec2Int32);
        // Read BuildAction from input stream
        class function ReadFrom(stream: TStream): TBuildAction; static;
        // Write BuildAction to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TBuildAction.Create(entityType: TEntityType; position: TVec2Int32);
begin
    self.entityType := entityType;
    self.position := position;
end;

class function TBuildAction.ReadFrom(stream: TStream): TBuildAction;
var entityType: TEntityType;
var position: TVec2Int32;
begin
    entityType := TEntityType(stream.ReadInt32);
    position := TVec2Int32.ReadFrom(stream);
    result := TBuildAction.Create(entityType, position);
end;

procedure TBuildAction.WriteTo(stream: TStream);
begin
    stream.WriteInt32(ord(entityType));
    position.WriteTo(stream);
end;

function TBuildAction.ToString: ansistring;
var entityTypeName: String;
begin
    result := 'BuildAction {';
    result += 'entityType=';
    WriteStr(entityTypeName, entityType);
    result += entityTypeName;
    result += ', ';  
    result += 'position=';
    result += position.ToString;
    result += '}';
end;

end.