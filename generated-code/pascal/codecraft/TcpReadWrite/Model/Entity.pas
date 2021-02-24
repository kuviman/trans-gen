unit Entity;

{$mode delphi}{$H+}

interface

uses
    EntityType in 'Model/EntityType.pas',
    Nullable,
    Stream,
    SysUtils,
    Vec2Int32 in 'Vec2Int32.pas';

type
    // Game entity
    TEntity = class
        // Entity's ID. Unique for each entity
        id: Int32;
        // Entity's owner player ID, if owned by a player
        playerId: TNullable<Int32>;
        // Entity's type
        entityType: TEntityType;
        // Entity's position (corner with minimal coordinates)
        position: TVec2Int32;
        // Current health
        health: Int32;
        // If entity is active, it can perform actions
        active: Boolean;
        constructor Create(id: Int32; playerId: TNullable<Int32>; entityType: TEntityType; position: TVec2Int32; health: Int32; active: Boolean);
        // Read Entity from input stream
        class function ReadFrom(stream: TStream): TEntity; static;
        // Write Entity to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TEntity.Create(id: Int32; playerId: TNullable<Int32>; entityType: TEntityType; position: TVec2Int32; health: Int32; active: Boolean);
begin
    self.id := id;
    self.playerId := playerId;
    self.entityType := entityType;
    self.position := position;
    self.health := health;
    self.active := active;
end;

class function TEntity.ReadFrom(stream: TStream): TEntity;
var active: Boolean;
var entityType: TEntityType;
var health: Int32;
var id: Int32;
var playerId: TNullable<Int32>;
var playerIdValue: Int32;
var position: TVec2Int32;
begin
    id := stream.ReadInt32;
    if stream.ReadBoolean then begin
        playerIdValue := stream.ReadInt32;
        playerId := playerIdValue;
    end else
        playerId := nil;
    entityType := TEntityType(stream.ReadInt32);
    position := TVec2Int32.ReadFrom(stream);
    health := stream.ReadInt32;
    active := stream.ReadBoolean;
    result := TEntity.Create(id, playerId, entityType, position, health, active);
end;

procedure TEntity.WriteTo(stream: TStream);
var playerIdValue: Int32;
begin
    stream.WriteInt32(id);
    if playerId.HasValue then begin
        stream.WriteBoolean(true);
        playerIdValue := playerId.Value;
        stream.WriteInt32(playerIdValue);
    end else
        stream.WriteBoolean(false);
    stream.WriteInt32(ord(entityType));
    position.WriteTo(stream);
    stream.WriteInt32(health);
    stream.WriteBoolean(active);
end;

function TEntity.ToString: ansistring;
var entityTypeName: String;
var playerIdValue: Int32;
begin
    result := 'Entity {';
    result += 'id=';
    result += IntToStr(id);
    result += ', ';  
    result += 'playerId=';
    if playerId.HasValue then begin
        playerIdValue := playerId.Value;
        result += IntToStr(playerIdValue);
    end else
        result += 'nil';
    result += ', ';  
    result += 'entityType=';
    WriteStr(entityTypeName, entityType);
    result += entityTypeName;
    result += ', ';  
    result += 'position=';
    result += position.ToString;
    result += ', ';  
    result += 'health=';
    result += IntToStr(health);
    result += ', ';  
    result += 'active=';
    result += BoolToStr(active);
    result += '}';
end;

end.