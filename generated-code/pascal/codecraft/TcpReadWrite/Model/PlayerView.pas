unit PlayerView;

interface

uses
    Entity in 'Model/Entity.pas',
    EntityProperties in 'Model/EntityProperties.pas',
    EntityType in 'Model/EntityType.pas',
    Generics.Collections,
    Player in 'Model/Player.pas',
    Stream,
    SysUtils;

type
    // Information available to the player
    TPlayerView = class
        // Your player's ID
        myId: Int32;
        // Size of the map
        mapSize: Int32;
        // Whether fog of war is enabled
        fogOfWar: Boolean;
        // Entity properties for each entity type
        entityProperties: TDictionary<TEntityType, TEntityProperties>;
        // Max tick count for the game
        maxTickCount: Int32;
        // Max pathfind nodes when performing pathfinding in the game simulator
        maxPathfindNodes: Int32;
        // Current tick
        currentTick: Int32;
        // List of players
        players: TArray<TPlayer>;
        // List of entities
        entities: TArray<TEntity>;
        constructor Create(myId: Int32; mapSize: Int32; fogOfWar: Boolean; entityProperties: TDictionary<TEntityType, TEntityProperties>; maxTickCount: Int32; maxPathfindNodes: Int32; currentTick: Int32; players: TArray<TPlayer>; entities: TArray<TEntity>);
        // Read PlayerView from input stream
        class function ReadFrom(stream: TStream): TPlayerView; static;
        // Write PlayerView to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TPlayerView.Create(myId: Int32; mapSize: Int32; fogOfWar: Boolean; entityProperties: TDictionary<TEntityType, TEntityProperties>; maxTickCount: Int32; maxPathfindNodes: Int32; currentTick: Int32; players: TArray<TPlayer>; entities: TArray<TEntity>);
begin
    self.myId := myId;
    self.mapSize := mapSize;
    self.fogOfWar := fogOfWar;
    self.entityProperties := entityProperties;
    self.maxTickCount := maxTickCount;
    self.maxPathfindNodes := maxPathfindNodes;
    self.currentTick := currentTick;
    self.players := players;
    self.entities := entities;
end;

class function TPlayerView.ReadFrom(stream: TStream): TPlayerView;
var currentTick: Int32;
var entities: TArray<TEntity>;
var entitiesElement: TEntity;
var entitiesIndex: Int32;
var entityProperties: TDictionary<TEntityType, TEntityProperties>;
var entityPropertiesIndex: Int32;
var entityPropertiesKey: TEntityType;
var entityPropertiesSize: Int32;
var entityPropertiesValue: TEntityProperties;
var fogOfWar: Boolean;
var mapSize: Int32;
var maxPathfindNodes: Int32;
var maxTickCount: Int32;
var myId: Int32;
var players: TArray<TPlayer>;
var playersElement: TPlayer;
var playersIndex: Int32;
begin
    myId := stream.ReadInt32;
    mapSize := stream.ReadInt32;
    fogOfWar := stream.ReadBoolean;
    entityProperties := TDictionary<TEntityType, TEntityProperties>.Create;
    entityPropertiesSize := stream.ReadInt32;
    for entityPropertiesIndex := 1 to entityPropertiesSize do begin
        entityPropertiesKey := TEntityType(stream.ReadInt32);
        entityPropertiesValue := TEntityProperties.ReadFrom(stream);
        entityProperties.Add(entityPropertiesKey, entityPropertiesValue);
    end;
    maxTickCount := stream.ReadInt32;
    maxPathfindNodes := stream.ReadInt32;
    currentTick := stream.ReadInt32;
    players := TArray<TPlayer>.Create;
    SetLength(players, stream.ReadInt32);
    for playersIndex := 0 to Length(players) - 1 do begin
        playersElement := TPlayer.ReadFrom(stream);
        players[playersIndex] := playersElement;
    end;
    entities := TArray<TEntity>.Create;
    SetLength(entities, stream.ReadInt32);
    for entitiesIndex := 0 to Length(entities) - 1 do begin
        entitiesElement := TEntity.ReadFrom(stream);
        entities[entitiesIndex] := entitiesElement;
    end;
    result := TPlayerView.Create(myId, mapSize, fogOfWar, entityProperties, maxTickCount, maxPathfindNodes, currentTick, players, entities);
end;

procedure TPlayerView.WriteTo(stream: TStream);
var entitiesElement: TEntity;
var entityPropertiesKey: TEntityType;
var entityPropertiesValue: TEntityProperties;
var playersElement: TPlayer;
begin
    stream.WriteInt32(myId);
    stream.WriteInt32(mapSize);
    stream.WriteBoolean(fogOfWar);
    stream.WriteInt32(entityProperties.Count);
    for entityPropertiesKey in entityProperties.Keys do begin
        entityPropertiesValue := entityProperties.Items[entityPropertiesKey];
        stream.WriteInt32(ord(entityPropertiesKey));
        entityPropertiesValue.WriteTo(stream);
    end;
    stream.WriteInt32(maxTickCount);
    stream.WriteInt32(maxPathfindNodes);
    stream.WriteInt32(currentTick);
    stream.WriteInt32(Length(players));
    for playersElement in players do begin
        playersElement.WriteTo(stream);
    end;
    stream.WriteInt32(Length(entities));
    for entitiesElement in entities do begin
        entitiesElement.WriteTo(stream);
    end;
end;

function TPlayerView.ToString: ansistring;
var entitiesElement: TEntity;
var entitiesIndex: Int32;
var entityPropertiesFirst: Boolean;
var entityPropertiesKey: TEntityType;
var entityPropertiesKeyName: String;
var entityPropertiesValue: TEntityProperties;
var playersElement: TPlayer;
var playersIndex: Int32;
begin
    result := 'PlayerView {';
    result += 'myId=';
    result += IntToStr(myId);
    result += ', ';  
    result += 'mapSize=';
    result += IntToStr(mapSize);
    result += ', ';  
    result += 'fogOfWar=';
    result += BoolToStr(fogOfWar);
    result += ', ';  
    result += 'entityProperties=';
    result += '[';
    entityPropertiesFirst := true;
    for entityPropertiesKey in entityProperties.Keys do begin
        if not entityPropertiesFirst then
            result += ', ';
        entityPropertiesFirst := false;
        entityPropertiesValue := entityProperties.Items[entityPropertiesKey];
        WriteStr(entityPropertiesKeyName, entityPropertiesKey);
        result += entityPropertiesKeyName;;
        result += ': ';
        result += entityPropertiesValue.ToString;;
    end;
    result += ']';
    result += ', ';  
    result += 'maxTickCount=';
    result += IntToStr(maxTickCount);
    result += ', ';  
    result += 'maxPathfindNodes=';
    result += IntToStr(maxPathfindNodes);
    result += ', ';  
    result += 'currentTick=';
    result += IntToStr(currentTick);
    result += ', ';  
    result += 'players=';
    result += '[';
    for playersIndex := 0 to Length(players) - 1 do begin
        if playersIndex <> 0 then
            result += ', ';
        playersElement := players[playersIndex];
        result += playersElement.ToString;;
    end;
    result += ']';
    result += ', ';  
    result += 'entities=';
    result += '[';
    for entitiesIndex := 0 to Length(entities) - 1 do begin
        if entitiesIndex <> 0 then
            result += ', ';
        entitiesElement := entities[entitiesIndex];
        result += entitiesElement.ToString;;
    end;
    result += ']';
    result += '}';
end;

end.