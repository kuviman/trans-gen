unit Action;

interface

uses
    EntityAction in 'Model/EntityAction.pas',
    Generics.Collections,
    Stream,
    SysUtils;

type
    // Player's action
    TAction = class
        // New actions for entities. If entity does not get new action, if will continue to perform previously set one
        entityActions: TDictionary<Int32, TEntityAction>;
        constructor Create(entityActions: TDictionary<Int32, TEntityAction>);
        // Read Action from input stream
        class function ReadFrom(stream: TStream): TAction; static;
        // Write Action to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TAction.Create(entityActions: TDictionary<Int32, TEntityAction>);
begin
    self.entityActions := entityActions;
end;

class function TAction.ReadFrom(stream: TStream): TAction;
var entityActions: TDictionary<Int32, TEntityAction>;
var entityActionsIndex: Int32;
var entityActionsKey: Int32;
var entityActionsSize: Int32;
var entityActionsValue: TEntityAction;
begin
    entityActions := TDictionary<Int32, TEntityAction>.Create;
    entityActionsSize := stream.ReadInt32;
    for entityActionsIndex := 1 to entityActionsSize do begin
        entityActionsKey := stream.ReadInt32;
        entityActionsValue := TEntityAction.ReadFrom(stream);
        entityActions.Add(entityActionsKey, entityActionsValue);
    end;
    result := TAction.Create(entityActions);
end;

procedure TAction.WriteTo(stream: TStream);
var entityActionsKey: Int32;
var entityActionsValue: TEntityAction;
begin
    stream.WriteInt32(entityActions.Count);
    for entityActionsKey in entityActions.Keys do begin
        entityActionsValue := entityActions.Items[entityActionsKey];
        stream.WriteInt32(entityActionsKey);
        entityActionsValue.WriteTo(stream);
    end;
end;

function TAction.ToString: ansistring;
var entityActionsFirst: Boolean;
var entityActionsKey: Int32;
var entityActionsValue: TEntityAction;
begin
    result := 'Action {';
    result += 'entityActions=';
    result += '[';
    entityActionsFirst := true;
    for entityActionsKey in entityActions.Keys do begin
        if not entityActionsFirst then
            result += ', ';
        entityActionsFirst := false;
        entityActionsValue := entityActions.Items[entityActionsKey];
        result += IntToStr(entityActionsKey);;
        result += ': ';
        result += entityActionsValue.ToString;;
    end;
    result += ']';
    result += '}';
end;

end.