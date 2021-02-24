unit EntityAction;

{$mode delphi}{$H+}

interface

uses
    AttackAction in 'Model/AttackAction.pas',
    BuildAction in 'Model/BuildAction.pas',
    MoveAction in 'Model/MoveAction.pas',
    Nullable,
    RepairAction in 'Model/RepairAction.pas',
    Stream,
    SysUtils;

type
    // Entity's action
    TEntityAction = class
        // Move action
        moveAction: TNullable<TMoveAction>;
        // Build action
        buildAction: TNullable<TBuildAction>;
        // Attack action
        attackAction: TNullable<TAttackAction>;
        // Repair action
        repairAction: TNullable<TRepairAction>;
        constructor Create(moveAction: TNullable<TMoveAction>; buildAction: TNullable<TBuildAction>; attackAction: TNullable<TAttackAction>; repairAction: TNullable<TRepairAction>);
        // Read EntityAction from input stream
        class function ReadFrom(stream: TStream): TEntityAction; static;
        // Write EntityAction to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TEntityAction.Create(moveAction: TNullable<TMoveAction>; buildAction: TNullable<TBuildAction>; attackAction: TNullable<TAttackAction>; repairAction: TNullable<TRepairAction>);
begin
    self.moveAction := moveAction;
    self.buildAction := buildAction;
    self.attackAction := attackAction;
    self.repairAction := repairAction;
end;

class function TEntityAction.ReadFrom(stream: TStream): TEntityAction;
var attackAction: TNullable<TAttackAction>;
var attackActionValue: TAttackAction;
var buildAction: TNullable<TBuildAction>;
var buildActionValue: TBuildAction;
var moveAction: TNullable<TMoveAction>;
var moveActionValue: TMoveAction;
var repairAction: TNullable<TRepairAction>;
var repairActionValue: TRepairAction;
begin
    if stream.ReadBoolean then begin
        moveActionValue := TMoveAction.ReadFrom(stream);
        moveAction := moveActionValue;
    end else
        moveAction := nil;
    if stream.ReadBoolean then begin
        buildActionValue := TBuildAction.ReadFrom(stream);
        buildAction := buildActionValue;
    end else
        buildAction := nil;
    if stream.ReadBoolean then begin
        attackActionValue := TAttackAction.ReadFrom(stream);
        attackAction := attackActionValue;
    end else
        attackAction := nil;
    if stream.ReadBoolean then begin
        repairActionValue := TRepairAction.ReadFrom(stream);
        repairAction := repairActionValue;
    end else
        repairAction := nil;
    result := TEntityAction.Create(moveAction, buildAction, attackAction, repairAction);
end;

procedure TEntityAction.WriteTo(stream: TStream);
var attackActionValue: TAttackAction;
var buildActionValue: TBuildAction;
var moveActionValue: TMoveAction;
var repairActionValue: TRepairAction;
begin
    if moveAction.HasValue then begin
        stream.WriteBoolean(true);
        moveActionValue := moveAction.Value;
        moveActionValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
    if buildAction.HasValue then begin
        stream.WriteBoolean(true);
        buildActionValue := buildAction.Value;
        buildActionValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
    if attackAction.HasValue then begin
        stream.WriteBoolean(true);
        attackActionValue := attackAction.Value;
        attackActionValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
    if repairAction.HasValue then begin
        stream.WriteBoolean(true);
        repairActionValue := repairAction.Value;
        repairActionValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
end;

function TEntityAction.ToString: ansistring;
var attackActionValue: TAttackAction;
var buildActionValue: TBuildAction;
var moveActionValue: TMoveAction;
var repairActionValue: TRepairAction;
begin
    result := 'EntityAction {';
    result += 'moveAction=';
    if moveAction.HasValue then begin
        moveActionValue := moveAction.Value;
        result += moveActionValue.ToString;
    end else
        result += 'nil';
    result += ', ';  
    result += 'buildAction=';
    if buildAction.HasValue then begin
        buildActionValue := buildAction.Value;
        result += buildActionValue.ToString;
    end else
        result += 'nil';
    result += ', ';  
    result += 'attackAction=';
    if attackAction.HasValue then begin
        attackActionValue := attackAction.Value;
        result += attackActionValue.ToString;
    end else
        result += 'nil';
    result += ', ';  
    result += 'repairAction=';
    if repairAction.HasValue then begin
        repairActionValue := repairAction.Value;
        result += repairActionValue.ToString;
    end else
        result += 'nil';
    result += '}';
end;

end.