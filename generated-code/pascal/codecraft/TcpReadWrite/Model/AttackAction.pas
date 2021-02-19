unit AttackAction;

interface

uses
    AutoAttack in 'Model/AutoAttack.pas',
    Nullable,
    Stream,
    SysUtils;

type
    // Attack action
    TAttackAction = class
        // If specified, target entity's ID
        target: TNullable<Int32>;
        // If specified, configures auto attacking
        autoAttack: TNullable<TAutoAttack>;
        constructor Create(target: TNullable<Int32>; autoAttack: TNullable<TAutoAttack>);
        // Read AttackAction from input stream
        class function ReadFrom(stream: TStream): TAttackAction; static;
        // Write AttackAction to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TAttackAction.Create(target: TNullable<Int32>; autoAttack: TNullable<TAutoAttack>);
begin
    self.target := target;
    self.autoAttack := autoAttack;
end;

class function TAttackAction.ReadFrom(stream: TStream): TAttackAction;
var autoAttack: TNullable<TAutoAttack>;
var autoAttackValue: TAutoAttack;
var target: TNullable<Int32>;
var targetValue: Int32;
begin
    if stream.ReadBoolean then begin
        targetValue := stream.ReadInt32;
        target := targetValue;
    end else
        target := nil;
    if stream.ReadBoolean then begin
        autoAttackValue := TAutoAttack.ReadFrom(stream);
        autoAttack := autoAttackValue;
    end else
        autoAttack := nil;
    result := TAttackAction.Create(target, autoAttack);
end;

procedure TAttackAction.WriteTo(stream: TStream);
var autoAttackValue: TAutoAttack;
var targetValue: Int32;
begin
    if target.HasValue then begin
        stream.WriteBoolean(true);
        targetValue := target.Value;
        stream.WriteInt32(targetValue);
    end else
        stream.WriteBoolean(false);
    if autoAttack.HasValue then begin
        stream.WriteBoolean(true);
        autoAttackValue := autoAttack.Value;
        autoAttackValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
end;

function TAttackAction.ToString: ansistring;
var autoAttackValue: TAutoAttack;
var targetValue: Int32;
begin
    result := 'AttackAction {';
    result += 'target=';
    if target.HasValue then begin
        targetValue := target.Value;
        result += IntToStr(targetValue);
    end else
        result += 'nil';
    result += ', ';  
    result += 'autoAttack=';
    if autoAttack.HasValue then begin
        autoAttackValue := autoAttack.Value;
        result += autoAttackValue.ToString;
    end else
        result += 'nil';
    result += '}';
end;

end.