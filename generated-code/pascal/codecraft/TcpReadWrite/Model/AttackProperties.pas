unit AttackProperties;

interface

uses
    Stream,
    SysUtils;

type
    // Entity's attack properties
    TAttackProperties = class
        // Maximum attack range
        attackRange: Int32;
        // Damage dealt in one tick
        damage: Int32;
        // If true, dealing damage will collect resource from target
        collectResource: Boolean;
        constructor Create(attackRange: Int32; damage: Int32; collectResource: Boolean);
        // Read AttackProperties from input stream
        class function ReadFrom(stream: TStream): TAttackProperties; static;
        // Write AttackProperties to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TAttackProperties.Create(attackRange: Int32; damage: Int32; collectResource: Boolean);
begin
    self.attackRange := attackRange;
    self.damage := damage;
    self.collectResource := collectResource;
end;

class function TAttackProperties.ReadFrom(stream: TStream): TAttackProperties;
var attackRange: Int32;
var collectResource: Boolean;
var damage: Int32;
begin
    attackRange := stream.ReadInt32;
    damage := stream.ReadInt32;
    collectResource := stream.ReadBoolean;
    result := TAttackProperties.Create(attackRange, damage, collectResource);
end;

procedure TAttackProperties.WriteTo(stream: TStream);
begin
    stream.WriteInt32(attackRange);
    stream.WriteInt32(damage);
    stream.WriteBoolean(collectResource);
end;

function TAttackProperties.ToString: ansistring;
begin
    result := 'AttackProperties {';
    result += 'attackRange=';
    result += IntToStr(attackRange);
    result += ', ';  
    result += 'damage=';
    result += IntToStr(damage);
    result += ', ';  
    result += 'collectResource=';
    result += BoolToStr(collectResource);
    result += '}';
end;

end.