unit EntityProperties;

interface

uses
    AttackProperties in 'Model/AttackProperties.pas',
    BuildProperties in 'Model/BuildProperties.pas',
    Nullable,
    RepairProperties in 'Model/RepairProperties.pas',
    Stream,
    SysUtils;

type
    // Entity properties
    TEntityProperties = class
        // Size. Entity has a form of a square with side of this length
        size: Int32;
        // Score for building this entity
        buildScore: Int32;
        // Score for destroying this entity
        destroyScore: Int32;
        // Whether this entity can move
        canMove: Boolean;
        // Number of population points this entity provides, if active
        populationProvide: Int32;
        // Number of population points this entity uses
        populationUse: Int32;
        // Maximum health points
        maxHealth: Int32;
        // Cost to build this first entity of this type. If this is a unit (entity can move), the cost is increased by 1 for each existing unit of this type
        initialCost: Int32;
        // If fog of war is enabled, maximum distance at which other entities are considered visible
        sightRange: Int32;
        // Amount of resource added to enemy able to collect resource on dealing damage for 1 health point
        resourcePerHealth: Int32;
        // Build properties, if entity can build
        build: TNullable<TBuildProperties>;
        // Attack properties, if entity can attack
        attack: TNullable<TAttackProperties>;
        // Repair properties, if entity can repair
        repair: TNullable<TRepairProperties>;
        constructor Create(size: Int32; buildScore: Int32; destroyScore: Int32; canMove: Boolean; populationProvide: Int32; populationUse: Int32; maxHealth: Int32; initialCost: Int32; sightRange: Int32; resourcePerHealth: Int32; build: TNullable<TBuildProperties>; attack: TNullable<TAttackProperties>; repair: TNullable<TRepairProperties>);
        // Read EntityProperties from input stream
        class function ReadFrom(stream: TStream): TEntityProperties; static;
        // Write EntityProperties to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TEntityProperties.Create(size: Int32; buildScore: Int32; destroyScore: Int32; canMove: Boolean; populationProvide: Int32; populationUse: Int32; maxHealth: Int32; initialCost: Int32; sightRange: Int32; resourcePerHealth: Int32; build: TNullable<TBuildProperties>; attack: TNullable<TAttackProperties>; repair: TNullable<TRepairProperties>);
begin
    self.size := size;
    self.buildScore := buildScore;
    self.destroyScore := destroyScore;
    self.canMove := canMove;
    self.populationProvide := populationProvide;
    self.populationUse := populationUse;
    self.maxHealth := maxHealth;
    self.initialCost := initialCost;
    self.sightRange := sightRange;
    self.resourcePerHealth := resourcePerHealth;
    self.build := build;
    self.attack := attack;
    self.repair := repair;
end;

class function TEntityProperties.ReadFrom(stream: TStream): TEntityProperties;
var attack: TNullable<TAttackProperties>;
var attackValue: TAttackProperties;
var build: TNullable<TBuildProperties>;
var buildScore: Int32;
var buildValue: TBuildProperties;
var canMove: Boolean;
var destroyScore: Int32;
var initialCost: Int32;
var maxHealth: Int32;
var populationProvide: Int32;
var populationUse: Int32;
var repair: TNullable<TRepairProperties>;
var repairValue: TRepairProperties;
var resourcePerHealth: Int32;
var sightRange: Int32;
var size: Int32;
begin
    size := stream.ReadInt32;
    buildScore := stream.ReadInt32;
    destroyScore := stream.ReadInt32;
    canMove := stream.ReadBoolean;
    populationProvide := stream.ReadInt32;
    populationUse := stream.ReadInt32;
    maxHealth := stream.ReadInt32;
    initialCost := stream.ReadInt32;
    sightRange := stream.ReadInt32;
    resourcePerHealth := stream.ReadInt32;
    if stream.ReadBoolean then begin
        buildValue := TBuildProperties.ReadFrom(stream);
        build := buildValue;
    end else
        build := nil;
    if stream.ReadBoolean then begin
        attackValue := TAttackProperties.ReadFrom(stream);
        attack := attackValue;
    end else
        attack := nil;
    if stream.ReadBoolean then begin
        repairValue := TRepairProperties.ReadFrom(stream);
        repair := repairValue;
    end else
        repair := nil;
    result := TEntityProperties.Create(size, buildScore, destroyScore, canMove, populationProvide, populationUse, maxHealth, initialCost, sightRange, resourcePerHealth, build, attack, repair);
end;

procedure TEntityProperties.WriteTo(stream: TStream);
var attackValue: TAttackProperties;
var buildValue: TBuildProperties;
var repairValue: TRepairProperties;
begin
    stream.WriteInt32(size);
    stream.WriteInt32(buildScore);
    stream.WriteInt32(destroyScore);
    stream.WriteBoolean(canMove);
    stream.WriteInt32(populationProvide);
    stream.WriteInt32(populationUse);
    stream.WriteInt32(maxHealth);
    stream.WriteInt32(initialCost);
    stream.WriteInt32(sightRange);
    stream.WriteInt32(resourcePerHealth);
    if build.HasValue then begin
        stream.WriteBoolean(true);
        buildValue := build.Value;
        buildValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
    if attack.HasValue then begin
        stream.WriteBoolean(true);
        attackValue := attack.Value;
        attackValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
    if repair.HasValue then begin
        stream.WriteBoolean(true);
        repairValue := repair.Value;
        repairValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
end;

function TEntityProperties.ToString: ansistring;
var attackValue: TAttackProperties;
var buildValue: TBuildProperties;
var repairValue: TRepairProperties;
begin
    result := 'EntityProperties {';
    result += 'size=';
    result += IntToStr(size);
    result += ', ';  
    result += 'buildScore=';
    result += IntToStr(buildScore);
    result += ', ';  
    result += 'destroyScore=';
    result += IntToStr(destroyScore);
    result += ', ';  
    result += 'canMove=';
    result += BoolToStr(canMove);
    result += ', ';  
    result += 'populationProvide=';
    result += IntToStr(populationProvide);
    result += ', ';  
    result += 'populationUse=';
    result += IntToStr(populationUse);
    result += ', ';  
    result += 'maxHealth=';
    result += IntToStr(maxHealth);
    result += ', ';  
    result += 'initialCost=';
    result += IntToStr(initialCost);
    result += ', ';  
    result += 'sightRange=';
    result += IntToStr(sightRange);
    result += ', ';  
    result += 'resourcePerHealth=';
    result += IntToStr(resourcePerHealth);
    result += ', ';  
    result += 'build=';
    if build.HasValue then begin
        buildValue := build.Value;
        result += buildValue.ToString;
    end else
        result += 'nil';
    result += ', ';  
    result += 'attack=';
    if attack.HasValue then begin
        attackValue := attack.Value;
        result += attackValue.ToString;
    end else
        result += 'nil';
    result += ', ';  
    result += 'repair=';
    if repair.HasValue then begin
        repairValue := repair.Value;
        result += repairValue.ToString;
    end else
        result += 'nil';
    result += '}';
end;

end.