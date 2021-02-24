unit BuildProperties;

{$mode delphi}{$H+}

interface

uses
    EntityType in 'Model/EntityType.pas',
    Nullable,
    Stream,
    SysUtils;

type
    // Entity's build properties
    TBuildProperties = class
        // Valid new entity types
        options: TArray<TEntityType>;
        // Initial health of new entity. If absent, it will have full health
        initHealth: TNullable<Int32>;
        constructor Create(options: TArray<TEntityType>; initHealth: TNullable<Int32>);
        // Read BuildProperties from input stream
        class function ReadFrom(stream: TStream): TBuildProperties; static;
        // Write BuildProperties to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TBuildProperties.Create(options: TArray<TEntityType>; initHealth: TNullable<Int32>);
begin
    self.options := options;
    self.initHealth := initHealth;
end;

class function TBuildProperties.ReadFrom(stream: TStream): TBuildProperties;
var initHealth: TNullable<Int32>;
var initHealthValue: Int32;
var options: TArray<TEntityType>;
var optionsElement: TEntityType;
var optionsIndex: Int32;
begin
    options := TArray<TEntityType>.Create;
    SetLength(options, stream.ReadInt32);
    for optionsIndex := 0 to Length(options) - 1 do begin
        optionsElement := TEntityType(stream.ReadInt32);
        options[optionsIndex] := optionsElement;
    end;
    if stream.ReadBoolean then begin
        initHealthValue := stream.ReadInt32;
        initHealth := initHealthValue;
    end else
        initHealth := nil;
    result := TBuildProperties.Create(options, initHealth);
end;

procedure TBuildProperties.WriteTo(stream: TStream);
var initHealthValue: Int32;
var optionsElement: TEntityType;
begin
    stream.WriteInt32(Length(options));
    for optionsElement in options do begin
        stream.WriteInt32(ord(optionsElement));
    end;
    if initHealth.HasValue then begin
        stream.WriteBoolean(true);
        initHealthValue := initHealth.Value;
        stream.WriteInt32(initHealthValue);
    end else
        stream.WriteBoolean(false);
end;

function TBuildProperties.ToString: ansistring;
var initHealthValue: Int32;
var optionsElement: TEntityType;
var optionsElementName: String;
var optionsIndex: Int32;
begin
    result := 'BuildProperties {';
    result += 'options=';
    result += '[';
    for optionsIndex := 0 to Length(options) - 1 do begin
        if optionsIndex <> 0 then
            result += ', ';
        optionsElement := options[optionsIndex];
        WriteStr(optionsElementName, optionsElement);
        result += optionsElementName;;
    end;
    result += ']';
    result += ', ';  
    result += 'initHealth=';
    if initHealth.HasValue then begin
        initHealthValue := initHealth.Value;
        result += IntToStr(initHealthValue);
    end else
        result += 'nil';
    result += '}';
end;

end.