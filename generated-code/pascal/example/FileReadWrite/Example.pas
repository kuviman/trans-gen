unit Example;

{$mode delphi}{$H+}

interface

uses
    Enumeration in 'Enumeration.pas',
    Generics.Collections,
    Nullable,
    OneOf in 'OneOf.pas',
    Stream,
    Structure in 'Structure.pas',
    SysUtils;

type
    // Example
    TExample = class
        // OneOf
        oneOf: TOneOf;
        // Dictionary
        hashMap: TDictionary<TEnumeration, Int32>;
        // Optional int
        optionalInt: TNullable<Int32>;
        // Optional boolean
        optionalBool: TNullable<Boolean>;
        // Optional OneOf
        optionalOneOf: TNullable<TOneOf>;
        // Optional struct
        optionalStruct: TNullable<TStructure>;
        // Optional enum
        optionalEnum: TNullable<TEnumeration>;
        constructor Create(oneOf: TOneOf; hashMap: TDictionary<TEnumeration, Int32>; optionalInt: TNullable<Int32>; optionalBool: TNullable<Boolean>; optionalOneOf: TNullable<TOneOf>; optionalStruct: TNullable<TStructure>; optionalEnum: TNullable<TEnumeration>);
        // Read Example from input stream
        class function ReadFrom(stream: TStream): TExample; static;
        // Write Example to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TExample.Create(oneOf: TOneOf; hashMap: TDictionary<TEnumeration, Int32>; optionalInt: TNullable<Int32>; optionalBool: TNullable<Boolean>; optionalOneOf: TNullable<TOneOf>; optionalStruct: TNullable<TStructure>; optionalEnum: TNullable<TEnumeration>);
begin
    self.oneOf := oneOf;
    self.hashMap := hashMap;
    self.optionalInt := optionalInt;
    self.optionalBool := optionalBool;
    self.optionalOneOf := optionalOneOf;
    self.optionalStruct := optionalStruct;
    self.optionalEnum := optionalEnum;
end;

class function TExample.ReadFrom(stream: TStream): TExample;
var hashMap: TDictionary<TEnumeration, Int32>;
var hashMapIndex: Int32;
var hashMapKey: TEnumeration;
var hashMapSize: Int32;
var hashMapValue: Int32;
var oneOf: TOneOf;
var optionalBool: TNullable<Boolean>;
var optionalBoolValue: Boolean;
var optionalEnum: TNullable<TEnumeration>;
var optionalEnumValue: TEnumeration;
var optionalInt: TNullable<Int32>;
var optionalIntValue: Int32;
var optionalOneOf: TNullable<TOneOf>;
var optionalOneOfValue: TOneOf;
var optionalStruct: TNullable<TStructure>;
var optionalStructValue: TStructure;
begin
    oneOf := TOneOf.ReadFrom(stream);
    hashMap := TDictionary<TEnumeration, Int32>.Create;
    hashMapSize := stream.ReadInt32;
    for hashMapIndex := 1 to hashMapSize do begin
        hashMapKey := TEnumeration(stream.ReadInt32);
        hashMapValue := stream.ReadInt32;
        hashMap.Add(hashMapKey, hashMapValue);
    end;
    if stream.ReadBoolean then begin
        optionalIntValue := stream.ReadInt32;
        optionalInt := optionalIntValue;
    end else
        optionalInt := nil;
    if stream.ReadBoolean then begin
        optionalBoolValue := stream.ReadBoolean;
        optionalBool := optionalBoolValue;
    end else
        optionalBool := nil;
    if stream.ReadBoolean then begin
        optionalOneOfValue := TOneOf.ReadFrom(stream);
        optionalOneOf := optionalOneOfValue;
    end else
        optionalOneOf := nil;
    if stream.ReadBoolean then begin
        optionalStructValue := TStructure.ReadFrom(stream);
        optionalStruct := optionalStructValue;
    end else
        optionalStruct := nil;
    if stream.ReadBoolean then begin
        optionalEnumValue := TEnumeration(stream.ReadInt32);
        optionalEnum := optionalEnumValue;
    end else
        optionalEnum := nil;
    result := TExample.Create(oneOf, hashMap, optionalInt, optionalBool, optionalOneOf, optionalStruct, optionalEnum);
end;

procedure TExample.WriteTo(stream: TStream);
var hashMapKey: TEnumeration;
var hashMapValue: Int32;
var optionalBoolValue: Boolean;
var optionalEnumValue: TEnumeration;
var optionalIntValue: Int32;
var optionalOneOfValue: TOneOf;
var optionalStructValue: TStructure;
begin
    oneOf.WriteTo(stream);
    stream.WriteInt32(hashMap.Count);
    for hashMapKey in hashMap.Keys do begin
        hashMapValue := hashMap.Items[hashMapKey];
        stream.WriteInt32(ord(hashMapKey));
        stream.WriteInt32(hashMapValue);
    end;
    if optionalInt.HasValue then begin
        stream.WriteBoolean(true);
        optionalIntValue := optionalInt.Value;
        stream.WriteInt32(optionalIntValue);
    end else
        stream.WriteBoolean(false);
    if optionalBool.HasValue then begin
        stream.WriteBoolean(true);
        optionalBoolValue := optionalBool.Value;
        stream.WriteBoolean(optionalBoolValue);
    end else
        stream.WriteBoolean(false);
    if optionalOneOf.HasValue then begin
        stream.WriteBoolean(true);
        optionalOneOfValue := optionalOneOf.Value;
        optionalOneOfValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
    if optionalStruct.HasValue then begin
        stream.WriteBoolean(true);
        optionalStructValue := optionalStruct.Value;
        optionalStructValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
    if optionalEnum.HasValue then begin
        stream.WriteBoolean(true);
        optionalEnumValue := optionalEnum.Value;
        stream.WriteInt32(ord(optionalEnumValue));
    end else
        stream.WriteBoolean(false);
end;

function TExample.ToString: ansistring;
var hashMapFirst: Boolean;
var hashMapKey: TEnumeration;
var hashMapKeyName: String;
var hashMapValue: Int32;
var optionalBoolValue: Boolean;
var optionalEnumValue: TEnumeration;
var optionalEnumValueName: String;
var optionalIntValue: Int32;
var optionalOneOfValue: TOneOf;
var optionalStructValue: TStructure;
begin
    result := 'Example {';
    result += 'oneOf=';
    result += oneOf.ToString;
    result += ', ';  
    result += 'hashMap=';
    result += '[';
    hashMapFirst := true;
    for hashMapKey in hashMap.Keys do begin
        if not hashMapFirst then
            result += ', ';
        hashMapFirst := false;
        hashMapValue := hashMap.Items[hashMapKey];
        WriteStr(hashMapKeyName, hashMapKey);
        result += hashMapKeyName;;
        result += ': ';
        result += IntToStr(hashMapValue);;
    end;
    result += ']';
    result += ', ';  
    result += 'optionalInt=';
    if optionalInt.HasValue then begin
        optionalIntValue := optionalInt.Value;
        result += IntToStr(optionalIntValue);
    end else
        result += 'nil';
    result += ', ';  
    result += 'optionalBool=';
    if optionalBool.HasValue then begin
        optionalBoolValue := optionalBool.Value;
        result += BoolToStr(optionalBoolValue);
    end else
        result += 'nil';
    result += ', ';  
    result += 'optionalOneOf=';
    if optionalOneOf.HasValue then begin
        optionalOneOfValue := optionalOneOf.Value;
        result += optionalOneOfValue.ToString;
    end else
        result += 'nil';
    result += ', ';  
    result += 'optionalStruct=';
    if optionalStruct.HasValue then begin
        optionalStructValue := optionalStruct.Value;
        result += optionalStructValue.ToString;
    end else
        result += 'nil';
    result += ', ';  
    result += 'optionalEnum=';
    if optionalEnum.HasValue then begin
        optionalEnumValue := optionalEnum.Value;
        WriteStr(optionalEnumValueName, optionalEnumValue);
        result += optionalEnumValueName;
    end else
        result += 'nil';
    result += '}';
end;

end.