unit OneOf;

interface

uses
    Stream,
    SysUtils;

type
    // Oneof example
    TOneOf = class
        // Write OneOf to output stream
        procedure WriteTo(stream: TStream); virtual; abstract;
        // Read OneOf from input stream
        class function ReadFrom(stream: TStream): TOneOf; static;
    end;

type
    // First option
    TOneOfOptionOne = class (TOneOf)
        // List of integers
        vecInt32: TArray<Int32>;
        // Long integer
        longInt: Int64;
        constructor Create(vecInt32: TArray<Int32>; longInt: Int64);
        // Read OneOfOptionOne from input stream
        class function ReadFrom(stream: TStream): TOneOfOptionOne; static;
        // Write OneOfOptionOne to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

type
    // Second option
    TOneOfOptionTwo = class (TOneOf)
        // usize
        value: Int32;
        constructor Create(value: Int32);
        // Read OneOfOptionTwo from input stream
        class function ReadFrom(stream: TStream): TOneOfOptionTwo; static;
        // Write OneOfOptionTwo to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

implementation

class function TOneOf.ReadFrom(stream: TStream): TOneOf;
var tag: Int32;
begin
    tag := stream.ReadInt32;
    case tag of
        0: result := TOneOfOptionOne.ReadFrom(stream);
        1: result := TOneOfOptionTwo.ReadFrom(stream);
        else raise Exception.Create('Unexpected tag value');
    end;
end;

constructor TOneOfOptionOne.Create(vecInt32: TArray<Int32>; longInt: Int64);
begin
    self.vecInt32 := vecInt32;
    self.longInt := longInt;
end;

class function TOneOfOptionOne.ReadFrom(stream: TStream): TOneOfOptionOne;
var longInt: Int64;
var vecInt32: TArray<Int32>;
var vecInt32Element: Int32;
var vecInt32Index: Int32;
begin
    vecInt32 := TArray<Int32>.Create;
    SetLength(vecInt32, stream.ReadInt32);
    for vecInt32Index := 0 to Length(vecInt32) - 1 do begin
        vecInt32Element := stream.ReadInt32;
        vecInt32[vecInt32Index] := vecInt32Element;
    end;
    longInt := stream.ReadInt64;
    result := TOneOfOptionOne.Create(vecInt32, longInt);
end;

procedure TOneOfOptionOne.WriteTo(stream: TStream);
var vecInt32Element: Int32;
begin
    stream.WriteInt32(0);
    stream.WriteInt32(Length(vecInt32));
    for vecInt32Element in vecInt32 do begin
        stream.WriteInt32(vecInt32Element);
    end;
    stream.WriteInt64(longInt);
end;

function TOneOfOptionOne.ToString: ansistring;
var vecInt32Element: Int32;
var vecInt32Index: Int32;
begin
    result := 'OptionOne {';
    result += 'vecInt32=';
    result += '[';
    for vecInt32Index := 0 to Length(vecInt32) - 1 do begin
        if vecInt32Index <> 0 then
            result += ', ';
        vecInt32Element := vecInt32[vecInt32Index];
        result += IntToStr(vecInt32Element);;
    end;
    result += ']';
    result += ', ';  
    result += 'longInt=';
    result += IntToStr(longInt);
    result += '}';
end;

constructor TOneOfOptionTwo.Create(value: Int32);
begin
    self.value := value;
end;

class function TOneOfOptionTwo.ReadFrom(stream: TStream): TOneOfOptionTwo;
var value: Int32;
begin
    value := stream.ReadInt32;
    result := TOneOfOptionTwo.Create(value);
end;

procedure TOneOfOptionTwo.WriteTo(stream: TStream);
begin
    stream.WriteInt32(1);
    stream.WriteInt32(value);
end;

function TOneOfOptionTwo.ToString: ansistring;
begin
    result := 'OptionTwo {';
    result += 'value=';
    result += IntToStr(value);
    result += '}';
end;

end.