unit Structure;

interface

uses
    Stream,
    SysUtils;

type
    // Example structure
    TStructure = class
        // Text
        text: String;
        // 32-bit float
        floatNumber: Single;
        // 64-bit float
        doubleNumber: Double;
        constructor Create(text: String; floatNumber: Single; doubleNumber: Double);
        // Read Structure from input stream
        class function ReadFrom(stream: TStream): TStructure; static;
        // Write Structure to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TStructure.Create(text: String; floatNumber: Single; doubleNumber: Double);
begin
    self.text := text;
    self.floatNumber := floatNumber;
    self.doubleNumber := doubleNumber;
end;

class function TStructure.ReadFrom(stream: TStream): TStructure;
var doubleNumber: Double;
var floatNumber: Single;
var text: String;
begin
    text := stream.ReadString;
    floatNumber := stream.ReadSingle;
    doubleNumber := stream.ReadDouble;
    result := TStructure.Create(text, floatNumber, doubleNumber);
end;

procedure TStructure.WriteTo(stream: TStream);
begin
    stream.WriteString(text);
    stream.WriteSingle(floatNumber);
    stream.WriteDouble(doubleNumber);
end;

function TStructure.ToString: ansistring;
begin
    result := 'Structure {';
    result += 'text=';
    result += '''';
    result += text;
    result += '''';
    result += ', ';  
    result += 'floatNumber=';
    result += FloatToStr(floatNumber);
    result += ', ';  
    result += 'doubleNumber=';
    result += FloatToStr(doubleNumber);
    result += '}';
end;

end.