unit DebugData;

{$mode delphi}{$H+}

interface

uses
    ColoredVertex in 'Model/DebugInterface/ColoredVertex.pas',
    PrimitiveType in 'Model/DebugInterface/PrimitiveType.pas',
    Stream,
    SysUtils;

type
    // Debug data can be drawn in the app
    TDebugData = class
        // Write DebugData to output stream
        procedure WriteTo(stream: TStream); virtual; abstract;
        // Read DebugData from input stream
        class function ReadFrom(stream: TStream): TDebugData; static;
    end;

type
    // Log some text
    TDebugDataLog = class (TDebugData)
        // Text to show
        text: String;
        constructor Create(text: String);
        // Read DebugDataLog from input stream
        class function ReadFrom(stream: TStream): TDebugDataLog; static;
        // Write DebugDataLog to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

type
    // Draw primitives
    TDebugDataPrimitives = class (TDebugData)
        // Vertices
        vertices: TArray<TColoredVertex>;
        // Primitive type
        primitiveType: TPrimitiveType;
        constructor Create(vertices: TArray<TColoredVertex>; primitiveType: TPrimitiveType);
        // Read DebugDataPrimitives from input stream
        class function ReadFrom(stream: TStream): TDebugDataPrimitives; static;
        // Write DebugDataPrimitives to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

type
    // Draw text
    TDebugDataPlacedText = class (TDebugData)
        // Vertex to determine text position and color
        vertex: TColoredVertex;
        // Text
        text: String;
        // Text alignment (0 means left, 0.5 means center, 1 means right)
        alignment: Single;
        // Font size in pixels
        size: Single;
        constructor Create(vertex: TColoredVertex; text: String; alignment: Single; size: Single);
        // Read DebugDataPlacedText from input stream
        class function ReadFrom(stream: TStream): TDebugDataPlacedText; static;
        // Write DebugDataPlacedText to output stream
        procedure WriteTo(stream: TStream); override;
        function ToString: ansistring; override;
    end;

implementation

class function TDebugData.ReadFrom(stream: TStream): TDebugData;
var tag: Int32;
begin
    tag := stream.ReadInt32;
    case tag of
        0: result := TDebugDataLog.ReadFrom(stream);
        1: result := TDebugDataPrimitives.ReadFrom(stream);
        2: result := TDebugDataPlacedText.ReadFrom(stream);
        else raise Exception.Create('Unexpected tag value');
    end;
end;

constructor TDebugDataLog.Create(text: String);
begin
    self.text := text;
end;

class function TDebugDataLog.ReadFrom(stream: TStream): TDebugDataLog;
var text: String;
begin
    text := stream.ReadString;
    result := TDebugDataLog.Create(text);
end;

procedure TDebugDataLog.WriteTo(stream: TStream);
begin
    stream.WriteInt32(0);
    stream.WriteString(text);
end;

function TDebugDataLog.ToString: ansistring;
begin
    result := 'Log {';
    result += 'text=';
    result += '''';
    result += text;
    result += '''';
    result += '}';
end;

constructor TDebugDataPrimitives.Create(vertices: TArray<TColoredVertex>; primitiveType: TPrimitiveType);
begin
    self.vertices := vertices;
    self.primitiveType := primitiveType;
end;

class function TDebugDataPrimitives.ReadFrom(stream: TStream): TDebugDataPrimitives;
var primitiveType: TPrimitiveType;
var vertices: TArray<TColoredVertex>;
var verticesElement: TColoredVertex;
var verticesIndex: Int32;
begin
    vertices := TArray<TColoredVertex>.Create;
    SetLength(vertices, stream.ReadInt32);
    for verticesIndex := 0 to Length(vertices) - 1 do begin
        verticesElement := TColoredVertex.ReadFrom(stream);
        vertices[verticesIndex] := verticesElement;
    end;
    primitiveType := TPrimitiveType(stream.ReadInt32);
    result := TDebugDataPrimitives.Create(vertices, primitiveType);
end;

procedure TDebugDataPrimitives.WriteTo(stream: TStream);
var verticesElement: TColoredVertex;
begin
    stream.WriteInt32(1);
    stream.WriteInt32(Length(vertices));
    for verticesElement in vertices do begin
        verticesElement.WriteTo(stream);
    end;
    stream.WriteInt32(ord(primitiveType));
end;

function TDebugDataPrimitives.ToString: ansistring;
var primitiveTypeName: String;
var verticesElement: TColoredVertex;
var verticesIndex: Int32;
begin
    result := 'Primitives {';
    result += 'vertices=';
    result += '[';
    for verticesIndex := 0 to Length(vertices) - 1 do begin
        if verticesIndex <> 0 then
            result += ', ';
        verticesElement := vertices[verticesIndex];
        result += verticesElement.ToString;;
    end;
    result += ']';
    result += ', ';  
    result += 'primitiveType=';
    WriteStr(primitiveTypeName, primitiveType);
    result += primitiveTypeName;
    result += '}';
end;

constructor TDebugDataPlacedText.Create(vertex: TColoredVertex; text: String; alignment: Single; size: Single);
begin
    self.vertex := vertex;
    self.text := text;
    self.alignment := alignment;
    self.size := size;
end;

class function TDebugDataPlacedText.ReadFrom(stream: TStream): TDebugDataPlacedText;
var alignment: Single;
var size: Single;
var text: String;
var vertex: TColoredVertex;
begin
    vertex := TColoredVertex.ReadFrom(stream);
    text := stream.ReadString;
    alignment := stream.ReadSingle;
    size := stream.ReadSingle;
    result := TDebugDataPlacedText.Create(vertex, text, alignment, size);
end;

procedure TDebugDataPlacedText.WriteTo(stream: TStream);
begin
    stream.WriteInt32(2);
    vertex.WriteTo(stream);
    stream.WriteString(text);
    stream.WriteSingle(alignment);
    stream.WriteSingle(size);
end;

function TDebugDataPlacedText.ToString: ansistring;
begin
    result := 'PlacedText {';
    result += 'vertex=';
    result += vertex.ToString;
    result += ', ';  
    result += 'text=';
    result += '''';
    result += text;
    result += '''';
    result += ', ';  
    result += 'alignment=';
    result += FloatToStr(alignment);
    result += ', ';  
    result += 'size=';
    result += FloatToStr(size);
    result += '}';
end;

end.