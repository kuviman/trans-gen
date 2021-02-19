unit ColoredVertex;

interface

uses
    Color in 'Color.pas',
    Nullable,
    Stream,
    SysUtils,
    Vec2Single in 'Vec2Single.pas';

type
    // Vertex for debug rendering
    TColoredVertex = class
        // Position in world coordinates (if none, screen position (0, 0) is used)
        worldPos: TNullable<TVec2Single>;
        // Additional offset in screen coordinates
        screenOffset: TVec2Single;
        // Color to use
        color: TColor;
        constructor Create(worldPos: TNullable<TVec2Single>; screenOffset: TVec2Single; color: TColor);
        // Read ColoredVertex from input stream
        class function ReadFrom(stream: TStream): TColoredVertex; static;
        // Write ColoredVertex to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TColoredVertex.Create(worldPos: TNullable<TVec2Single>; screenOffset: TVec2Single; color: TColor);
begin
    self.worldPos := worldPos;
    self.screenOffset := screenOffset;
    self.color := color;
end;

class function TColoredVertex.ReadFrom(stream: TStream): TColoredVertex;
var color: TColor;
var screenOffset: TVec2Single;
var worldPos: TNullable<TVec2Single>;
var worldPosValue: TVec2Single;
begin
    if stream.ReadBoolean then begin
        worldPosValue := TVec2Single.ReadFrom(stream);
        worldPos := worldPosValue;
    end else
        worldPos := nil;
    screenOffset := TVec2Single.ReadFrom(stream);
    color := TColor.ReadFrom(stream);
    result := TColoredVertex.Create(worldPos, screenOffset, color);
end;

procedure TColoredVertex.WriteTo(stream: TStream);
var worldPosValue: TVec2Single;
begin
    if worldPos.HasValue then begin
        stream.WriteBoolean(true);
        worldPosValue := worldPos.Value;
        worldPosValue.WriteTo(stream);
    end else
        stream.WriteBoolean(false);
    screenOffset.WriteTo(stream);
    color.WriteTo(stream);
end;

function TColoredVertex.ToString: ansistring;
var worldPosValue: TVec2Single;
begin
    result := 'ColoredVertex {';
    result += 'worldPos=';
    if worldPos.HasValue then begin
        worldPosValue := worldPos.Value;
        result += worldPosValue.ToString;
    end else
        result += 'nil';
    result += ', ';  
    result += 'screenOffset=';
    result += screenOffset.ToString;
    result += ', ';  
    result += 'color=';
    result += color.ToString;
    result += '}';
end;

end.