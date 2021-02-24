unit Color;

{$mode delphi}{$H+}

interface

uses
    Stream,
    SysUtils;

type
    // RGBA Color
    TColor = class
        // Red component
        r: Single;
        // Green component
        g: Single;
        // Blue component
        b: Single;
        // Alpha (opacity) component
        a: Single;
        constructor Create(r: Single; g: Single; b: Single; a: Single);
        // Read Color from input stream
        class function ReadFrom(stream: TStream): TColor; static;
        // Write Color to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TColor.Create(r: Single; g: Single; b: Single; a: Single);
begin
    self.r := r;
    self.g := g;
    self.b := b;
    self.a := a;
end;

class function TColor.ReadFrom(stream: TStream): TColor;
var a: Single;
var b: Single;
var g: Single;
var r: Single;
begin
    r := stream.ReadSingle;
    g := stream.ReadSingle;
    b := stream.ReadSingle;
    a := stream.ReadSingle;
    result := TColor.Create(r, g, b, a);
end;

procedure TColor.WriteTo(stream: TStream);
begin
    stream.WriteSingle(r);
    stream.WriteSingle(g);
    stream.WriteSingle(b);
    stream.WriteSingle(a);
end;

function TColor.ToString: ansistring;
begin
    result := 'Color {';
    result += 'r=';
    result += FloatToStr(r);
    result += ', ';  
    result += 'g=';
    result += FloatToStr(g);
    result += ', ';  
    result += 'b=';
    result += FloatToStr(b);
    result += ', ';  
    result += 'a=';
    result += FloatToStr(a);
    result += '}';
end;

end.