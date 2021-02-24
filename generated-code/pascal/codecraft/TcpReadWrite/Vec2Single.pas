unit Vec2Single;

{$mode delphi}{$H+}

interface

uses
    Stream,
    SysUtils;

type
    // 2 dimensional vector.
    TVec2Single = class
        // `x` coordinate of the vector
        x: Single;
        // `y` coordinate of the vector
        y: Single;
        constructor Create(x: Single; y: Single);
        // Read Vec2Single from input stream
        class function ReadFrom(stream: TStream): TVec2Single; static;
        // Write Vec2Single to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TVec2Single.Create(x: Single; y: Single);
begin
    self.x := x;
    self.y := y;
end;

class function TVec2Single.ReadFrom(stream: TStream): TVec2Single;
var x: Single;
var y: Single;
begin
    x := stream.ReadSingle;
    y := stream.ReadSingle;
    result := TVec2Single.Create(x, y);
end;

procedure TVec2Single.WriteTo(stream: TStream);
begin
    stream.WriteSingle(x);
    stream.WriteSingle(y);
end;

function TVec2Single.ToString: ansistring;
begin
    result := 'Vec2Single {';
    result += 'x=';
    result += FloatToStr(x);
    result += ', ';  
    result += 'y=';
    result += FloatToStr(y);
    result += '}';
end;

end.