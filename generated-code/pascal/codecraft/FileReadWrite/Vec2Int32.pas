unit Vec2Int32;

interface

uses
    Stream,
    SysUtils;

type
    // 2 dimensional vector.
    TVec2Int32 = class
        // `x` coordinate of the vector
        x: Int32;
        // `y` coordinate of the vector
        y: Int32;
        constructor Create(x: Int32; y: Int32);
        // Read Vec2Int32 from input stream
        class function ReadFrom(stream: TStream): TVec2Int32; static;
        // Write Vec2Int32 to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TVec2Int32.Create(x: Int32; y: Int32);
begin
    self.x := x;
    self.y := y;
end;

class function TVec2Int32.ReadFrom(stream: TStream): TVec2Int32;
var x: Int32;
var y: Int32;
begin
    x := stream.ReadInt32;
    y := stream.ReadInt32;
    result := TVec2Int32.Create(x, y);
end;

procedure TVec2Int32.WriteTo(stream: TStream);
begin
    stream.WriteInt32(x);
    stream.WriteInt32(y);
end;

function TVec2Int32.ToString: ansistring;
begin
    result := 'Vec2Int32 {';
    result += 'x=';
    result += IntToStr(x);
    result += ', ';  
    result += 'y=';
    result += IntToStr(y);
    result += '}';
end;

end.