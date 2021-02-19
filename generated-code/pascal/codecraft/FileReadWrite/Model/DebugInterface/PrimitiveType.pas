unit PrimitiveType;

interface

uses
    Stream,
    SysUtils;

type
    // Primitive type for debug rendering
    {$scopedEnums on}
    TPrimitiveType = (
        // Lines, number of vertices should be divisible by 2
        Lines = 0,
        // Triangles, number of vertices should be divisible by 3
        Triangles = 1);

implementation

end.