unit Enumeration;

{$mode delphi}{$H+}

interface

uses
    Stream,
    SysUtils;

type
    // Example enumeration
    {$scopedEnums on}
    TEnumeration = (
        // First option
        ValueOne = 0,
        // Second option
        ValueTwo = 1);

implementation

end.