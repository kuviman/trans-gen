unit RepairAction;

{$mode delphi}{$H+}

interface

uses
    Stream,
    SysUtils;

type
    // Repair action
    TRepairAction = class
        // Target entity's ID
        target: Int32;
        constructor Create(target: Int32);
        // Read RepairAction from input stream
        class function ReadFrom(stream: TStream): TRepairAction; static;
        // Write RepairAction to output stream
        procedure WriteTo(stream: TStream);
        function ToString: ansistring; override;
    end;

implementation

constructor TRepairAction.Create(target: Int32);
begin
    self.target := target;
end;

class function TRepairAction.ReadFrom(stream: TStream): TRepairAction;
var target: Int32;
begin
    target := stream.ReadInt32;
    result := TRepairAction.Create(target);
end;

procedure TRepairAction.WriteTo(stream: TStream);
begin
    stream.WriteInt32(target);
end;

function TRepairAction.ToString: ansistring;
begin
    result := 'RepairAction {';
    result += 'target=';
    result += IntToStr(target);
    result += '}';
end;

end.