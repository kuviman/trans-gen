uses SysUtils, BufferedStream, FileStream, MessageGameModel in 'Codegame/MessageGameModel.pas';

{$mode delphi}{$H+}

var
    inputStream: TBufferedStream;
    outputStream: TBufferedStream;
    input: TMessageGameModel;
    i, repeatNumber: Integer;
begin
    repeatNumber := StrToInt(paramStr(3));
    for i := 1 to repeatNumber do begin
        inputStream := TBufferedStream.Create(TFileStream.Create(paramStr(1), TFileMode.Read));
        input := TMessageGameModel.ReadFrom(inputStream);
        inputStream.Free;
        if repeatNumber = 1 then
            writeln(input.ToString);
        outputStream := TBufferedStream.Create(TFileStream.Create(paramStr(2), TFileMode.Write));
        input.WriteTo(outputStream);
        outputStream.Flush();
        outputStream.Free;
    end;
end.