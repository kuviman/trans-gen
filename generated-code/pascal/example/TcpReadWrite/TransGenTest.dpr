uses SysUtils, BufferedStream, TcpStream, Example in 'Example.pas';

{$mode delphi}{$H+}

var
    stream: TBufferedStream;
    input: TExample;
    stdout: Boolean;
begin
    stdout := paramStr(3) = 'true';
    stream := TBufferedStream.Create(TTcpStream.Create(paramStr(1), strToInt(paramStr(2))));
    while stream.ReadBoolean do begin
        input := TExample.ReadFrom(stream);
        if stdout then
            writeln(input.ToString);
        input.WriteTo(stream);
        stream.Flush();
    end;
end.