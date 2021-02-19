uses SysUtils, BufferedStream, TcpStream, MessageGameModel in 'Codegame/MessageGameModel.pas';

var
    stream: TBufferedStream;
    input: TMessageGameModel;
    stdout: Boolean;
begin
    stdout := paramStr(3) = 'true';
    stream := TBufferedStream.Create(TTcpStream.Create(paramStr(1), strToInt(paramStr(2))));
    while stream.ReadBoolean do begin
        input := TMessageGameModel.ReadFrom(stream);
        if stdout then
            writeln(input.ToString);
        input.WriteTo(stream);
        stream.Flush();
    end;
end.