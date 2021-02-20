
Unit TcpStream;

Interface

Uses Stream, Sockets, NetDB, SysUtils;

Type 
  TByteArray = Array Of Byte;
  TTcpStream = Class (TStream)
    Private 
      sock: LongInt;
    Public 
      Constructor Create(host: String; port: Word);

      Destructor Destroy;
      override;

      Function ReadBytesAtMost(byteCount: Integer): TByteArray;
      override;

      Procedure Write(bytes: TByteArray);
      override;

      Procedure Flush;
      override;
  End;

Implementation

constructor TTcpStream.Create(host: String; port: Word);

Var 
  hostEntry: THostEntry;
  addr: TInetSockAddr;
Begin
  addr.sin_family := AF_INET;
  addr.sin_addr.s_addr := htonl(StrToHostAddr(host).s_addr);
  If addr.sin_addr.s_addr = 0 Then
    Begin
      If GetHostByName(host, hostEntry) Then
        Begin
          addr.sin_addr.s_addr := htonl(addr.sin_addr.s_addr);
        End
      Else
        Begin
          If Not ResolveHostByName(host, hostEntry) Then
            raise Exception.Create('Failed to resolve host');
        End;
      addr.sin_addr := hostEntry.Addr;
    End;
  sock := fpsocket(AF_INET, SOCK_STREAM, 0);
  If sock = -1 Then
    raise Exception.Create('Unable to create socket.');
  addr.sin_port := htons(port);
  If fpconnect(sock, @addr, sizeof(addr)) <> 0 Then
    raise Exception.Create('Unable to connect');
End;

Function TTcpStream.ReadBytesAtMost(byteCount: Integer): TByteArray;

Var 
  actualByteCount: Integer;
Begin
  result := TByteArray.Create;
  SetLength(result, byteCount);
  actualByteCount := fprecv(sock, result, byteCount, 0);
  If actualByteCount = -1 Then
    raise Exception.Create('Failed to read from socket');
  SetLength(result, actualByteCount);
End;

Procedure TTcpStream.Write(bytes: TByteArray);
Begin
  If fpsend(sock, bytes, Length(bytes), 0) <> Length(bytes) Then
    raise Exception.Create('Failed to send data to socket');
End;

Procedure TTcpStream.Flush;
Begin
End;

destructor TTcpStream.Destroy;
Begin
  inherited;
End;

End.
