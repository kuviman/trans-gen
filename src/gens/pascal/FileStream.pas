
Unit FileStream;

Interface

Uses Stream;

Type 
  {$scopedEnums on}
  TFileMode = (Read, Write);
  TFileStream = Class (TStream)
    Private 
      f: File Of Byte;
    Public 
      Constructor Create(path: String; mode: TFileMode);

      Destructor destroy;
      override;

      Function ReadBytesAtMost(byteCount: Integer): TByteArray;
      override;

      Procedure Write(bytes: TByteArray);
      override;

      Procedure Flush;
      override;
  End;

Implementation

constructor TFileStream.Create(path: String; mode: TFileMode);
Begin
  AssignFile(f, path);
  If mode = TFileMode.Read Then
    Reset(f, 1)
  Else
    Rewrite(f, 1);
End;

Function TFileStream.ReadBytesAtMost(byteCount: Integer): TByteArray;

Var actualByteCount: Integer;
Begin
  result := TByteArray.Create;
  SetLength(result, byteCount);
  BlockRead(f, result[0], byteCount, actualByteCount);
  If actualByteCount <> byteCount Then
    result := copy(result, 0, actualByteCount);
End;

Procedure TFileStream.Write(bytes: TByteArray);
Begin
  BlockWrite(f, bytes[0], Length(bytes));
End;

Procedure TFileStream.Flush;
Begin
End;

destructor TFileStream.Destroy;
Begin
  CloseFile(f);
  inherited;
End;

End.
