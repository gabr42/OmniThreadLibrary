program GetACPI_SLIT;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils;

function GetSystemFirmwareTable(
  FirmwareTableProviderSignature: DWORD;
  FirmwareTableID: DWORD;
  pFirmwareTableBuffer: pointer;
  BufferSize: DWORD): UInt; stdcall; external 'kernel32';

function GetNumaProximityNodeEx(
  ProximityId: ULONG;
  var NodeNumber: USHORT): BOOL; stdcall; external 'kernel32';

function MakeDWORD(const name: AnsiString): DWORD;
begin
  Result := (Ord(name[1]) SHL 24)
         OR (Ord(name[2]) SHL 16)
         OR (Ord(name[3]) SHL  8)
         OR (Ord(name[4])       );
end;

function MakeString(p: PAnsiChar; len: integer): AnsiString;
var
  i: integer;
begin
  SetLength(Result, len);
  for i := 1 to len do begin
    Result[i] := p^;
    Inc(p);
  end;
end;

procedure DumpSLIT(p: PByte; size: integer);
var
  i    : integer;
  j    : integer;
  numSL: integer;
  node : word;
begin
  Writeln('System Locality Information Table');
  Writeln('---------------------------------');
  Writeln('Signature: ', MakeString(PAnsiChar(p), 4));    Inc(p, 4);
  Writeln('Length: ', PDWORD(p)^);                        Inc(p, 4);
  Writeln('Revision: ', p^);                              Inc(p);
  Writeln('Checksum: ', p^);                              Inc(p);
  Writeln('OEM ID: ', MakeString(PAnsiChar(p),6));        Inc(p, 6);
  Writeln('OEM Table ID: ', MakeString(PAnsiChar(p), 8)); Inc(p, 8);
  Writeln('OEM Revision: ', PDWORD(p)^);                  Inc(p, 4);
  Writeln('Creator ID: ', MakeString(PAnsiChar(p), 4));   Inc(p, 4);
  Writeln('Creator Revision: ', PDWORD(p)^);              Inc(p, 4);
  numSL := PInt64(p)^;
  Writeln('Number of System Localities: ', numSL);        Inc(p, 8);
  for i := 0 to numSL - 1 do
    for j := 0 to numSL - 1 do begin
      Writeln('Entry[', i, '][', j, ']: ', p^);           Inc(p);
    end;
  Writeln;
  Writeln('Proximity node to NUMA node mapping');
  Writeln('-----------------------------------');
  for i := 0 to numSL - 1 do begin
    if not GetNumaProximityNodeEx(i, node) then
      Writeln('Failed to map proximity node ', i)
    else
      Writeln(i, ' ==> ', node);
  end;
end;

var
  i: integer;
  p: pointer;

begin
  try
    i := GetSystemFirmwareTable(MakeDWORD('ACPI'), MakeDWORD('TILS'), nil, 0);
    if i = 0 then
      Writeln('Table not found')
    else begin
      GetMem(p, i);
      try
        i := GetSystemFirmwareTable(MakeDWORD('ACPI'), MakeDWORD('TILS'), p, i);
        DumpSLIT(p, i);
      finally FreeMem(p); end;
    end;

    if DebugHook <> 0 then
      Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
