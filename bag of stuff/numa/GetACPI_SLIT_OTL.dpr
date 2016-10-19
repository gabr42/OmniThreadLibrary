program GetACPI_SLIT_OTL;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  DSiWin32,
  OtlCommon;

procedure DumpNUMADistance;
var
  i   : integer;
  j   : integer;
  numa: IOmniNUMANodes;
begin
  numa := Environment.NUMANodes;
  for i := 0 to numa.Count - 1 do
    for j := 0 to numa.Count - 1 do
      Writeln('Distance[', numa[i].NodeNumber, '][', numa[j].NodeNumber, '] = ',
        numa.Distance(numa[i].NodeNumber, numa[j].NodeNumber));
end;

begin
  try
    DumpNUMADistance;

    if DebugHook <> 0 then
      Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
