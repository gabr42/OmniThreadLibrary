program GetProcessorInfoOTL;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM4,
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  OtlCommon in '..\..\OtlCommon.pas';

var
  i: integer;

begin
  try
    Writeln('Processor groups');
    for i := 0 to Environment.ProcessorGroups.Count - 1 do
      Writeln(Environment.ProcessorGroups[i].GroupNumber, ': Mask: ',
        IntToHex(Environment.ProcessorGroups[i].Affinity.AsMask, 16));
    Writeln;

    Writeln('NUMA nodes');
    for i := 0 to Environment.NUMANodes.Count - 1 do
      Writeln(Environment.NUMANodes[i].NodeNumber, ': Group: ',
        Environment.NUMANodes[i].GroupNumber, ', Mask: ',
        IntToHex(Environment.NUMANodes[i].Affinity.AsMask, 16));

    if DebugHook <> 0 then
      Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
