program GetProcessorInfo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes;

var
  CRelationship: array [0..4] of string = (
    '0: Core', '1: NumaNode', '2: Cache', '3: Package', '4: Group'
  );

procedure ShowProcessorInfo(const processor: TProcessorRelationship);
var
  i: integer;
begin
  Writeln('  Group affinity:');
  for i := 0 to processor.GroupCount - 1 do
    {$R-}
    Writeln('    ', i, ': Mask: ', processor.GroupMask[i].Mask, ', Group: ', processor.GroupMask[i].Group);
    {$R+}
end;

procedure ShowNumaInfo(const numaNode: TNumaNodeRelationship);
begin
  Writeln('  NodeNumber: ', numaNode.NodeNumber);
  Writeln('  Group affinity: Mask: ', numaNode.GroupMask.Mask, ', Group:', numaNode.GroupMask.Group);
end;

procedure ShowCacheInfo(const cache: TCacheRelationship);
begin
  Writeln('  Level: ', cache.Level);
  Writeln('  Associativity: ', cache.Associativity);
  Writeln('  LineSize: ', cache.LineSize);
  Writeln('  CacheSize: ', cache.CacheSize);
  Writeln('  Type: ', Ord(cache._Type));
  Writeln('  Group affinity: Mask: ', cache.GroupMask.Mask, ', Group:', cache.GroupMask.Group);
end;

procedure ShowGroupInfo(const group: TGroupRelationship);
var
  i: integer;
begin
  Writeln('  MaximumGroupCount: ', group.MaximumGroupCount);
  Writeln('  ActiveGroupCount: ', group.ActiveGroupCount);
  for i := 0 to group.ActiveGroupCount - 1 do
    {$R-}
    Writeln('    ', i, ': MaximumProcessorCount: ', group.GroupInfo[i].MaximumProcessorCount,
      ', ActiveProcessorCount: ', group.GroupInfo[i].ActiveProcessorCount,
      ', ActiveProcessorMask: ', group.GroupInfo[i].ActiveProcessorMask);
    {$R+}
end;

procedure DumpToFile(const fileName: string; buffer: pointer; bufSize: integer);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fileName, fmCreate);
  try
    fs.WriteBuffer(buffer^, bufSize);
  finally FreeAndNil(fs); end;
end;

var
  bufLen: DWORD;
  procInfo: PSystemLogicalProcessorInformationEx;
  currentInfo: PSystemLogicalProcessorInformationEx;
  iProc: integer;

begin
  try
    bufLen := 0;
    GetLogicalProcessorInformationEx(RelationAll, nil, bufLen);
    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
      Writeln('GetLogicalProcessorInformation[1] failed with ', GetLastError)
    else begin
      GetMem(procInfo, bufLen);
      try
        if not GetLogicalProcessorInformationEx(RelationAll, PSYSTEM_LOGICAL_PROCESSOR_INFORMATION(procInfo), bufLen) then
          Writeln('GetLogicalProcessorInformation[2] failed with ', GetLastError)
        else begin
          iProc := 0;
          currentInfo := procInfo;
          while (NativeUInt(currentInfo) - NativeUInt(procInfo)) < bufLen do begin
            Writeln(iProc, ':');
            Writeln('  Relationship: ', CRelationship[integer(currentInfo.Relationship)]);
            case integer(currentInfo.Relationship) of
              0: ShowProcessorInfo(currentInfo.Processor);
              1: ShowNumaInfo(currentInfo.NumaNode);
              2: ShowCacheInfo(currentInfo.Cache);
              3: ShowProcessorInfo(currentInfo.Processor);
              4: ShowGroupInfo(currentInfo.Group);
            end;
            currentInfo := PSystemLogicalProcessorInformationEx(NativeUInt(currentInfo) + currentInfo.Size);
            Inc(iProc);
          end;
        end;

        DumpToFile('procinfo.dat', procInfo, bufLen);
        Writeln;
        Writeln('Processor info saved to "procinfo.dat"');
      finally FreeMem(procInfo); end;
    end;

    if DebugHook <> 0 then
      Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
