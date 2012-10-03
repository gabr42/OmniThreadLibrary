// JCL_DEBUG_EXPERT_INSERTJDBG ON
program FullDebugMode_DLL_TestApp;

{$APPTYPE CONSOLE}

{$R *.res}

{$stackframes on}

uses
  System.SysUtils;

const
  {$if SizeOf(Pointer) = 8}
  FullDebugModeLibraryName = 'FastMM_FullDebugMode64.dll';
  {$else}
  FullDebugModeLibraryName = 'FastMM_FullDebugMode.dll';
  {$ifend}

const
  MaxEntries = 20;
  SkipFrames = 0;
  TextBufSize = 64 * 1024;
var
  LReturnAddresses: array[0..MaxEntries - 1] of NativeUInt;
  LTextBuffer: array[0..TextBufSize - 1] of AnsiChar;

{Procedures exported by the DLL that should be tested.}
procedure GetFrameBasedStackTrace(AReturnAddresses: PNativeUInt;
  AMaxDepth, ASkipFrames: Cardinal); external FullDebugModeLibraryName;
procedure GetRawStackTrace(AReturnAddresses: PNativeUInt;
  AMaxDepth, ASkipFrames: Cardinal); external FullDebugModeLibraryName;
function LogStackTrace(AReturnAddresses: PNativeUInt; AMaxDepth: Cardinal;
  ABuffer: PAnsiChar): PAnsiChar; external FullDebugModeLibraryName;

procedure TestFrameBasedStackTrace;
begin
  FillChar(LReturnAddresses, SizeOf(LReturnAddresses), 0);
  FillChar(LTextBuffer, SizeOf(LTextBuffer), 0);

  GetFrameBasedStackTrace(@LReturnAddresses, MaxEntries, SkipFrames);
  LogStackTrace(@LReturnAddresses, MaxEntries, @LTextBuffer);
  WriteLn(LTextBuffer);
end;

procedure TestRawStackTrace;
begin
  FillChar(LReturnAddresses, SizeOf(LReturnAddresses), 0);
  FillChar(LTextBuffer, SizeOf(LTextBuffer), 0);

  GetRawStackTrace(@LReturnAddresses, MaxEntries, SkipFrames);
  LogStackTrace(@LReturnAddresses, MaxEntries, @LTextBuffer);
  WriteLn(LTextBuffer);
end;

procedure RunTest;
begin
  TestFrameBasedStackTrace;
  TestRawStackTrace;
end;

begin
  try
    RunTest;
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
