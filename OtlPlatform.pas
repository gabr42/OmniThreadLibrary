unit OtlPlatform;

{$I OtlOptions.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  DSiWin32,
  {$ENDIF MSWINDOWS}
  {$IFDEF OTL_MobileSupport}
  System.Classes,
  {$ENDIF OTL_MobileSupport}
  System.Diagnostics,
  SysUtils;

type
  TTimeSource = record
  private
    FStopwatch: TStopwatch;
  public
    class function Create: TTimeSource; static;
    function  Elapsed_ms(startTime_ms: int64): int64; inline;
    function  HasElapsed(startTime_ms, timeout_ms: int64): boolean;
    function  Timestamp_ms: int64; inline;
  end; { TTimeSource }
  PTimeSource = ^TTimeSource;

  TPlatform = record
  private
    class function  GetThreadAffinity: string; static;
    class function  GetThreadID: TThreadID; static;
    class procedure SetThreadAffinity(const value: string); static;
  public
    class property ThreadAffinity: string read GetThreadAffinity write SetThreadAffinity;
    class property ThreadID: TThreadID read GetThreadID;
  end; { TPlatform }

function Time: PTimeSource; inline;

// must be global for inlining
var
  GTimeSource: TTimeSource;

implementation

{ exports }

function Time: PTimeSource;
begin
  Result := @GTimeSource;
end; { Time }

{ TTimeSource }

class function TTimeSource.Create: TTimeSource;
begin
  Result.FStopwatch := TStopwatch.StartNew;
end; { TTimeSource.Create }

function TTimeSource.Timestamp_ms: int64;
begin
  {$IFDEF MSWINDOWS}
  if FStopwatch.IsHighResolution then
    Result := Round(FStopwatch.ElapsedTicks / FStopwatch.Frequency * 1000)
  else
    Result := DSiTimeGetTime64;
  {$ELSE}
  Result := Round(FStopwatch.ElapsedTicks / FStopwatch.Frequency * 1000)
  {$ENDIF ~MSWINDOWS}
end; { TTimeSource.Timestamp_ms }

function TTimeSource.Elapsed_ms(startTime_ms: int64): int64;
begin
  Result := Timestamp_ms - startTime_ms;
end; { TTimeSource.Elapsed_ms }

function TTimeSource.HasElapsed(startTime_ms, timeout_ms: int64): boolean;
begin
  if timeout_ms <= 0 then
    Result := true
  else if timeout_ms = INFINITE then
    Result := false
  else
    Result := (Elapsed_ms(startTime_ms) >= timeout_ms);
end; { TTimeSource.HasElapsed }

{ TPlatform }

class function TPlatform.GetThreadAffinity: string;
{$IFNDEF MSWINDOWS}
const
  CPUIDs = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz@$';
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Result := DSiGetThreadAffinity;
  {$ELSE}
  Result := Copy(CPUIDs, 1, TThread.ProcessorCount);
  {$ENDIF ~MSWINDOWS}
end; { TPlatform.GetThreadAffinity }

class function TPlatform.GetThreadID: TThreadID;
begin
  Result := {$IFDEF OTL_MobileSupport}TThread.CurrentThread.ThreadID{$ELSE}GetCurrentThreadID{$ENDIF};
end; { TPlatform.GetThreadID }

class procedure TPlatform.SetThreadAffinity(const value: string);
begin
  {$IFDEF MSWINDOWS}
  DSiSetThreadAffinity(value);
  {$ENDIF MSWINDOWS}
end; { TPlatform.SetThreadAffinity }

initialization
  GTimeSource := TTimeSource.Create;
end.
