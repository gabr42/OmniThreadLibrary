unit OtlPlatform;

{$I OtlOptions.inc}

interface

uses
  {$IFDEF OTL_HasStopwatch}
  System.Diagnostics;
  {$ELSE}
  DSiWin32;
  {$ENDIF ~OTL_HasStopwatch}

type
  TTimeSource = record
  {$IFDEF OTL_HasStopwatch}
  private
    FStopwatch: TStopwatch;
  {$ENDIF OTL_HasStopwatch}
  public
    class function Create: TTimeSource; static;
    function  Elapsed_ms(startTime_ms: int64): int64; inline;
    function  HasElapsed(startTime_ms, timeout_ms: int64): boolean;
    function  Timestamp_ms: int64; inline;
  end; { TTimeSource }
  PTimeSource = ^TTimeSource;

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
  {$IFDEF OTL_HasStopwatch}
  Result.FStopwatch := TStopwatch.StartNew;
  {$ENDIF OTL_HasStopwatch}
end; { TTimeSource.Create }

function TTimeSource.Timestamp_ms: int64;
begin
  {$IFDEF OTL_HasStopwatch}
  Result := Round(FStopwatch.ElapsedTicks / FStopwatch.Frequency * 1000);
  {$ELSE}
  Result := DSiTimeGetTime64;
  {$ENDIF ~OTL_HasStopwatch}
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

initialization
  GTimeSource := TTimeSource.Create;
end.
