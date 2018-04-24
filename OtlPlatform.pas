unit OtlPlatform;

{$I OtlOptions.inc}

interface

uses
  {$IFDEF OTL_HasStopwatch}
  System.Diagnostics;
  {$ELSE}
  DSiWin32;
  {$ENDIF ~OTL_HasStopwatch}

function GetTimestamp_ms: int64; inline;

type
  TTimeSource = record
  {$IFDEF OTL_HasStopwatch}
  private
    FStopwatch: TStopwatch;
  {$ENDIF OTL_HasStopwatch}
  public
    class function Create: TTimeSource; static;
    function  GetTimestamp_ms: int64;
  end; { TTimeSource }

var
  GTimeSource: TTimeSource;

implementation

{ exports }

function GetTimestamp_ms: int64;
begin
  Result := GTimeSource.GetTimestamp_ms;
end; { GetTimestamp_ms }

{ TTimeSource }

class function TTimeSource.Create: TTimeSource;
begin
  {$IFDEF OTL_HasStopwatch}
  Result.FStopwatch := TStopwatch.StartNew;
  {$ENDIF OTL_HasStopwatch}
end; { TTimeSource.Create }

function TTimeSource.GetTimestamp_ms: int64;
begin
  {$IFDEF OTL_HasStopwatch}
  Result := Round(FStopwatch.ElapsedTicks / FStopwatch.Frequency * 1000);
  {$ELSE}
  Result := DSiTimeGetTime64;
  {$ENDIF ~OTL_HasStopwatch}
end; { TTimeSource.GetTimestamp_ms }

initialization
  GTimeSource := TTimeSource.Create;
end.
