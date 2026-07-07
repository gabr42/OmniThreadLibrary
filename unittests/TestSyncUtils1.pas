unit TestSyncUtils1;

interface

{$I OtlOptions.inc}

uses
  TestFramework;

type
  TestOmniSynchronizer = class(TTestCase)
  published
    procedure TestSignalAndWaitFor;
    procedure TestWaitForTimeout;
    procedure TestCount;
    procedure TestReset;
    {$IFDEF OTL_MobileSupport}
    procedure TestCrossThreadSignal;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  {$IFDEF OTL_MobileSupport}
  Threading,
  {$ENDIF}
  Classes,
  OtlSync.Utils;

{ TestOmniSynchronizer }

procedure TestOmniSynchronizer.TestSignalAndWaitFor;
var
  sync: IOmniSynchronizer;
begin
  sync := TOmniSynchronizer.Create;
  sync.Signal('ready');
  CheckTrue(sync.WaitFor('ready', 0));
end;

procedure TestOmniSynchronizer.TestWaitForTimeout;
var
  sync: IOmniSynchronizer;
begin
  sync := TOmniSynchronizer.Create;
  CheckFalse(sync.WaitFor('never', 10));
end;

procedure TestOmniSynchronizer.TestCount;
var
  sync: IOmniSynchronizer;
begin
  sync := TOmniSynchronizer.Create;
  CheckEquals(0, sync.Count);
  sync.Signal('a');
  CheckEquals(1, sync.Count);
  sync.Signal('b');
  CheckEquals(2, sync.Count);
  // Signaling same name again doesn't increase count
  sync.Signal('a');
  CheckEquals(2, sync.Count);
end;

procedure TestOmniSynchronizer.TestReset;
var
  sync: TOmniSynchronizer;
begin
  sync := TOmniSynchronizer.Create;
  try
    sync.Signal('flag');
    CheckTrue(sync.WaitFor('flag', 0));
    sync.Reset('flag');
    CheckFalse(sync.WaitFor('flag', 0));
  finally sync.Free; end;
end;

{$IFDEF OTL_MobileSupport}
procedure TestOmniSynchronizer.TestCrossThreadSignal;
var
  sync: IOmniSynchronizer;
begin
  sync := TOmniSynchronizer.Create;

  System.Threading.TTask.Run(
    procedure
    begin
      Sleep(50);
      sync.Signal('done');
    end);

  CheckTrue(sync.WaitFor('done', 5000));
end;
{$ENDIF}

initialization
  RegisterTest(TestOmniSynchronizer.Suite);
end.
