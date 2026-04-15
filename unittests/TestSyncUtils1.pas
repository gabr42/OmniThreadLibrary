unit TestSyncUtils1;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TestOmniSynchronizer = class
  public
    [Test] procedure TestSignalAndWaitFor;
    [Test] procedure TestWaitForTimeout;
    [Test] procedure TestCount;
    [Test] procedure TestReset;
    [Test] procedure TestCrossThreadSignal;
  end;

implementation

uses
  System.SysUtils,
  System.Threading,
  OtlSync.Utils;

{ TestOmniSynchronizer }

procedure TestOmniSynchronizer.TestSignalAndWaitFor;
begin
  var sync: IOmniSynchronizer := TOmniSynchronizer.Create;
  sync.Signal('ready');
  Assert.IsTrue(sync.WaitFor('ready', 0));
end;

procedure TestOmniSynchronizer.TestWaitForTimeout;
begin
  var sync: IOmniSynchronizer := TOmniSynchronizer.Create;
  Assert.IsFalse(sync.WaitFor('never', 10));
end;

procedure TestOmniSynchronizer.TestCount;
begin
  var sync: IOmniSynchronizer := TOmniSynchronizer.Create;
  Assert.AreEqual<integer>(0, sync.Count);
  sync.Signal('a');
  Assert.AreEqual<integer>(1, sync.Count);
  sync.Signal('b');
  Assert.AreEqual<integer>(2, sync.Count);
  // Signaling same name again doesn't increase count
  sync.Signal('a');
  Assert.AreEqual<integer>(2, sync.Count);
end;

procedure TestOmniSynchronizer.TestReset;
begin
  var sync := TOmniSynchronizer.Create;
  try
    sync.Signal('flag');
    Assert.IsTrue(sync.WaitFor('flag', 0));
    sync.Reset('flag');
    Assert.IsFalse(sync.WaitFor('flag', 0));
  finally sync.Free; end;
end;

procedure TestOmniSynchronizer.TestCrossThreadSignal;
begin
  var sync: IOmniSynchronizer := TOmniSynchronizer.Create;

  System.Threading.TTask.Run(
    procedure
    begin
      Sleep(50);
      sync.Signal('done');
    end);

  Assert.IsTrue(sync.WaitFor('done', 5000));
end;

end.
