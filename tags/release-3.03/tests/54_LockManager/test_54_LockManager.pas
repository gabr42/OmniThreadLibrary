unit test_54_LockManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  OtlSync;

const
  CHighSlot = 64;
  CTestDuration_sec = 10;

type
  TfrmTestLockManager = class(TForm)
    ListBox1: TListBox;
    btnUnsafe: TButton;
    btnSafe: TButton;
    btnMonitor: TButton;
    btnReentrancy: TButton;
    btnUnsafe1: TButton;
    btnSafe1: TButton;
    btnMonitor1: TButton;
    btnNoCollisions: TButton;
    btnNoCollisionsMonitor: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnMonitorClick(Sender: TObject);
    procedure btnNoCollisionsClick(Sender: TObject);
    procedure btnNoCollisionsMonitorClick(Sender: TObject);
    procedure btnReentrancyClick(Sender: TObject);
    procedure btnUnsafeClick(Sender: TObject);
    procedure btnSafeClick(Sender: TObject);
  private
    FValues: array [1..CHighSlot] of integer;
    FLocks: array [1..CHighSlot] of TObject;
    procedure Prepare;
    procedure ShowValues(count, slots: integer);
  public
  end;

var
  frmTestLockManager: TfrmTestLockManager;

implementation

uses
  DSiWin32,
  GpStuff,
  OtlCommon,
  OtlParallel;

{$R *.dfm}

procedure TfrmTestLockManager.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := Low(FLocks) to High(FLocks) do
    FLocks[i] := TObject.Create;
  btnUnsafe.Tag := Environment.Process.Affinity.Count;
  btnSafe.Tag := Environment.Process.Affinity.Count;
  btnMonitor.Tag := Environment.Process.Affinity.Count;
end;

procedure TfrmTestLockManager.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  for i := Low(FLocks) to High(FLocks) do
    FLocks[i].Free;
end;

procedure TfrmTestLockManager.btnMonitorClick(Sender: TObject);
var
  cnt: TGp4AlignedInt;
begin
  Prepare;
  cnt.Value := 0;
  Parallel.ParallelTask.NumTasks((Sender as TButton).Tag).Execute(
    procedure
    var
      slot: integer;
      startTime: int64;
    begin
      startTime := DSiTimeGetTime64;
      while not DSiHasElapsed64(startTime, CTestDuration_sec*1000) do begin
        slot := Random(CHighSlot) + 1;
        if System.TMonitor.Enter(FLocks[slot], CTestDuration_sec*1000) then try
          FValues[slot] := FValues[slot] + 1;
          DSiYield;
          FValues[slot] := FValues[slot] - 1;
        finally System.TMonitor.Exit(FLocks[slot]); end;
        cnt.Increment;
      end;
    end);
  ShowValues(cnt.Value, (Sender as TButton).Tag);
end;

procedure TfrmTestLockManager.btnNoCollisionsClick(Sender: TObject);
var
  cnt        : TGp4AlignedInt;
  lockManager: TOmniLockManager<integer>;
  taskNums   : TGp4AlignedInt;
begin
  Prepare;
  cnt.Value := 0;
  lockManager := TOmniLockManager<integer>.Create(CHighSlot);
  try
    Parallel.ParallelTask.Execute(
      procedure
      var
        slot: integer;
        startTime: int64;
      begin
        slot := taskNums.Increment;
        startTime := DSiTimeGetTime64;
        while not DSiHasElapsed64(startTime, CTestDuration_sec*1000) do begin
          if lockManager.Lock(slot, CTestDuration_sec*1000) then try
            FValues[slot] := FValues[slot] + 1;
            DSiYield;
            FValues[slot] := FValues[slot] - 1;
            cnt.Increment;
          finally lockManager.Unlock(slot); end;
        end;
      end);
  finally FreeAndNil(lockManager); end;
  ShowValues(cnt.Value, Environment.Process.Affinity.Count);
end;

procedure TfrmTestLockManager.btnNoCollisionsMonitorClick(Sender: TObject);
var
  cnt     : TGp4AlignedInt;
  taskNums: TGp4AlignedInt;
begin
  Prepare;
  cnt.Value := 0;
  Parallel.ParallelTask.Execute(
    procedure
    var
      slot: integer;
      startTime: int64;
    begin
      slot := taskNums.Increment;
      startTime := DSiTimeGetTime64;
      while not DSiHasElapsed64(startTime, CTestDuration_sec*1000) do begin
        if System.TMonitor.Enter(FLocks[slot], CTestDuration_sec*1000) then try
          FValues[slot] := FValues[slot] + 1;
          DSiYield;
          FValues[slot] := FValues[slot] - 1;
        finally System.TMonitor.Exit(FLocks[slot]); end;
        cnt.Increment;
      end;
    end);
  ShowValues(cnt.Value, Environment.Process.Affinity.Count);
end;

procedure TfrmTestLockManager.btnReentrancyClick(Sender: TObject);
var
  cnt        : TGp4AlignedInt;
  lockManager: TOmniLockManager<integer>;
begin
  Prepare;
  cnt.Value := 0;
  lockManager := TOmniLockManager<integer>.Create(CHighSlot);
  try
    Parallel.ParallelTask.Execute(
      procedure
      var
        slot: integer;
        startTime: int64;
      begin
        startTime := DSiTimeGetTime64;
        while not DSiHasElapsed64(startTime, CTestDuration_sec*1000) do begin
          slot := Random(CHighSlot) + 1;
          if lockManager.Lock(slot, CTestDuration_sec*1000) then try
            if not lockManager.Lock(slot, 0) then
              raise Exception.Create('Not reentrant!')
            else try
              FValues[slot] := FValues[slot] + 1;
              DSiYield;
              FValues[slot] := FValues[slot] - 1;
              cnt.Increment;
            finally lockManager.Unlock(slot); end;
          finally lockManager.Unlock(slot); end;
        end;
      end);
  finally FreeAndNil(lockManager); end;
  ShowValues(cnt.Value, Environment.Process.Affinity.Count);
end;

procedure TfrmTestLockManager.btnUnsafeClick(Sender: TObject);
var
  cnt: TGp4AlignedInt;
begin
  Prepare;
  cnt.Value := 0;
  Parallel.ParallelTask.NumTasks((Sender as TButton).Tag).Execute(
    procedure
    var
      slot: integer;
      startTime: int64;
    begin
      startTime := DSiTimeGetTime64;
      while not DSiHasElapsed64(startTime, CTestDuration_sec*1000) do begin
        slot := Random(CHighSlot) + 1;
        FValues[slot] := FValues[slot] + 1;
        DSiYield;
        FValues[slot] := FValues[slot] - 1;
        cnt.Increment;
      end;
    end);
  ShowValues(cnt.Value, (Sender as TButton).Tag);
end;

procedure TfrmTestLockManager.Prepare;
begin
  FillChar(FValues, SizeOf(FValues), 0);
end;

procedure TfrmTestLockManager.btnSafeClick(Sender: TObject);
var
  cnt        : TGp4AlignedInt;
  lockManager: TOmniLockManager<integer>;
begin
  Prepare;
  cnt.Value := 0;
  lockManager := TOmniLockManager<integer>.Create(CHighSlot);
  try
    Parallel.ParallelTask.NumTasks((Sender as TButton).Tag).Execute(
      procedure
      var
        slot: integer;
        startTime: int64;
      begin
        startTime := DSiTimeGetTime64;
        while not DSiHasElapsed64(startTime, CTestDuration_sec*1000) do begin
          slot := Random(CHighSlot) + 1;
          if lockManager.Lock(slot, CTestDuration_sec*1000) then try
            FValues[slot] := FValues[slot] + 1;
            DSiYield;
            FValues[slot] := FValues[slot] - 1;
            cnt.Increment;
          finally lockManager.Unlock(slot); end;
        end;
      end);
  finally FreeAndNil(lockManager); end;
  ShowValues(cnt.Value, (Sender as TButton).Tag);
end;

procedure TfrmTestLockManager.ShowValues(count, slots: integer);
var
  iSlot: integer;
  s    : string;
begin
  s := Format('%d operations/sec', [count div CTestDuration_sec]) + ':';
  for iSlot := 1 to slots do
    s := s + '/' + IntToStr(FValues[iSlot]);
  ListBox1.Items.Add(s);
end;

end.
