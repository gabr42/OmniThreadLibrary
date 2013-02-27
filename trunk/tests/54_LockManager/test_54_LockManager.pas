unit test_54_LockManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  OtlSync;

const
  CHighSlot = 10;
  CTestDuration_sec = 5;

type
  TfrmTestLockManager = class(TForm)
    ListBox1: TListBox;
    btnUnsafe: TButton;
    Safe: TButton;
    procedure btnUnsafeClick(Sender: TObject);
    procedure SafeClick(Sender: TObject);
  private
    FValues: array [1..CHighSlot] of integer;
    procedure Prepare;
    procedure ShowValues(count: integer);
  public
  end;

var
  frmTestLockManager: TfrmTestLockManager;

implementation

uses
  DSiWin32,
  GpStuff,
  OtlParallel;

{$R *.dfm}

procedure TfrmTestLockManager.btnUnsafeClick(Sender: TObject);
var
  cnt: TGp4AlignedInt;
begin
  Prepare;
  cnt.Value := 0;
  Parallel.ParallelTask.Execute(
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
  ShowValues(cnt.Value);
end;

procedure TfrmTestLockManager.Prepare;
begin
  FillChar(FValues, SizeOf(FValues), 0);
end;

procedure TfrmTestLockManager.SafeClick(Sender: TObject);
var
  cnt        : TGp4AlignedInt;
  lockManager: TOmniLockManager<integer>;
begin
  Prepare;
  cnt.Value := 0;
  lockManager := TOmniLockManager<integer>.Create(10);
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
            FValues[slot] := FValues[slot] + 1;
            DSiYield;
            FValues[slot] := FValues[slot] - 1;
            cnt.Increment;
          finally lockManager.Unlock(slot); end;
        end;
      end);
  finally FreeAndNil(lockManager); end;
  ShowValues(cnt.Value);
end;

procedure TfrmTestLockManager.ShowValues(count: integer);
var
  s   : string;
  slot: integer;
begin
  s := IntToStr(count) + ':';
  for slot in FValues do
    s := s + '/' + IntToStr(slot);
  ListBox1.Items.Add(s);
end;

end.
