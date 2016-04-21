unit test_37_ParallelJoin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  GpStuff;

type
  TfrmTestParallelJoin = class(TForm)
    btnJoinAll: TButton;
    btnJoinOne: TButton;
    lbLog: TListBox;
    btnJoinTProc: TButton;
    btnCancel: TButton;
    btnNoWait: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnJoinAllClick(Sender: TObject);
    procedure btnJoinTProcClick(Sender: TObject);
    procedure btnNoWaitClick(Sender: TObject);
  protected
    FJoinCount: TGp4AlignedInt;
    FJoinCount2: TGp4AlignedInt;
    procedure Log(const msg: string);
  end;

var
  frmTestParallelJoin: TfrmTestParallelJoin;

implementation

uses
  DSiWin32,
  OtlCommon,
  OtlTask,
  OtlParallel;

{$R *.dfm}

procedure TfrmTestParallelJoin.btnCancelClick(Sender: TObject);
begin
  FJoinCount.Value := 0;
  Parallel.Join(
    procedure (const joinState: IOmniJoinState)
    begin
      Sleep(500);
      joinState.Cancel;
    end,
    procedure (const joinState: IOmniJoinState)
    var
      i: integer;
    begin
      for i := 1 to 10 do begin
        Sleep(100);
        FJoinCount.Increment;
        if joinState.IsCancelled then
          break; //for
      end;
    end
  ).Execute;
  Log(Format('Join counted up to %d', [FJoinCount.Value]));
end;

procedure TfrmTestParallelJoin.btnJoinAllClick(Sender: TObject);
var
  expectedTime: integer;
  join        : IOmniParallelJoin;
  startTime   : int64;
begin
  if Sender = btnJoinOne then
    expectedTime := 5
  else
    expectedTime := 3;
  Log(Format('Starting two tasks, expected execution time is %d seconds', [expectedTime]));
  startTime := DSiTimeGetTime64;
  join := Parallel.Join(
    procedure (const joinState: IOmniJoinState)
    begin
      Sleep(3000);
    end,
    procedure (const joinState: IOmniJoinState)
    begin
      Sleep(2000);
    end);
  if Sender = btnJoinOne then
    join.NumTasks(1);
  join.Execute;
  Log(Format('Tasks stopped, execution time was %s seconds',
    [FormatDateTime('s.zzz', DSiElapsedTime64(startTime)/MSecsPerDay)]));
end;

procedure TfrmTestParallelJoin.btnJoinTProcClick(Sender: TObject);
var
  expectedTime: integer;
  startTime   : int64;
begin
  if Environment.Process.Affinity.Count = 1 then
    expectedTime := 5
  else
    expectedTime := 3;
  Log(Format('Starting two tasks, expected execution time is %d seconds', [expectedTime]));
  startTime := DSiTimeGetTime64;
  Parallel.Join(
    procedure
    begin
      Sleep(3000);
    end,
    procedure
    begin
      Sleep(2000);
    end).Execute;
  Log(Format('Tasks stopped, execution time was %s seconds',
    [FormatDateTime('s.zzz', DSiElapsedTime64(startTime)/MSecsPerDay)]));
end;

procedure TfrmTestParallelJoin.btnNoWaitClick(Sender: TObject);
var
  join: IOmniParallelJoin;
  time: int64;
begin
  FJoinCount.Value := 0;
  FJoinCount2.Value := 0;
  join := Parallel.Join(
    procedure (const joinState: IOmniJoinState)
    var
      i: integer;
    begin
      for i := 1 to 10 do begin
        Sleep(100);
        FJoinCount.Increment;
        if joinState.IsCancelled then
          break; //for
      end;
    end,
    procedure (const joinState: IOmniJoinState)
    var
      i: integer;
    begin
      for i := 1 to 10 do begin
        Sleep(200);
        FJoinCount2.Increment;
        if joinState.IsCancelled then
          break; //for
      end;
    end
  ).NoWait.Execute;
  Sleep(500);
  time := DSiTimeGetTime64;
  join.Cancel.WaitFor(INFINITE);
  Log(Format('Waited %d ms for joins to terminate', [DSiElapsedTime64(time)]));
  Log(Format('Joins counted up to %d and %d', [FJoinCount.Value, FJoinCount2.Value]));
end;

procedure TfrmTestParallelJoin.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn:ss', Now) + ' ' + msg);
end;

end.
