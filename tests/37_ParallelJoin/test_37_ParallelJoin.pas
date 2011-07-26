unit test_37_ParallelJoin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmTestParallelJoin = class(TForm)
    btnJoinAll: TButton;
    btnJointOne: TButton;
    lbLog: TListBox;
    btnJoinTProc: TButton;
    procedure btnJoinAllClick(Sender: TObject);
    procedure btnJoinTProcClick(Sender: TObject);
  private
  protected
    procedure Log(const msg: string);
  public
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

procedure TfrmTestParallelJoin.btnJoinAllClick(Sender: TObject);
var
  expectedTime: integer;
  join        : IOmniParallelJoin;
  startTime   : int64;
begin
  if Environment.Process.Affinity.Count = 1 then
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
  if Sender = btnJointOne then
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

procedure TfrmTestParallelJoin.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn:ss', Now) + ' ' + msg);
end;

end.
