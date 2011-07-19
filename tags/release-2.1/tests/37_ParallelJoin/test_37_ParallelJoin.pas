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
    procedure btnJointOneClick(Sender: TObject);
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
  startTime   : int64;
begin
  if Environment.Process.Affinity.Count = 1 then
    expectedTime := 5
  else
    expectedTime := 3;
  Log(Format('Starting two tasks, expected execution time is %d seconds', [expectedTime]));
  startTime := DSiTimeGetTime64;
  Parallel.Join(
    procedure (const task: IOmniTask)
    begin
      Sleep(3000);
    end,
    procedure (const task: IOmniTask)
    begin
      Sleep(2000);
    end);
  Log(Format('Tasks stopped, execution time was %s seconds',
    [FormatDateTime('s.zzz', DSiElapsedTime64(startTime)/MSecsPerDay)]));
end;

procedure TfrmTestParallelJoin.btnJointOneClick(Sender: TObject);
begin
  Environment.Process.Affinity.Count := 1;
  btnJoinAllClick(Sender);
  Environment.Process.Affinity.Count := Environment.System.Affinity.Count;
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
    end);
  Log(Format('Tasks stopped, execution time was %s seconds',
    [FormatDateTime('s.zzz', DSiElapsedTime64(startTime)/MSecsPerDay)]));
end;

procedure TfrmTestParallelJoin.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn:ss', Now) + ' ' + msg);
end;

end.
