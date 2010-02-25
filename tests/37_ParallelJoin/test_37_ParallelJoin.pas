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
    procedure btnJoinAllClick(Sender: TObject);
    procedure btnJointOneClick(Sender: TObject);
  private
  protected
    procedure Log(const msg: string);
  public
  end;

var
  frmTestParallelJoin: TfrmTestParallelJoin;

implementation

uses
  OtlCommon,
  OtlTask,
  OtlParallel;

{$R *.dfm}

procedure TfrmTestParallelJoin.btnJoinAllClick(Sender: TObject);
begin
  Log('Starting two tasks');
  Parallel.Join(
    procedure (const task: IOmniTask)
    begin
      Sleep(3000);
    end,
    procedure (const task: IOmniTask)
    begin
      Sleep(2000);
    end);
  Log('Tasks stopped');
end;

procedure TfrmTestParallelJoin.btnJointOneClick(Sender: TObject);
begin
  Environment.Process.Affinity.Count := 1;
  btnJoinAllClick(Sender);
  Environment.Process.Affinity.Count := Environment.System.Affinity.Count;
end;

procedure TfrmTestParallelJoin.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn:ss', Now) + ' ' + msg);
end;

end.
