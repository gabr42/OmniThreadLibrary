unit test_46_Async;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlComm;

type
  TfrmDemoParallelAsync = class(TForm)
    btnAsync: TButton;
    lbLog: TListBox;
    procedure btnAsyncClick(Sender: TObject);
  private
  public
  end;

var
  frmDemoParallelAsync: TfrmDemoParallelAsync;

implementation

uses
  OtlTask,
  OtlParallel;

{$R *.dfm}

procedure TfrmDemoParallelAsync.btnAsyncClick(Sender: TObject);
var
  i: integer;
begin
  btnAsync.Enabled := false;

  Parallel.Async(
    procedure
    begin
      Sleep(500);
      MessageBeep($FFFFFFFF);
    end,

    procedure
    begin
      btnAsync.Enabled := true;
    end
  );
end;

end.
