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
  OtlTaskControl,
  OtlParallel;

{$R *.dfm}

procedure TfrmDemoParallelAsync.btnAsyncClick(Sender: TObject);
begin
  btnAsync.Enabled := false;

  Parallel.Async(
    procedure
    begin
      // executed in background thread
      Sleep(500);
      MessageBeep($FFFFFFFF);
    end,
    Parallel.TaskConfig.OnTerminated(
      procedure (const task: IOmniTaskControl)
      begin
        // executed in main thread
        btnAsync.Enabled := true;
      end
    )
  );
end;

end.
