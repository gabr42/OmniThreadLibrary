program app_54_LockManager;

uses
  Vcl.Forms,
  test_54_LockManager in 'test_54_LockManager.pas' {frmTestLockManager};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestLockManager, frmTestLockManager);
  Application.Run;
end.
