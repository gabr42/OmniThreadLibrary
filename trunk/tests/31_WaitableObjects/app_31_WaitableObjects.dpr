program app_31_WaitableObjects.XE3;

uses
  FastMM4,
  Forms,
  test_31_WaitableObjects in 'test_31_WaitableObjects.pas' {frmTestWaitableObjects};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestWaitableObjects, frmTestWaitableObjects);
  Application.Run;
end.
