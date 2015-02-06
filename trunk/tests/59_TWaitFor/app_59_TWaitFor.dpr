program app_59_TWaitFor;

uses
  Forms,
  test_59_TWaitFor in 'test_59_TWaitFor.pas' {frmTestTWaitFor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestTWaitFor, frmTestTWaitFor);
  Application.Run;
end.
