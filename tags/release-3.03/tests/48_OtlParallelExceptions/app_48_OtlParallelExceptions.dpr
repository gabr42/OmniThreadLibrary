program app_48_OtlParallelExceptions.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_48_OtlParallelExceptions in 'test_48_OtlParallelExceptions.pas' {frmOtlParallelExceptions};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmOtlParallelExceptions, frmOtlParallelExceptions);
  Application.Run;
end.
