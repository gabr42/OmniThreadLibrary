program app_28_Hooks.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_28_Hooks in 'test_28_Hooks.pas' {frmHooksDemo},
  OtlHooks in '..\..\OtlHooks.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmHooksDemo, frmHooksDemo);
  Application.Run;
end.
