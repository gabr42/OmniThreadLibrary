program app_53_AsyncAwait.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_53_AsyncAwait in 'test_53_AsyncAwait.pas' {frmAsyncAwaitTest};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAsyncAwaitTest, frmAsyncAwaitTest);
  Application.Run;
end.
