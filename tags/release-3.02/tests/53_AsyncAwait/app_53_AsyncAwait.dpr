program app_53_AsyncAwait;

uses
  Forms,
  test_53_AsyncAwait in 'test_53_AsyncAwait.pas' {frmAsyncAwaitTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAsyncAwaitTest, frmAsyncAwaitTest);
  Application.Run;
end.
