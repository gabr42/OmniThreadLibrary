program app_66_threadsInThreads;

uses
  Forms,
  test_66_threadsInThreads in 'test_66_threadsInThreads.pas' {frmThreadInThreads};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmThreadInThreads, frmThreadInThreads);
  Application.Run;
end.
