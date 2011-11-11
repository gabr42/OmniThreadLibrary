program app_51_BackgroundWorker;

uses
  FastMM4,
  Forms,
  test_51_BackgroundWorker in 'test_51_BackgroundWorker.pas' {Form16};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm16, Form16);
  Application.Run;
end.
