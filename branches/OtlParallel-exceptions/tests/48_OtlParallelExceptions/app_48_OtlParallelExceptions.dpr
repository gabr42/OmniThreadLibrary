program app_48_OtlParallelExceptions;

uses
  FastMM4,
  Forms,
  test_48_OtlParallelExceptions in 'test_48_OtlParallelExceptions.pas' {Form34};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm34, Form34);
  Application.Run;
end.
