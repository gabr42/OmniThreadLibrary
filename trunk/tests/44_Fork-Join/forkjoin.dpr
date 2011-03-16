program forkjoin;

uses
  Forms,
  forkjoin1 in 'C:\Users\gabr\Documents\RAD Studio\Projects\forkjoin1.pas' {Form22};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm22, Form22);
  Application.Run;
end.
