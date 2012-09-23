program app_39_Futures.XE3;

uses
  Forms,
  test_39_Futures in 'test_39_Futures.pas' {frmFuturesDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFuturesDemo, frmFuturesDemo);
  Application.Run;
end.
