program app_39_Futures.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  Forms,
  test_39_Futures in 'test_39_Futures.pas' {frmFuturesDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFuturesDemo, frmFuturesDemo);
  Application.Run;
end.
