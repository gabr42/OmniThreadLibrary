program app_24_ConnectionPool.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_24_ConnectionPool in 'test_24_ConnectionPool.pas' {frmConnectionPoolDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmConnectionPoolDemo, frmConnectionPoolDemo);
  Application.Run;
end.
