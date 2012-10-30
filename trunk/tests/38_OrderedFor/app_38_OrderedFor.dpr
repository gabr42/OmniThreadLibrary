program app_38_OrderedFor.XE3;

{$R 'MainIcon.res' '..\..\res\MainIcon.rc'}

uses
  FastMM4,
  Forms,
  test_38_OrderedFor in 'test_38_OrderedFor.pas' {frmOderedForDemo};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmOderedForDemo, frmOderedForDemo);
  Application.Run;
end.
