program app_2_TwoWayHello_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  test_2_TwoWayHello_FMX in 'test_2_TwoWayHello_FMX.pas' {frmTwoWayHello};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTwoWayHello, frmTwoWayHello);
  Application.Run;
end.
