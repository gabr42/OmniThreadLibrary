program app_38_OrderedFor;

uses
  Forms,
  test_38_OrderedFor in 'test_38_OrderedFor.pas' {frmOderedForDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmOderedForDemo, frmOderedForDemo);
  Application.Run;
end.
