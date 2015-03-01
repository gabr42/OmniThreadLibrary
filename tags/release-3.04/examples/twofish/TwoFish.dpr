program TwoFish;

uses
  Vcl.Forms,
  twofish1 in 'twofish1.pas' {frmTwoFish},
  twoFishDB in 'twoFishDB.pas' {dmTwoFishDB: TDataModule},
  twoFishDB_GUI in 'twoFishDB_GUI.pas' {frmTwoFishDB_GUI: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTwoFish, frmTwoFish);
  Application.Run;
end.
