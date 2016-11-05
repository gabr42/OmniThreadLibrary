program FolderScanner;

uses
  Vcl.Forms,
  fsMain in 'fsMain.pas' {frmFolderScanner};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFolderScanner, frmFolderScanner);
  Application.Run;
end.
