unit twofish1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, IBCustomDataSet, IBTable, IBDatabase,
  twoFishDB_GUI;

type
  TfrmTwoFish = class(TForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    FFrames: array of TfrmTwoFishDB_GUI;
    function  CreateFrame(left, top, width, height: integer;
      const name: string): TfrmTwoFishDB_GUI;
    procedure OpenConnections;
  public
  end;

var
  frmTwoFish: TfrmTwoFish;

implementation

{$R *.dfm}

const
  CNumFrames = 2;
  CFrameWidth = 512;
  CFrameHeight = 400;

  CDatabaseName = 'C:\Users\Public\Documents\RAD Studio\9.0\Samples\Data\dbdemos.gdb';

function TfrmTwoFish.CreateFrame(left, top, width, height: integer;
  const name: string): TfrmTwoFishDB_GUI;
begin
  Result := TfrmTwoFishDB_GUI.Create(Self);
  Result.Parent := Self;
  Result.Left := left;
  Result.Top := top;
  Result.Width := width;
  Result.Height := height;
  Result.Name := name;
end;

procedure TfrmTwoFish.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  frame: TfrmTwoFishDB_GUI;
begin
  for frame in FFrames do
    frame.CloseConnection;
end;

procedure TfrmTwoFish.FormCreate(Sender: TObject);
var
  iFrame: integer;
begin
  SetLength(FFrames, CNumFrames);
  for iFrame := 0 to CNumFrames-1 do
    FFrames[iFrame] := CreateFrame(CFrameWidth * iFrame, 0, CFrameWidth, CFrameHeight,
      Format('Frame%d', [iFrame+1]));
  ClientWidth := CNumFrames * CFrameWidth;
  ClientHeight := CFrameHeight;
  OpenConnections;
end;

procedure TfrmTwoFish.OpenConnections;
var
  frame: TfrmTwoFishDB_GUI;
begin
  for frame in FFrames do
    frame.OpenConnection(CDatabaseName ,
      procedure (Sender: TObject; FatalException: Exception)
      begin
        if assigned(FatalException) then
          ShowMessage('Failed to connect to the database! ' + FatalException.Message)
        else
          (Sender as TfrmTwoFishDB_GUI).Reload;
      end);
end;

end.
