unit test_49_FramedWorkers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  OtlTaskControl,
  test_49_FrameWithWorker,
  test_49_Worker;

type
  TfrmFramedWorkers = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTaskGroup: IOmniTaskGroup;
    function  CreateFrame(left, top, width, height: integer; const name: string):
      TfrmFrameWithWorker;
    procedure CreateWorker(frame: TfrmFrameWithWorker; const caption: string);
  public
  end;

var
  frmFramedWorkers: TfrmFramedWorkers;

implementation

uses
  OtlCommon;

{$R *.dfm}

const
  CNumFrames = 5;
  CFrameWidth = 150;
  CFrameHeight = 200;

procedure TfrmFramedWorkers.FormDestroy(Sender: TObject);
begin
  FTaskGroup.TerminateAll;
end;

function TfrmFramedWorkers.CreateFrame(left, top, width, height: integer;
  const name: string): TfrmFrameWithWorker;
begin
  Result := TfrmFrameWithWorker.Create(Self);
  Result.Parent := Self;
  Result.Left := left;
  Result.Top := top;
  Result.Width := width;
  Result.Height := height;
  Result.Name := name;
end;

procedure TfrmFramedWorkers.CreateWorker(frame: TfrmFrameWithWorker;
  const caption: string);
var
  worker: IOmniTaskControl;
begin
  worker := CreateTask(TFramedWorker.Create(), caption)
    .SetParameter('Msg', caption)
    .OnMessage(frame)
    .Run;
  frame.Worker := worker;
  FTaskGroup.Add(worker);
end;

procedure TfrmFramedWorkers.FormCreate(Sender: TObject);
var
  frame : TfrmFrameWithWorker;
  iFrame: integer;
begin
  FTaskGroup := CreateTaskGroup;
  for iFrame := 1 to CNumFrames do begin
    frame := CreateFrame(CFrameWidth * (iFrame - 1), 0, CFrameWidth, CFrameHeight,
      Format('Frame%d', [iFrame]));
    CreateWorker(frame, Format('Frame #%d', [iFrame]));
  end;
  ClientWidth := CNumFrames * CFrameWidth;
  ClientHeight := CFrameHeight;
end;

end.
