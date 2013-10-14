unit test_23_BackgroundFileSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  TfrmBackgroundFileSearchDemo = class(TForm)
    btnScan         : TButton;
    inpFolderMask   : TLabeledEdit;
    lbFiles         : TListBox;
    OTLMonitor      : TOmniEventMonitor;
    outFiles        : TLabeledEdit;
    outScanning     : TLabeledEdit;
    tmrDisplayStatus: TTimer;
    procedure btnScanClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure OTLMonitorTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure OTLMonitorTaskTerminated(const task: IOmniTaskControl);
    procedure tmrDisplayStatusTimer(Sender: TObject);
  private
    FFileList      : TStringList;
    FScanTask      : IOmniTaskControl;
    FWaitingCount  : string;
    FWaitingMessage: string;
    procedure StopScan(Sender: TObject);
  end;

var
  frmBackgroundFileSearchDemo: TfrmBackgroundFileSearchDemo;

implementation

uses
  OtlCommon;

{$R *.dfm}

const
  MSG_SCAN_FOLDER  = 1;
  MSG_FOLDER_FILES = 2;

procedure ScanFolder(const task: IOmniTask; const folder, mask: string);
var
  err        : integer;
  folderFiles: TStringList;
  S          : TSearchRec;
begin
  err := FindFirst(folder + '*.*', faDirectory, S);
  if err = 0 then try
    repeat
      if ((S.Attr and faDirectory) <> 0) and (S.Name <> '.') and (S.Name <> '..') then 
        ScanFolder(task, folder + S.Name + '\', mask);
      err := FindNext(S);
    until task.Terminated or (err <> 0);
  finally FindClose(S); end;
  task.Comm.Send(MSG_SCAN_FOLDER, folder);
  folderFiles := TStringList.Create;
  try
    err := FindFirst(folder + mask, 0, S);
    if err = 0 then try
      repeat
        folderFiles.Add(folder + S.Name);
        err := FindNext(S);
      until task.Terminated or (err <> 0);
    finally FindClose(S); end;
  finally task.Comm.Send(MSG_FOLDER_FILES, folderFiles); end;
end;

procedure ScanFolders(const task: IOmniTask);
var
  folder: string;
  mask  : string;
begin
  mask := task.Param['FolderMask'];
  folder := ExtractFilePath(mask);
  Delete(mask, 1, Length(folder));
  if folder <> '' then
    folder := IncludeTrailingPathDelimiter(folder);
  ScanFolder(task, folder, mask);
end;

{ TfrmBackgroundFileSearchDemo }

procedure TfrmBackgroundFileSearchDemo.btnScanClick(Sender: TObject);
begin
  FFileList := TStringList.Create;
  btnScan.Caption := 'Stop';
  btnScan.OnClick := StopScan;
  tmrDisplayStatus.Enabled := true;
  FScanTask := CreateTask(ScanFolders, 'ScanFolders')
    .MonitorWith(OTLMonitor)
    .SetParameter('FolderMask', inpFolderMask.Text)
    .Run;
end;

procedure TfrmBackgroundFileSearchDemo.FormCloseQuery(Sender: TObject; var CanClose:
  boolean);
begin
  if assigned(FScanTask) then begin
    FScanTask.Terminate;
    FScanTask := nil;
    CanClose := true;
  end;
end;

procedure TfrmBackgroundFileSearchDemo.OTLMonitorTaskMessage(const task:
  IOmniTaskControl; const msg: TOmniMessage);
var
  folderFiles: TStringList;
begin
  if msg.MsgID = MSG_SCAN_FOLDER then
    FWaitingMessage := msg.MsgData
  else if msg.MsgID = MSG_FOLDER_FILES then begin
    folderFiles := TStringList(msg.MsgData.AsObject);
    FFileList.AddStrings(folderFiles);
    FreeAndNil(folderFiles);
    FWaitingCount := IntToStr(FFileList.Count);
  end;
end;

procedure TfrmBackgroundFileSearchDemo.OTLMonitorTaskTerminated(const task:
  IOmniTaskControl);
begin
  tmrDisplayStatus.Enabled := false;
  outScanning.Text := '';
  outFiles.Text := IntToStr(FFileList.Count);
  lbFiles.Clear;
  lbFiles.Items.AddStrings(FFileList);
  FreeAndNil(FFileList);
  FScanTask := nil;
  btnScan.Caption := 'Start';
  btnScan.OnClick := btnScanClick;
end;

procedure TfrmBackgroundFileSearchDemo.StopScan(Sender: TObject);
begin
  FScanTask.Terminate; // APP Hangs here
  FScanTask := nil;
  btnScan.Caption := 'Start';
  btnScan.OnClick := btnScanClick;
end;

procedure TfrmBackgroundFileSearchDemo.tmrDisplayStatusTimer(Sender: TObject);
begin
  if FWaitingMessage <> '' then begin
    outScanning.Text := FWaitingMessage;
    FWaitingMessage := '';
  end;
  if FWaitingCount <> '' then begin
    outFiles.Text := FWaitingCount;
    FWaitingCount := '';
  end;
end;

end.
