unit fsMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl, System.Actions,
  Vcl.ActnList, Vcl.Samples.Spin,
  OtlCommon,
  OtlCollections,
  OtlTask,
  OtlParallel;

type
  TfrmFolderScanner = class(TForm)
    ActionList    : TActionList;
    actStart      : TAction;
    actStop       : TAction;
    btnSelect     : TButton;
    btnStart      : TButton;
    inpFolder     : TEdit;
    inpNumScanners: TSpinEdit;
    lblFolder     : TLabel;
    lblNumScanners: TLabel;
    lbLog: TListBox;
    procedure actStartExecute(Sender: TObject);
    procedure actStartUpdate(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
  private
    FCancelProcessing    : boolean;
    FFolderScanner       : IOmniPipeline;
    FNumFolders          : TOmniAlignedInt32;
    FNumFoldersInPipeline: TOmniAlignedInt32;
  protected
    procedure DataCollector_asy(const input, output: IOmniBlockingCollection);
    procedure FolderScanner_asy(const input, output: IOmniBlockingCollection;
      const task: IOmniTask);
    procedure UniqueFilter_asy(const input, output: IOmniBlockingCollection);
    procedure WorkDone;
  public
  end;

var
  frmFolderScanner: TfrmFolderScanner;

implementation

type
  TFileInfo = class
  strict private
    FFileSize: int64;
    FNumFiles: integer;
  public
    property FileSize: int64 read FFileSize write FFileSize;
    property NumFiles: integer read FNumFiles write FNumFiles;
  end;

{$R *.dfm}

procedure TfrmFolderScanner.actStartExecute(Sender: TObject);
begin
  btnStart.Action := actStop;

  FNumFolders.Value := 0;
  FNumFoldersInPipeline.Value := 1;
  FCancelProcessing := false;

  FFolderScanner := Parallel.Pipeline
    .NoThrottling
    .Stage(UniqueFilter_asy)
          // filters out duplicates (in case of symlink loops); also ensures that folder names include trailing backslash
    .Stage(FolderScanner_asy)          // collects information about files and feeds subfolders back into the pipeline
      .NumTasks(inpNumScanners.Value)
    .Stage(DataCollector_asy,          // summarizes file statistics
      Parallel.TaskConfig.OnTerminated(WorkDone))
    .Run;

  FFolderScanner.Input.Add(inpFolder.Text);

  lbLog.ItemIndex := lbLog.Items.Add('Pipeline started');
end;

procedure TfrmFolderScanner.actStartUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (Trim(inpFolder.Text) <> '')
    and System.SysUtils.DirectoryExists(inpFolder.Text);
end;

procedure TfrmFolderScanner.actStopExecute(Sender: TObject);
var
  fileInfo: TFileInfo;
begin
  if assigned(FFolderScanner) then begin
    FCancelProcessing := true;
    FFolderScanner.Input.CompleteAdding;

    fileInfo := FFolderScanner.Output.Next;
    lbLog.ItemIndex := lbLog.Items.Add(Format('%d folders, %d files, %d bytes',
      [FNumFolders.Value, fileInfo.NumFiles, fileInfo.FileSize]));

    FFolderScanner.WaitFor(INFINITE);
    FFolderScanner := nil;
    lbLog.ItemIndex := lbLog.Items.Add('Pipeline stopped');
  end;

  btnStart.Action := actStart;
end;

procedure TfrmFolderScanner.btnSelectClick(Sender: TObject);
var
  folders: TArray<string>;
begin
  if SelectDirectory(inpFolder.Text, folders) then
    inpFolder.Text := folders[0];
end;

procedure TfrmFolderScanner.DataCollector_asy(const input, output:
  IOmniBlockingCollection);
var
  fileInfo  : TFileInfo;
  folderInfo: TFileInfo;
  inValue   : TOmniValue;
begin
  fileInfo := TFileInfo.Create;
  for inValue in input do begin
    folderInfo := inValue;
    fileInfo.NumFiles := fileInfo.NumFiles + folderInfo.NumFiles;
    fileInfo.FileSize := fileInfo.FileSize + folderInfo.FileSize;
    folderInfo.Free;
  end;

  output.Add(fileInfo);
end;

procedure TfrmFolderScanner.FolderScanner_asy(const input, output:
  IOmniBlockingCollection; const task: IOmniTask);
var
  fileInfo: TFileInfo;
  inValue : TOmniValue;
  SR      : TSearchRec;
begin
  for inValue in input do begin
    if FCancelProcessing then
      Exit;

    if FindFirst(inValue.AsString + '*.*', faAnyFile , SR) = 0 then try

      fileInfo := TFileInfo.Create;
      repeat
        if (faDirectory AND SR.Attr) <> 0 then begin
          if (SR.Name <> '.') and (SR.Name <> '..') then begin
            if not FFolderScanner.Input.TryAdd(inValue.AsString + SR.Name) then
              break; //external stop request
            FNumFoldersInPipeline.Increment;
          end;
        end
        else begin
          fileInfo.NumFiles := fileInfo.NumFiles + 1;
          fileInfo.FileSize := fileInfo.FileSize + SR.Size;
        end;
      until (FindNext(SR) <> 0) or FCancelProcessing;

      if fileInfo.NumFiles = 0 then
        fileInfo.Free
      else if not output.TryAdd(fileInfo) then // next stage will take the ownership
       break; //external stop request

    finally
      FindClose(SR);
    end;

    if FNumFoldersInPipeline.Decrement = 0 then
      FFolderScanner.Input.CompleteAdding;
  end;
end;

procedure TfrmFolderScanner.UniqueFilter_asy(const input, output:
  IOmniBlockingCollection);
var
  uniqueUrls: TStringList;
  url       : string;
  urlDelim  : string;
begin
  uniqueUrls := TStringList.Create;
  try
    uniqueUrls.Sorted := true;

    for url in input do begin
      urlDelim := IncludeTrailingPathDelimiter(url);
      if uniqueUrls.IndexOf(urlDelim) < 0 then begin
        uniqueUrls.Add(urlDelim);
        output.TryAdd(urlDelim);
      end
      else if FNumFoldersInPipeline.Decrement = 0 then
        FFolderScanner.Input.CompleteAdding;
    end;

    FNumFolders.Value := uniqueUrls.Count;

  finally FreeAndNil(uniqueUrls); end;
end;

procedure TfrmFolderScanner.WorkDone;
begin
  actStop.Execute;
end;

end.
