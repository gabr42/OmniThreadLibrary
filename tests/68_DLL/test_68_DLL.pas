unit test_68_DLL;

// This program doesn't use OmniThreadLibrary. Only the DLL does.

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls,
  api_68_DLL;

type
  TfrmTestDLL = class(TForm)
    btnLoad: TButton;
    btnStart: TButton;
    btnStartStubborn: TButton;
    btnCancel: TButton;
    btnUnload: TButton;
    lblProgress: TLabel;
    lbLog: TListBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStartStubbornClick(Sender: TObject);
    procedure btnUnloadClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  strict private
    FCancelWork      : TCancelWork;
    FDll             : HMODULE;
    FPrepareForUnload: TPrepareForUnload;
    FStartWork       : TStartWork;
  strict protected
    function  GetFunction(const name: string): pointer;
    procedure Log(const msg: string; const params: array of const);
    procedure StartWork(ignoreCancel: boolean);
    function  UnloadDll: boolean;
    procedure UpdateButtons;
    procedure WMJobCompleted(var msg: TMessage); message WM_JOB_COMPLETED;
    procedure WMJobProgress(var msg: TMessage); message WM_JOB_PROGRESS;
  end; { TfrmTestDLL }

var
  frmTestDLL: TfrmTestDLL;

implementation

{$R *.dfm}

const
  CMaxNumber        = 20000000;
  CUnloadTimeout_ms = 5000;

procedure TfrmTestDLL.btnCancelClick(Sender: TObject);
begin
  FCancelWork;
end; { TfrmTestDLL.btnCancelClick }

procedure TfrmTestDLL.btnLoadClick(Sender: TObject);
var
  lastError: DWORD;
begin
  FDll := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + CDllName));
  if FDll = 0 then begin
    lastError := GetLastError;
    Log('Failed to load %s. [%d] %s', [CDllName, lastError, SysErrorMessage(lastError)]);
    Exit;
  end;
  FStartWork := GetFunction('StartWork');
  FCancelWork := GetFunction('CancelWork');
  FPrepareForUnload := GetFunction('PrepareForUnload');
  Log('DLL loaded', []);
  UpdateButtons;
end; { TfrmTestDLL.btnLoadClick }

procedure TfrmTestDLL.btnStartClick(Sender: TObject);
begin
  StartWork(false);
end; { TfrmTestDLL.btnStartClick }

procedure TfrmTestDLL.btnStartStubbornClick(Sender: TObject);
begin
  StartWork(true);
end; { TfrmTestDLL.btnStartStubbornClick }

procedure TfrmTestDLL.btnUnloadClick(Sender: TObject);
begin
  UnloadDll;
end; { TfrmTestDLL.btnUnloadClick }

procedure TfrmTestDLL.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Windows kills all threads when the program exits and they may be in the middle of something.
  // Stop the DLL first.
  CanClose := UnloadDll;
end; { TfrmTestDLL.FormCloseQuery }

procedure TfrmTestDLL.FormCreate(Sender: TObject);
begin
  UpdateButtons;
end; { TfrmTestDLL.FormCreate }

function TfrmTestDLL.GetFunction(const name: string): pointer;
var
  lastError: DWORD;
begin
  Result := GetProcAddress(FDll, PChar(name));
  if not assigned(Result) then begin
    lastError := GetLastError;
    raise Exception.CreateFmt('TfrmTestDLL.GetFunction: %s not found in %s. [%d] %s',
      [name, CDllName, lastError, SysErrorMessage(lastError)]);
  end;
end; { TfrmTestDLL.GetFunction }

procedure TfrmTestDLL.Log(const msg: string; const params: array of const);
begin
  lbLog.ItemIndex := lbLog.Items.Add(FormatDateTime('hh:nn:ss.zzz ', Now) + Format(msg, params));
end; { TfrmTestDLL.Log }

procedure TfrmTestDLL.StartWork(ignoreCancel: boolean);
var
  job: integer;
begin
  job := FStartWork(Handle, CMaxNumber, ignoreCancel);
  if job > 0 then
    Log('Job %d started', [job])
  else
    Log('DLL is being stopped, job was not started', []);
end; { TfrmTestDLL.StartWork }

function TfrmTestDLL.UnloadDll: boolean;
begin
  Result := true;
  if FDll = 0 then
    Exit;

  Log('Stopping the DLL', []);
  if not FPrepareForUnload(CUnloadTimeout_ms) then begin
    Log('Some job is still running. DLL was NOT unloaded, try again later.', []);
    Result := false;
    Exit;
  end;

  // All threads were stopped. DLL can be safely unloaded.
  FStartWork := nil;
  FCancelWork := nil;
  FPrepareForUnload := nil;
  FreeLibrary(FDll);
  FDll := 0;
  Log('DLL unloaded', []);
  UpdateButtons;
end; { TfrmTestDLL.UnloadDll }

procedure TfrmTestDLL.UpdateButtons;
begin
  btnLoad.Enabled := (FDll = 0);
  btnStart.Enabled := (FDll <> 0);
  btnStartStubborn.Enabled := (FDll <> 0);
  btnCancel.Enabled := (FDll <> 0);
  btnUnload.Enabled := (FDll <> 0);
end; { TfrmTestDLL.UpdateButtons }

procedure TfrmTestDLL.WMJobCompleted(var msg: TMessage);
begin
  // Messages can arrive after the DLL is unloaded, so don't call the DLL here.
  if msg.LParam < 0 then
    Log('Job %d was cancelled', [msg.WParam])
  else
    Log('Job %d completed, result = %d', [msg.WParam, msg.LParam]);
end; { TfrmTestDLL.WMJobCompleted }

procedure TfrmTestDLL.WMJobProgress(var msg: TMessage);
begin
  lblProgress.Caption := Format('Job %d: %d %%', [msg.WParam, msg.LParam]);
end; { TfrmTestDLL.WMJobProgress }

end.
