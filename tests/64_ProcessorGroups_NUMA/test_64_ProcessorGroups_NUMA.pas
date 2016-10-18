unit test_64_ProcessorGroups_NUMA;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  OtlComm;

const
  MSG_LOG = WM_USER;

type
  TfrmProcessorGroupsNUMA = class(TForm)
    btnScheduleTask   : TButton;
    btnStartInNumaNode: TButton;
    btnStartProcGroup : TButton;
    inpAffinity       : TEdit;
    inpNUMANode       : TSpinEdit;
    inpNUMANodes      : TEdit;
    inpProcessorGroups: TEdit;
    inpProcGroup      : TSpinEdit;
    Label1            : TLabel;
    lblAffinity       : TLabel;
    lbLog             : TListBox;
    lblProcessorGroups: TLabel;
    btnScheduleAllGroups: TButton;
    btnSchedulAllNodes: TButton;
    procedure btnSchedulAllNodesClick(Sender: TObject);
    procedure btnScheduleAllGroupsClick(Sender: TObject);
    procedure btnScheduleTaskClick(Sender: TObject);
    procedure btnStartInNumaNodeClick(Sender: TObject);
    procedure btnStartProcGroupClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure DisplayInfo;
    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const params: array of const); overload;
    function  MapToIntegerArray(const s: string): TArray<integer>;
    procedure WMMsgLog(var msg: TOmniMessage); message MSG_LOG;
  public
  end;

var
  frmProcessorGroupsNUMA: TfrmProcessorGroupsNUMA;

implementation

uses
  DSiWin32,
  OtlTask,
  OtlTaskControl,
  OtlCommon,
  OtlThreadPool;

{$R *.dfm}

procedure TestWorker(const task: IOmniTask);
var
  groupAffinity: TOmniGroupAffinity;
begin
  groupAffinity := Environment.Thread.GroupAffinity;
  task.Comm.Send(MSG_LOG, Format('Thread affinity: group %d, mask %.16x',
    [groupAffinity.Group, groupAffinity.Affinity.AsMask]));
end;

{ TfrmProcessorGroupsNUMA }

procedure TfrmProcessorGroupsNUMA.btnSchedulAllNodesClick(Sender: TObject);
begin
  GlobalOmniThreadPool.ProcessorGroups.Clear;
  GlobalOmniThreadPool.NUMANodes := Environment.NUMANodes.All;

  Log('Max executing: %d', [GlobalOmniThreadPool.MaxExecuting]);

  CreateTask(TestWorker, 'Scheduled task')
    .OnMessage(Self)
    .Schedule;
end;

procedure TfrmProcessorGroupsNUMA.btnScheduleAllGroupsClick(Sender: TObject);
begin
  GlobalOmniThreadPool.ProcessorGroups := Environment.ProcessorGroups.All;
  GlobalOmniThreadPool.NUMANodes.Clear;

  Log('Max executing: %d', [GlobalOmniThreadPool.MaxExecuting]);

  CreateTask(TestWorker, 'Scheduled task')
    .OnMessage(Self)
    .Schedule;
end;

procedure TfrmProcessorGroupsNUMA.btnScheduleTaskClick(Sender: TObject);
var
  c   : integer;
  mask: cardinal;
begin
  if inpAffinity.Text <> '' then begin
    Val('$' + inpAffinity.Text, mask, c);
    if c <> 0 then
      raise Exception.Create('Invalid value: ' + inpAffinity.Text);
    GlobalOmniThreadPool.Affinity.AsMask := mask;
  end;

  if inpProcessorGroups.Text <> '' then
    GlobalOmniThreadPool.ProcessorGroups.AsArray := MapToIntegerArray(inpProcessorGroups.Text);

  if inpNUMANodes.Text <> '' then
    GlobalOmniThreadPool.NUMANodes.AsArray := MapToIntegerArray(inpNUMANodes.Text);

  Log('Max executing: %d', [GlobalOmniThreadPool.MaxExecuting]);

  CreateTask(TestWorker, 'Scheduled task')
    .OnMessage(Self)
    .Schedule;
end;

procedure TfrmProcessorGroupsNUMA.btnStartInNumaNodeClick(Sender: TObject);
begin
  CreateTask(TestWorker, 'NUMANode task')
    .NUMANode(inpNUMANode.Value)
    .OnMessage(Self)
    .Unobserved
    .Run;
end;

procedure TfrmProcessorGroupsNUMA.btnStartProcGroupClick(Sender: TObject);
begin
  CreateTask(TestWorker, 'ProcessorGroup task')
    .ProcessorGroup(inpProcGroup.Value)
    .OnMessage(Self)
    .Unobserved
    .Run;
end;

procedure TfrmProcessorGroupsNUMA.DisplayInfo;
var
  i: integer;
begin
  Log('Processor groups');
  for i := 0 to Environment.ProcessorGroups.Count - 1 do
    Log(Format('%d: Mask: %.16x', [
      Environment.ProcessorGroups[i].GroupNumber,
      Environment.ProcessorGroups[i].Affinity.AsMask]));
  Log('');

  Log('NUMA nodes');
  for i := 0 to Environment.NUMANodes.Count - 1 do
    Log(Format('%d: Group: %d, Mask: %.16x', [
      Environment.NUMANodes[i].NodeNumber,
      Environment.NUMANodes[i].GroupNumber,
      Environment.NUMANodes[i].Affinity.AsMask]));
end;

procedure TfrmProcessorGroupsNUMA.FormShow(Sender: TObject);
begin
  OnShow := nil;
  DisplayInfo;
end;

procedure TfrmProcessorGroupsNUMA.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmProcessorGroupsNUMA.Log(const msg: string; const params: array of const);
begin
  Log(Format(msg, params));
end;

function TfrmProcessorGroupsNUMA.MapToIntegerArray(const s: string): TArray<integer>;
var
  arr: TArray<string>;
  i  : integer;
begin
  arr := s.Split([',']);
  SetLength(Result, Length(arr));
  for i := Low(Result) to High(Result) do
    Result[i] := StrToInt(arr[i]);
end;

procedure TfrmProcessorGroupsNUMA.WMMsgLog(var msg: TOmniMessage);
begin
  Log(msg.MsgData);
end;

end.
