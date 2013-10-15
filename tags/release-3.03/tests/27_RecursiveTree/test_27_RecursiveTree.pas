unit test_27_RecursiveTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin,
  DSiWin32,
  OtlCommon,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlEventMonitor;

type
  PNode = ^TNode;
  TNodeArray = array [1..1] of PNode;
  TNode = packed record
  public
    Value: int64;
  private
    SubNodes: TNodeArray; // must be defined last because it will be allocated dynamically
    function  GetChild(idxChild: integer): PNode;
    procedure SetChild(idxChild: integer; value: PNode);
  public
    function IsLeaf: boolean; inline;
    property Child[idxChild: integer]: PNode read GetChild write SetChild;
  end;

  TfrmRecursiveTreeDemo = class(TForm)
    btnBuildTree     : TButton;
    btnMultiCoreTest : TButton;
    btnSingleCoreTest: TButton;
    inpNumChildren   : TSpinEdit;
    inpNumTasks      : TSpinEdit;
    inpTreeDepth     : TSpinEdit;
    lblNumChildren   : TLabel;
    lblNumNodes      : TLabel;
    lblNumTasks      : TLabel;
    lbLog            : TListBox;
    lblTreeDepth     : TLabel;
    lblTreeSize      : TLabel;
    outNumNodes      : TEdit;
    outTreeSize      : TEdit;
    OtlMonitor: TOmniEventMonitor;
    procedure btnBuildTreeClick(Sender: TObject);
    procedure btnMultiCoreTestClick(Sender: TObject);
    procedure btnSingleCoreTestClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure inpNumChildrenChange(Sender: TObject);
    procedure OtlMonitorTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure OtlMonitorTaskTerminated(const task: IOmniTaskControl);
  private
    FRebuildTree : boolean;
    FRoot        : PNode;
    FRootTask: IOmniTaskControl;
    FTaskMessages: TStringList;
    FTimeStart   : int64;
    FValue       : integer;
    function  AllocateNode: PNode;
    procedure CalcNumNodes;
    procedure ClearNode(node: PNode);
    procedure ClearTree;
    procedure CreateChildren(node: PNode; subLevels: integer);
    procedure DestroyNode(node: PNode);
    procedure DestroyTree;
    procedure Log(const msg: string);
    procedure ProcessTree(node: PNode);
  end; { TfrmRecursiveTreeDemo }

var
  frmRecursiveTreeDemo: TfrmRecursiveTreeDemo;

implementation

{$R *.dfm}

const
  CPruneLevel = 4; // don't create new task if under this level; empirically determined value

var
  GNumChildren: integer; //number of children on each level, global for simplicity

function FmtMem(mem: int64): string;
begin
  if mem > 1024*1024*1024 then
    Result := Format('%.1n GB',[mem/1024/1024/1024])
  else if mem > 2024*1024 then
    Result := Format('%.0n MB',[mem/1024/1024])
  else if mem > 2024 then
    Result := Format('%.0n KB',[mem/1024])
  else
    Result := Format('%.0n B',[int(mem)]);
end;

procedure ParallelProcessTree(const task: IOmniTask);

  procedure Process(node: PNode; startLevel, level: integer);
  var
    iChild  : integer;
    intf    : IInterface;
    monitor : IOmniTaskControlMonitor;
    monVal  : TOmniValue;
    subTasks: TInterfaceList;
  begin
    if node.IsLeaf then
      //leaf node, .Value already contains correct value
      Exit;
    subTasks := nil;
    try
      for iChild := 1 to GNumChildren do begin
        if {optimization}(level > CPruneLevel) and
           {available tasks}(task.Counter.Decrement > 0) then
        begin //schedule new task
          if not assigned(subTasks) then
            subTasks := TInterfaceList.Create;
          monVal := task.Param['Monitor']; //work around internal error T2575
          monitor := monVal.AsInterface as IOmniTaskControlMonitor;
          subTasks.Add(
            CreateTask(ParallelProcessTree)
              .SetParameter('Node',    TObject(node.Child[iChild]))
              .SetParameter('Monitor', monitor)
              .SetParameter('Level',   level-1)
              .SetParameter('Child',   iChild)
              .WithCounter(task.Counter)
              .MonitorWith(monitor)
              .Run
            );
        end
        else begin
          if level > CPruneLevel then //task.Counter.Decrement was executed, counteract it
            task.Counter.Increment;
          Process(node.Child[iChild], startLevel, level-1);
        end;
      end;
      //wait for all subtasks to complete
      if assigned(subtasks) then begin
        if level = startLevel then // all work done
          task.Counter.Increment;
        for intf in subTasks do
          (intf as IOmniTaskControl).WaitFor(INFINITE);
      end;
    finally
      subTasks.Free;
    end;
    node.Value := 0;
    for iChild := 1 to GNumChildren do
      node.Value := node.Value + Round(Exp(Ln(node.Child[iChild].Value)));
  end; { Process }

var
  level  : integer;
  nodeVal: TOmniValue;
begin
  level := task.Param['Level'];
  task.Comm.Send(0, Format('Task %d started: Level %d / Child %d',
    [task.UniqueID, level, integer(task.Param['Child'])]));
  nodeVal := task.Param['Node']; //work around internal error T2575
  Process(PNode(nodeVal.AsObject), level, level);
  task.Comm.Send(0, Format('Task %d completed', [task.UniqueID]));
end; { ParallelProcessTree }

{ TNode }

function TNode.GetChild(idxChild: integer): PNode;
begin
  {$R-}
  Result := SubNodes[idxChild];
  {$R+}
end;

function TNode.IsLeaf: boolean;
begin
  Result := not assigned(SubNodes[1]);
end;

procedure TNode.SetChild(idxChild: integer; value: PNode);
begin
  {$R-}
  SubNodes[idxChild] := value;
  {$R+}
end;

{ TfrmRecursiveTreeDemo }

function TfrmRecursiveTreeDemo.AllocateNode: PNode;
begin
  GetMem(Result, SizeOf(TNode) + SizeOf(PNode) * (GNumChildren - 1));
end;

procedure TfrmRecursiveTreeDemo.btnBuildTreeClick(Sender: TObject);
var
  timeStart: int64;
begin
  DestroyTree;
  GNumChildren := inpNumChildren.Value;
  FValue := 1;
  Log('Building tree');
  timeStart := GetTickCount;
  FRoot := AllocateNode;
  CreateChildren(FRoot, inpTreeDepth.Value - 1);
  Log(Format('Tree built in %d ms', [DSiElapsedTime(timeStart)]));
  FRebuildTree := false;
end;

procedure TfrmRecursiveTreeDemo.btnMultiCoreTestClick(Sender: TObject);
begin
  if (not assigned(FRoot)) or FRebuildTree then
    btnBuildTree.Click
  else
    ClearTree;
  Log('Scanning tree');
  FTaskMessages := TStringList.Create;
  FTimeStart := GetTickCount;
  FRootTask := CreateTask(ParallelProcessTree)
    .SetParameter('Node', TObject(FRoot))
    .SetParameter('Monitor', OtlMonitor as IOmniTaskControlMonitor)
    .SetParameter('Level', inpTreeDepth.Value) //for logging and pruning
    .SetParameter('Child', 1) //for logging only
    .WithCounter(CreateCounter(inpNumTasks.Value))
    .MonitorWith(OtlMonitor)
    .Run;
end;

procedure TfrmRecursiveTreeDemo.btnSingleCoreTestClick(Sender: TObject);
var
  timeStart: int64;
begin
  if (not assigned(FRoot)) or FRebuildTree then
    btnBuildTree.Click
  else
    ClearTree;
  Log('Scanning tree');
  timeStart := GetTickCount;
  ProcessTree(FRoot);
  Log(Format('Completed in %d ms, answer = %d', [DSiElapsedTime(timeStart), FRoot.Value]));
end;

procedure TfrmRecursiveTreeDemo.CalcNumNodes;
var
  iLevel  : integer;
  nodeSize: integer;
  numLevel: integer;
  numNodes: integer;
  treeSize: int64;
begin
  nodeSize := SizeOf(TNode) + (inpNumChildren.Value-1) * SizeOf(PNode);
  numNodes := 1; // root
  numLevel := 1;
  treeSize := nodeSize;
  for iLevel := 2 to inpTreeDepth.Value do begin
    numLevel := numLevel * inpNumChildren.Value;
    Inc(numNodes, numLevel);
    Inc(treeSize, numLevel * nodeSize);
  end;
  outNumNodes.Text := IntToStr(numNodes);
  outTreeSize.Text := FmtMem(treeSize);
end; 

procedure TfrmRecursiveTreeDemo.ClearNode(node: PNode);
var
  iChild: integer;
begin
  if not node.IsLeaf then begin
    node.Value := 0;
    for iChild := 1 to GNumChildren do
      ClearNode(node.Child[iChild]);
  end;
end;

procedure TfrmRecursiveTreeDemo.ClearTree;
var
  timeStart: int64;
begin
  if assigned(FRoot) then begin
    Log('Clearing tree');
    timeStart := GetTickCount;
    ClearNode(FRoot);
    Log(Format('Tree cleared in %d ms', [DSiElapsedTime(timeStart)]));
  end;
end;

procedure TfrmRecursiveTreeDemo.CreateChildren(node: PNode; subLevels: integer);
var
  iChild: integer;                 
begin
  if subLevels = 0 then begin
    node.Value := FValue;
    Inc(FValue);
    for iChild := 1 to GNumChildren do
      node.Child[iChild] := nil;
  end
  else begin
    for iChild := 1 to GNumChildren do begin
      node.Child[iChild] := AllocateNode;
      CreateChildren(node.Child[iChild], subLevels - 1);
    end;
  end;
end;

procedure TfrmRecursiveTreeDemo.DestroyNode(node: PNode);
var
  iChild: integer;
begin
  if not node.IsLeaf then
    for iChild := 1 to GNumChildren do
      DestroyNode(node.Child[iChild]);
  FreeMem(node);
end;

procedure TfrmRecursiveTreeDemo.DestroyTree;
var
  timeStart: int64;
begin
  if assigned(FRoot) then begin
    Log('Destroying tree');
    timeStart := GetTickCount;
    DestroyNode(FRoot);
    Log(Format('Tree destroyed in %d ms', [DSiElapsedTime(timeStart)]));
  end;
  FRoot := nil;
end;

procedure TfrmRecursiveTreeDemo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DestroyTree;
end;

procedure TfrmRecursiveTreeDemo.FormCreate(Sender: TObject);
begin
  inpNumTasks.Value := Length(DSiGetProcessAffinity);
  CalcNumNodes;
end;

procedure TfrmRecursiveTreeDemo.inpNumChildrenChange(Sender: TObject);
begin
  CalcNumNodes;
  FRebuildTree := true;
end;

procedure TfrmRecursiveTreeDemo.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmRecursiveTreeDemo.OtlMonitorTaskMessage(
  const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  FTaskMessages.Add(msg.MsgData);
end;

procedure TfrmRecursiveTreeDemo.OtlMonitorTaskTerminated(const task: IOmniTaskControl);
begin
  if task = FRootTask then begin
    FRootTask := nil;
    lbLog.Items.AddStrings(FTaskMessages);
    FreeAndNil(FTaskMessages);
    Log(Format('Completed in %d ms, answer = %d', [DSiElapsedTime(FTimeStart), FRoot.Value]));
  end;
end;

procedure TfrmRecursiveTreeDemo.ProcessTree(node: PNode);
var
  iChild: integer;
begin
  if node.IsLeaf then
    //leaf node, .Value already contains correct value
    Exit;
  for iChild := 1 to GNumChildren do
    ProcessTree(node.Child[iChild]);
  node.Value := 0;
  for iChild := 1 to GNumChildren do
    node.Value := node.Value + Round(Exp(Ln(node.Child[iChild].Value))); 
end;

end.
