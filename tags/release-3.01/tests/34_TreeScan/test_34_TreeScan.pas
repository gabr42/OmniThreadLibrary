unit test_34_TreeScan;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask;

type
  TNode = class
    Value: integer;
    Child: array of TNode;
    function NumChild: integer;
    function ToString: string; {$IF CompilerVersion >= 20}reintroduce;{$IFEND}
  end;

  TfrmTreeScanDemo = class(TForm)
    btnBuildTree: TButton;
    btnParaScan : TButton;
    btnSeqScan  : TButton;
    lbLog       : TListBox;
    btnBuildLarge: TButton;
    procedure btnBuildLargeClick(Sender: TObject);
    procedure btnBuildTreeClick(Sender: TObject);
    procedure btnParaScanClick(Sender: TObject);
    procedure btnSeqScanClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    FNumNodes: integer;
    FRootNode: TNode;
    function  CreateNode(availNodes: integer; nodeValue: integer): TNode;
    procedure CreateTree(maxNodes: integer);
    procedure DestroyNode(var node: TNode);
    procedure DestroyTree;
    procedure Log(const msg: string; const params: array of const);
    procedure ParaFind(value: integer);
    function  ParaScan(rootNode: TNode; value: integer): TNode;
    procedure ParaScanWorker(const task: IOmniTask);
    procedure RemoveEmptyLeaves(node: TNode);
    procedure SeqFind(value: integer);
    function  SeqScan(node: TNode; value: integer): TNode;
  public
  end; { TfrmTreeScanDemo }

var
  frmTreeScanDemo: TfrmTreeScanDemo;

implementation

uses
  DSiWin32,
  GpStuff,
  OtlCommon,
  OtlContainers,
  OtlCollections,
  OtlTaskControl;

{$R *.dfm}

const
  CNumNodes      = 100000;
  CNumNodesLarge = 1000000;
  CMaxChild      = 5;

type
  PNode = ^TNode;
  
{ TNode }

function TNode.NumChild: integer;
begin
  Result := Length(Child);
end; { TNode.NumChild }

function TNode.ToString: string;
var
  iNode: integer;
begin
  Result := IntToStr(Value) + ' [';
  for iNode := 0 to NumChild - 1 do begin
    if iNode > 0 then
      Result := Result + ',';
    Result := Result + IntToStr(Child[iNode].Value);
  end;
  Result := Result + ']';
end; { TNode.ToString }

{ TfrmTreeScanDemo }

procedure TfrmTreeScanDemo.btnBuildLargeClick(Sender: TObject);
begin
  DestroyTree;
  CreateTree(CNumNodesLarge);
end; { TfrmTreeScanDemo.btnBuildLargeClick }

procedure TfrmTreeScanDemo.btnBuildTreeClick(Sender: TObject);
begin
  DestroyTree;
  CreateTree(CNumNodes);
end; { TfrmTreeScanDemo.btnBuildTreeClick }

procedure TfrmTreeScanDemo.btnParaScanClick(Sender: TObject);
begin
  ParaFind(2);
  ParaFind(FNumNodes div 4 * 2);
  ParaFind(FNumNodes div 2 * 2);
  ParaFind(FNumNodes div 2 * 2 - 1);
end; { TfrmTreeScanDemo.btnParaScanClick }

procedure TfrmTreeScanDemo.btnSeqScanClick(Sender: TObject);
begin
  SeqFind(2);
  SeqFind(FNumNodes div 4 * 2);
  SeqFind(FNumNodes div 2 * 2);
  SeqFind(FNumNodes div 2 * 2 - 1);
end; { TfrmTreeScanDemo.btnSeqScanClick }

function TfrmTreeScanDemo.CreateNode(availNodes: integer; nodeValue: integer): TNode;
var
  numChild: integer;
begin
  Result := TNode.Create;
  Result.Value := nodeValue;
  numChild := Random(CMaxChild-1) + 1;
  if numChild > availNodes then
    numChild := availNodes;
  if numChild > 0 then
    SetLength(Result.Child, numChild);
end; { TfrmTreeScanDemo.CreateNode }

procedure TfrmTreeScanDemo.CreateTree(maxNodes: integer);
var
  iNode      : integer;
  node       : PNode;
  nodeValue  : integer;
  numNodes   : integer;
  unallocated: TOmniQueue;
  value      : TOmniValue;
begin
  unallocated := TOmniQueue.Create;
  try
    numNodes := maxNodes;
    nodeValue := 0;
    unallocated.Enqueue(@FRootNode);
    while unallocated.TryDequeue(value) do begin
      node := value.AsPointer;
      if numNodes <= 0 then
        node^ := nil
      else begin
        Inc(nodeValue, 2);
        node^ := CreateNode(numNodes, nodeValue);
        for iNode := 0 to node^.NumChild - 1 do
          unallocated.Enqueue(@(node^.Child[iNode]));
        Dec(numNodes);
      end;
    end;
  finally FreeAndNil(unallocated); end;
  RemoveEmptyLeaves(FRootNode);
  FNumNodes := nodeValue div 2;
  Log('Created tree with %d nodes', [FNumNodes]);
  btnParaScan.Enabled := true;
  btnSeqScan.Enabled := true;
end; { TfrmTreeScanDemo.CreateTree }

procedure TfrmTreeScanDemo.DestroyNode(var node: TNode);
var
  iChild: integer;
begin
  for iChild := 0 to node.NumChild - 1 do
    DestroyNode(node.Child[iChild]);
  FreeAndNil(node);
end; { TfrmTreeScanDemo.DestroyNode }

procedure TfrmTreeScanDemo.DestroyTree;
begin
  if assigned(FRootNode) then
    DestroyNode(FRootNode);
end; { TfrmTreeScanDemo.DestroyTree }

procedure TfrmTreeScanDemo.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  DestroyTree;
  CanClose := true;
end; { TfrmTreeScanDemo.FormCloseQuery }

procedure TfrmTreeScanDemo.Log(const msg: string; const params: array of const);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format(msg, params));
end; { TfrmTreeScanDemo.Log }

procedure TfrmTreeScanDemo.ParaFind(value: integer);
var
  node     : TNode;
  startTime: integer;
begin
  Log('Searching for: %d', [value]);
  startTime := DSiTimeGetTime64;
  node := ParaScan(FRootNode, value);
  if assigned(node) then
    Log('Found in %d ms; node = %s', [DSiTimeGetTime64 - startTime, node.ToString])
  else
    Log('Not found in %d ms', [DSiTimeGetTime64 - startTime]);
end; { TfrmTreeScanDemo.ParaFind }

function TfrmTreeScanDemo.ParaScan(rootNode: TNode; value: integer): TNode;
var
  countWorkers: IOmniCounter;
  iTask       : integer;
  nodeQueue   : TOmniBlockingCollection;
  numTasks    : integer;
  scanResult  : TOmniWaitableValue;
begin
  Result := nil;
  numTasks := Environment.Process.Affinity.Count;
  nodeQueue := TOmniBlockingCollection.Create(numTasks);
  try
    nodeQueue.Add(rootNode);
    countWorkers := CreateCounter(numTasks);
    scanResult := TOmniWaitableValue.Create;
    try
      scanResult.Value.AsPointer := nil;
      for iTask := 1 to numTasks do begin
        CreateTask(ParaScanWorker, 'Parallel scan worker #' + IntToStr(iTask))
          .SetParameters([nodeQueue, scanResult, value])
          .WithCounter(countWorkers)
          .Unobserved
          .Run;
      end;
      if WaitForSingleObject(nodeQueue.CompletedSignal, 10*1000) <> WAIT_OBJECT_0 then begin
        Log('Catastrophic failure, parallel scan did not complete in 10 seconds', []);
        nodeQueue.CompleteAdding;
        Result := nil;
      end
      else begin
        // wait for all workers to complete and release references to nodeQueue
        while countWorkers.Value > 0 do
          Sleep(1);
        Result := scanResult.Value.AsPointer;
      end;
    finally FreeAndNil(scanResult); end;
  finally FreeAndNil(nodeQueue); end;
end; { TfrmTreeScanDemo.ParaScan }

procedure TfrmTreeScanDemo.ParaScanWorker(const task: IOmniTask);
var
  iNode     : integer;
  node      : TNode;
  nodeQueue : TOmniBlockingCollection;
  param0    : TOmniValue;
  param1    : TOmniValue;
  scanResult: TOmniWaitableValue;
  scanValue : integer;
  value     : TOmniValue;

  procedure Add(queue: TOmniBlockingCollection; value: TOmniValue);
  begin
    // triggers internal compiler error in D2007 if inlined
    queue.Add(value);
  end;

begin
  try
    param0 := task.Param[0];
    nodeQueue := TOmniBlockingCollection(param0.AsPointer);
    param1 := task.Param[1];
    scanResult := TOmniWaitableValue(param1.AsPointer);
    scanValue := task.Param[2].AsInteger;
    for value in nodeQueue do begin
      node := TNode(value.AsPointer);
      if node.Value = scanValue then begin
        scanResult.Signal(node);
        nodeQueue.CompleteAdding;
      end
      else for iNode := 0 to node.NumChild - 1 do 
        Add(nodeQueue, node.Child[iNode]);
    end;
  finally task.Counter.Decrement; end;
end; { TfrmTreeScanDemo.ParaScanWorker }

procedure TfrmTreeScanDemo.RemoveEmptyLeaves(node: TNode);
var
  highestChild: integer;
  iNode       : integer;
begin
  highestChild := -1;
  for iNode := 0 to node.NumChild - 1 do begin
    if assigned(node.Child[iNode]) then begin
      highestChild := iNode;
      RemoveEmptyLeaves(node.Child[iNode]);
    end;
  end;
  if (highestChild + 1) <> node.NumChild then
    SetLength(node.Child, highestChild + 1);
end; { TfrmTreeScanDemo.RemoveEmptyLeaves }

procedure TfrmTreeScanDemo.SeqFind(value: integer);
var
  node     : TNode;
  startTime: integer;
begin
  Log('Searching for: %d', [value]);
  startTime := DSiTimeGetTime64;
  node := SeqScan(FRootNode, value);
  if assigned(node) then
    Log('Found in %d ms; node = %s', [DSiTimeGetTime64 - startTime, node.ToString])
  else
    Log('Not found in %d ms', [DSiTimeGetTime64 - startTime]);
end; { TfrmTreeScanDemo.SeqFind }

function TfrmTreeScanDemo.SeqScan(node: TNode; value: integer): TNode;
var
  iNode: integer;
begin
  if node.Value = value then
    Result := node
  else begin
    Result := nil;
    for iNode := 0 to node.NumChild - 1 do begin
      Result := SeqScan(node.Child[iNode], value);
      if assigned(Result) then
        break; //for iNode
    end;
  end;
end; { TfrmTreeScanDemo.SeqScan }

end.

