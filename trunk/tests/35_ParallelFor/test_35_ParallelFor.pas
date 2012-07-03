unit test_35_ParallelFor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OtlTask,
  OtlCollections;

type
  TNode = class;

  TNodeChildEnumerator = record
  strict private
    FNode   : TNode;
    FNodeIdx: integer;
  public
    constructor Create(node: TNode);
    function  GetCurrent: TNode;
    function  MoveNext: boolean;
    property Current: TNode read GetCurrent;
  end; { TNodeChildEnumerator }

  TNodeChildEnumeratorFactory = record
  strict private
    FNode: TNode;
  public
    constructor Create(node: TNode);
    function GetEnumerator: TNodeChildEnumerator;
  end; { TNodeChildEnumeratorFactory }

  TNode = class
    Value: integer;
    Child: array of TNode;
    function  NumChild: integer;
    function  ToString: string; {$IF CompilerVersion >= 20}reintroduce;{$IFEND}
    function  Children: TNodeChildEnumeratorFactory;
  end; { TNode }

  TfrmParallelForDemo = class(TForm)
    btnBuildLarge   : TButton;
    btnBuildTree    : TButton;
    btnIntegerEnum  : TButton;
    btnOmniValueEnum: TButton;
    btnParaScan     : TButton;
    btnSeqScan      : TButton;
    cbRepeat        : TCheckBox;
    lbLog           : TListBox;
    btnEnumeratorEnum: TButton;
    btnGetEnumeratorEnum: TButton;
    btnDelegateEnum: TButton;
    btnTEnumeratorTEnum: TButton;
    procedure btnBuildLargeClick(Sender: TObject);
    procedure btnBuildTreeClick(Sender: TObject);
    procedure btnDelegateEnumClick(Sender: TObject);
    procedure btnEnumeratorEnumClick(Sender: TObject);
    procedure btnGetEnumeratorEnumClick(Sender: TObject);
    procedure btnIntegerEnumClick(Sender: TObject);
    procedure btnOmniValueEnumClick(Sender: TObject);
    procedure btnParaScanClick(Sender: TObject);
    procedure btnSeqScanClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure btnTEnumeratorTEnumClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    procedure RemoveEmptyLeaves(node: TNode);
    procedure SeqFind(value: integer);
    function  SeqScan(node: TNode; value: integer): TNode;
    function  VerifyResult(outQueue: IOmniBlockingCollection; testSize, numCores: integer;
      time: int64): boolean;
  public
    procedure WMUser(var msg: TMessage); message WM_USER;
  end; { TfrmParallelForDemo }

var
  frmParallelForDemo: TfrmParallelForDemo;

implementation

uses
  DSiWin32,
  GpLists,
  Generics.Collections,
  OtlCommon,
  OtlSync,
  OtlContainers,
  OtlTaskControl,
  OtlDataManager,
  OtlParallel;

{$R *.dfm}

const
  CNumNodes      = 100000;
  CNumNodesLarge = 1000000;
  CMaxChild      = 5;

type
  PNode = ^TNode;

  TListEnum = class(TInterfacedObject, IEnumerator)
  strict private
    FList_ref: TList;
    FListEnum: TListEnumerator;
  public
    constructor Create(list: TList);
    destructor  Destroy; override;
    function  GetCurrent: TObject;
    function  MoveNext: Boolean;
    procedure Reset;
    property Current: TObject read GetCurrent;
  end;

  TListEnumHelp = class
  strict private
    lehList: TList;
  public
    constructor Create(list: TList);
    function  GetEnumerator: IEnumerator;
  end;

function CreateIEnumerator(li: TList): IEnumerator;
begin
  Result := TListEnum.Create(li);
end; { CreateIEnumerator }

{ TListEnumerator }

constructor TListEnum.Create(list: TList);
begin
  inherited Create;
  FList_ref := list;
  Reset;
end; { TListEnumerator.Create }

destructor TListEnum.Destroy;
begin
  FreeAndNil(FListEnum);
  inherited;
end; { TListEnumerator.Destroy }

function TListEnum.GetCurrent: TObject;
begin
  Result := TObject(FListEnum.Current);
end; { TListEnumerator.GetCurrent }

function TListEnum.MoveNext: Boolean;
begin
  Result := FListEnum.MoveNext;
end; { TListEnumerator.MoveNext }

procedure TListEnum.Reset;
begin
  FreeAndNil(FListEnum);
  FListEnum := FList_ref.GetEnumerator;
end; { TListEnumerator.Reset }

{ TNode }

function TNode.Children: TNodeChildEnumeratorFactory;
begin
  Result := TNodeChildEnumeratorFactory.Create(Self);
end;

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

{ TfrmParallelForDemo }

procedure TfrmParallelForDemo.btnBuildLargeClick(Sender: TObject);
begin
  DestroyTree;
  CreateTree(CNumNodesLarge);
end; { TfrmParallelForDemo.btnBuildLargeClick }

procedure TfrmParallelForDemo.btnBuildTreeClick(Sender: TObject);
begin
  DestroyTree;
  CreateTree(CNumNodes);
end; { TfrmParallelForDemo.btnBuildTreeClick }

procedure TfrmParallelForDemo.btnDelegateEnumClick(Sender: TObject);
var
  i       : integer;
  nodeList: TList;
  numCores: integer;
  outQueue: IOmniBlockingCollection;
  testSize: integer;
  time    : int64;
begin
  nodeList := TList.Create;
  try
    testSize := Random(200000)+1;
    numCores := Random(Environment.Process.Affinity.Count*2)+1;
    i := 1;
    outQueue := TOmniBlockingCollection.Create;
    time := DSiTimeGetTime64;
    Parallel.ForEach<integer>(
      function (var value: integer): boolean
      begin
        value := i;
        Result := (i <= testSize);
        Inc(i);
      end)
      .NumTasks(numCores)
      .Execute(
        procedure (const elem: integer)
        begin
          outQueue.Add(elem);
        end);
    VerifyResult(outQueue, testSize, numCores, DSiTimeGetTime64 - time);
  finally FreeAndNil(nodeList); end;
  if cbRepeat.Checked then
    PostMessage(Handle, WM_USER, 4, 0);
end; { TfrmParallelForDemo.btnDelegateEnumClick }

procedure TfrmParallelForDemo.btnEnumeratorEnumClick(Sender: TObject);
var
  i       : integer;
  nodeList: TList;
  numCores: integer;
  outQueue: IOmniBlockingCollection;
  testSize: integer;
  time    : int64;
begin
  nodeList := TList.Create;
  try
    testSize := Random(200000)+1;
    numCores := Random(Environment.Process.Affinity.Count*2)+1;
    for i := 1 to testSize do
      nodeList.Add(pointer(i));
    outQueue := TOmniBlockingCollection.Create;
    time := DSiTimeGetTime64;
    {$IF CompilerVersion >= 21}
    Parallel.ForEach<pointer>(CreateIEnumerator(nodeList))
      .NumTasks(numCores)
      .Execute(
        procedure (const elem: pointer)
        begin
          outQueue.Add(elem);
        end);
    {$ELSE} // there's no typeinfo for 'pointer' in D2009 which causes CastAs to break
    Parallel.ForEach<TObject>(CreateIEnumerator(nodeList))
      .NumTasks(numCores)
      .Execute(
        procedure (const elem: TObject)
        begin
          outQueue.Add(pointer(elem));
        end);
    {$IFEND}
    VerifyResult(outQueue, testSize, numCores, DSiTimeGetTime64 - time);
  finally FreeAndNil(nodeList); end;
  if cbRepeat.Checked then
    PostMessage(Handle, WM_USER, 2, 0);
end; { TfrmParallelForDemo.btnEnumeratorEnumClick }

procedure TfrmParallelForDemo.btnGetEnumeratorEnumClick(Sender: TObject);
{$IF CompilerVersion >= 21}
var
  i       : integer;
  nodeList: TList;
  numCores: integer;
  outQueue: IOmniBlockingCollection;
  testSize: integer;
  time    : int64;
{$IFEND}
begin
  {$IF CompilerVersion >= 21}
  nodeList := TList.Create;
  try
    testSize := Random(200000)+1;
    numCores := Random(Environment.Process.Affinity.Count*2)+1;
    for i := 1 to testSize do
      nodeList.Add(pointer(i));
    outQueue := TOmniBlockingCollection.Create;
    time := DSiTimeGetTime64;
    Parallel.ForEach<integer>(nodeList)
      .NumTasks(numCores)
      .Execute(
        procedure (const elem: integer)
        begin
          outQueue.Add(elem);
        end);
    VerifyResult(outQueue, testSize, numCores, DSiTimeGetTime64 - time);
  finally FreeAndNil(nodeList); end;
  if cbRepeat.Checked then
    PostMessage(Handle, WM_USER, 3, 0);
  {$IFEND}
end; { TfrmParallelForDemo.btnGetEnumeratorEnumClick }

procedure TfrmParallelForDemo.btnIntegerEnumClick(Sender: TObject);
var
  numCores: integer;
  outQueue: IOmniBlockingCollection;
  testSize: integer;
  time    : int64;
begin
  testSize := Random(200000)+1;
  numCores := Random(Environment.Process.Affinity.Count*2)+1;
  outQueue := TOmniBlockingCollection.Create;
  time := DSiTimeGetTime64;
  Parallel.ForEach(1, testSize)
    .NumTasks(1{numCores})
    .Execute(
      procedure (const elem: integer)
      begin
        outQueue.Add(elem);
      end);
  VerifyResult(outQueue, testSize, numCores, DSiTimeGetTime64 - time);
  if cbRepeat.Checked then
    PostMessage(Handle, WM_USER, 0, 0);
end; { TfrmParallelForDemo.btnIntegerRenumClick }

procedure TfrmParallelForDemo.btnOmniValueEnumClick(Sender: TObject);
var
  i        : integer;
  nodeQueue: IOmniBlockingCollection;
  numCores : integer;
  outQueue : IOmniBlockingCollection;
  testSize : integer;
  time     : int64;
begin
  nodeQueue := TOmniBlockingCollection.Create;
  testSize := Random(200000)+1;
  numCores := Random(Environment.Process.Affinity.Count*2)+1;
  for i := 1 to testSize do
    nodeQueue.Add(i);
  nodeQueue.CompleteAdding;
  outQueue := TOmniBlockingCollection.Create;
  time := DSiTimeGetTime64;
  Parallel.ForEach<int64>(nodeQueue as IOmniValueEnumerable)
    .NumTasks(numCores)
    .Execute(
      procedure (const elem: int64)
      begin
        outQueue.Add(elem);
      end);
  VerifyResult(outQueue, testSize, numCores, DSiTimeGetTime64 - time);
  if cbRepeat.Checked then
    PostMessage(Handle, WM_USER, 1, 0);
end; { TfrmParallelForDemo.btnOmniValueEnumClick }

procedure TfrmParallelForDemo.btnParaScanClick(Sender: TObject);
begin
  ParaFind(2);
  ParaFind(FNumNodes div 4 * 2);
  { TODO 5 -oPrimoz Gabrijelcic : Why big time difference between the next two? }
  ParaFind(FNumNodes div 2 * 2);
  ParaFind(FNumNodes div 2 * 2 - 1);
end; { TfrmParallelForDemo.btnParaScanClick }

procedure TfrmParallelForDemo.btnSeqScanClick(Sender: TObject);
begin
  SeqFind(2);
  SeqFind(FNumNodes div 4 * 2);
  SeqFind(FNumNodes div 2 * 2);
  SeqFind(FNumNodes div 2 * 2 - 1);
end; { TfrmParallelForDemo.btnSeqScanClick }

procedure TfrmParallelForDemo.btnTEnumeratorTEnumClick(Sender: TObject);
var
  i       : integer;
  nodeList: TList<integer>;
  numCores: integer;
  outQueue: IOmniBlockingCollection;
  testSize: integer;
  time    : int64;
begin
  nodeList := TList<integer>.Create;
  try
    testSize := Random(200000)+1;
    numCores := Random(Environment.Process.Affinity.Count*2)+1;
    for i := 1 to testSize do
      nodeList.Add(i);
    outQueue := TOmniBlockingCollection.Create;
    time := DSiTimeGetTime64;
    Parallel.ForEach<integer>(nodeList.GetEnumerator())
      .NumTasks(numCores)
      .Execute(
        procedure (const elem: integer)
        begin
          outQueue.Add(elem);
        end);
    VerifyResult(outQueue, testSize, numCores, DSiTimeGetTime64 - time);
  finally FreeAndNil(nodeList); end;
  if cbRepeat.Checked then
    PostMessage(Handle, WM_USER, 5, 0);
end; { TfrmParallelForDemo.btnTEnumeratorTEnumClick }

function TfrmParallelForDemo.CreateNode(availNodes: integer; nodeValue: integer): TNode;
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
end; { TfrmParallelForDemo.CreateNode }

procedure TfrmParallelForDemo.CreateTree(maxNodes: integer);
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
end; { TfrmParallelForDemo.CreateTree }

procedure TfrmParallelForDemo.DestroyNode(var node: TNode);
var
  iChild: integer;
begin
  for iChild := 0 to node.NumChild - 1 do
    DestroyNode(node.Child[iChild]);
  FreeAndNil(node);
end; { TfrmParallelForDemo.DestroyNode }

procedure TfrmParallelForDemo.DestroyTree;
begin
  if assigned(FRootNode) then
    DestroyNode(FRootNode);
end; { TfrmParallelForDemo.DestroyTree }

procedure TfrmParallelForDemo.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  DestroyTree;
  CanClose := true;
end; { TfrmParallelForDemo.FormCloseQuery }

procedure TfrmParallelForDemo.FormCreate(Sender: TObject);
begin
  {$IF CompilerVersion < 21}
  btnGetEnumeratorEnum.Enabled := false;
  {$IFEND}
end; { TfrmParallelForDemo.FormCreate }

procedure TfrmParallelForDemo.Log(const msg: string; const params: array of const);
begin
  lbLog.ItemIndex := lbLog.Items.Add(Format(msg, params));
end; { TfrmParallelForDemo.Log }

procedure TfrmParallelForDemo.ParaFind(value: integer);
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
end; { TfrmParallelForDemo.ParaFind }

function TfrmParallelForDemo.ParaScan(rootNode: TNode; value: integer): TNode;
var
  cancelToken: IOmniCancellationToken;
  nodeQueue  : IOmniBlockingCollection;
  nodeResult : TNode;
  numTasks   : integer;
begin
  nodeResult := nil;
  cancelToken := CreateOmniCancellationToken;
  numTasks := Environment.Process.Affinity.Count;
  nodeQueue := TOmniBlockingCollection.Create(numTasks);
  nodeQueue.Add(rootNode);
  Parallel.ForEach<TNode>(nodeQueue)
    .NumTasks(numTasks) // must be same number of task as in nodeQueue to ensure stopping
    .CancelWith(cancelToken)
    .Execute(
      procedure (const node: TNode)
      var
        childNode: TNode;
      begin
        if node.Value = value then begin
          nodeResult := node;
          nodeQueue.CompleteAdding; // this will unblock any task waiting for a node
          cancelToken.Signal; // this will tell all tasks to stop even if there is data in the nodeQueue
        end
        else for childNode in node.Children do
          nodeQueue.TryAdd(childNode);
      end);
  Result := nodeResult;
end; { TfrmParallelForDemo.ParaScan }

procedure TfrmParallelForDemo.RemoveEmptyLeaves(node: TNode);
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
end; { TfrmParallelForDemo.RemoveEmptyLeaves }

procedure TfrmParallelForDemo.SeqFind(value: integer);
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
end; { TfrmParallelForDemo.SeqFind }

function TfrmParallelForDemo.SeqScan(node: TNode; value: integer): TNode;
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
end; { TfrmParallelForDemo.SeqScan }

function TfrmParallelForDemo.VerifyResult(outQueue: IOmniBlockingCollection; testSize,
  numCores: integer; time: int64): boolean;
var
  i      : integer;
  outList: TGpInt64List;
  value  : TOmniValue;
begin
  Result := false;
  outQueue.CompleteAdding;
  try
    outList := TGpInt64List.Create;
    try
      while outQueue.Take(value) do
        outList.Add(value.RawData^);
      outList.Sort;
      outList.Sorted := false;
      outList.Insert(0, 0);
      for i := 1 to testSize do
      if outList[i] <> i then
         Assert(outList[i] = i, Format('[%d] = %d; [%d] = %d; [%d] = %d',
          [i-1, outList[i-1], i, outList[i], i+1, outList[i+1]]));
    finally FreeAndNil(outList); end;
    Log('1..%d /%d: OK %d ms', [testSize, numCores, time]);
    Result := true;
  except
    on E: Exception do
      Log('1..%d /%d: %s', [testSize, numCores, E.Message]);
  end;
end; { TfrmParallelForDemo.VerifyResult }

procedure TfrmParallelForDemo.WMUser(var msg: TMessage);
begin
  case msg.WParam of
    0: btnIntegerEnum.Click;
    1: btnOmniValueEnum.Click;
    2: btnEnumeratorEnum.Click;
    3: btnGetEnumeratorEnum.Click;
    4: btnDelegateEnum.Click;
    5: btnTEnumeratorTEnum.Click;
  end;
end; { TfrmParallelForDemo.WMUser }

{ TNodeChildEnumerator }

constructor TNodeChildEnumerator.Create(node: TNode);
begin
  FNode := node;
  FNodeIdx := -1;
end; { TNodeChildEnumerator.Create }

function TNodeChildEnumerator.GetCurrent: TNode;
begin
  Result := FNode.Child[FNodeIdx];
end; { TNodeChildEnumerator.GetCurrent }

function TNodeChildEnumerator.MoveNext: boolean;
begin
  Inc(FNodeIdx);
  Result := (FNodeIdx <= High(FNode.Child));
end; { TNodeChildEnumerator.MoveNext }

{ TNodeChildEnumeratorFactory }

constructor TNodeChildEnumeratorFactory.Create(node: TNode);
begin
  FNode := node;
end; { TNodeChildEnumeratorFactory.Create }

function TNodeChildEnumeratorFactory.GetEnumerator: TNodeChildEnumerator;
begin
  Result := TNodeChildEnumerator.Create(FNode);
end; { TNodeChildEnumeratorFactory.GetEnumerator }

{ TListEnumHelp }

constructor TListEnumHelp.Create(list: TList);
begin
  inherited Create;
  lehList := list;
end; { TListEnumHelp.Create }

function TListEnumHelp.GetEnumerator: IEnumerator;
begin
  Result := TListEnum.Create(lehList);
end; { TListEnumHelp.GetEnumerator }

end.

