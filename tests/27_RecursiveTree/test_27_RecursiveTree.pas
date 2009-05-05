unit test_27_RecursiveTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  PNode = ^TNode;
  TNodeArray = array [1..1] of PNode;
  TNode = packed record
    Value: int64;
    Child: TNodeArray;
  end;

  TfrmRecursiveTreeDemo = class(TForm)
    btnBuildTree  : TButton;
    inpNumChildren: TSpinEdit;
    inpNumTasks   : TSpinEdit;
    inpTreeDepth  : TSpinEdit;
    lblNumChildren: TLabel;
    lblNumNodes   : TLabel;
    lblNumTasks   : TLabel;
    lblTreeDepth  : TLabel;
    lblTreeSize   : TLabel;
    outNumNodes   : TEdit;
    outTreeSize   : TEdit;
    lbLog: TListBox;
    btnSingleCoreTest: TButton;
    procedure btnBuildTreeClick(Sender: TObject);
    procedure btnSingleCoreTestClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure inpNumChildrenChange(Sender: TObject);
  private
    FNumChildren: integer;
    FRoot       : PNode;
    FValue      : integer;
    function  AllocateNode: PNode;
    procedure CalcNumNodes;
    procedure CreateChildren(node: PNode; subLevels: integer);
    procedure DestroyNode(node: PNode);
    procedure DestroyTree;
    procedure Log(const msg: string);
    procedure ProcessTree(node: PNode);
  public
  end; { TfrmRecursiveTreeDemo }

var
  frmRecursiveTreeDemo: TfrmRecursiveTreeDemo;

implementation

uses
  DSiWin32;

{$R *.dfm}

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
end; { FmtMem }

function TfrmRecursiveTreeDemo.AllocateNode: PNode;
begin
  GetMem(Result, SizeOf(TNode) + SizeOf(PNode) * (FNumChildren - 1));
end;

procedure TfrmRecursiveTreeDemo.btnBuildTreeClick(Sender: TObject);
var
  timeStart: int64;
begin
  DestroyTree;
  FNumChildren := inpNumChildren.Value;
  FValue := 1;
  Log('Building tree');
  timeStart := GetTickCount;
  FRoot := AllocateNode;
  CreateChildren(FRoot, inpTreeDepth.Value - 1);
  Log(Format('Tree built in %.1f sec', [DSiElapsedTime(timeStart)/1000]));
end;

procedure TfrmRecursiveTreeDemo.btnSingleCoreTestClick(Sender: TObject);
var
  timeStart: int64;
begin
  if not assigned(FRoot) then
    btnBuildTree.Click;
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

procedure TfrmRecursiveTreeDemo.CreateChildren(node: PNode; subLevels: integer);
var
  iChild: integer;
begin
  if subLevels = 0 then begin
    node.Value := FValue;
    Inc(FValue);
    FillChar(node.Child, FNumChildren * SizeOf(PNode), 0)
  end
  else begin
    for iChild := 1 to FNumChildren do begin
      {$R-}
      node.Child[iChild] := AllocateNode;
      CreateChildren(node.Child[iChild], subLevels - 1);
      {$R+}
    end;
  end;
end;

procedure TfrmRecursiveTreeDemo.DestroyNode(node: PNode);
var
  iChild: integer;
begin
  if assigned(node.Child[1]) then
    for iChild := 1 to FNumChildren do
      {$R-} DestroyNode(node.Child[iChild]); {$R+}
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
    Log(Format('Tree destroyed in %.1f sec', [DSiElapsedTime(timeStart)/1000]));
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
end;

procedure TfrmRecursiveTreeDemo.Log(const msg: string);
begin
  lbLog.ItemIndex := lbLog.Items.Add(msg);
end;

procedure TfrmRecursiveTreeDemo.ProcessTree(node: PNode);
var
  iChild: integer;
begin
  if not assigned(node.Child[1]) then
    //leaf node, .Value already contains correct value
    Exit;
  node.Value := 0;
  for iChild := 1 to FNumChildren do begin
    {$R-}
    ProcessTree(node.Child[iChild]);
    node.Value := node.Value + node.Child[iChild].Value;
    {$R+}
  end;
end;

end.
