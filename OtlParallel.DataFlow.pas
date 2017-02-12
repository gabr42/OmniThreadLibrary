//Temporary research unit, will be merged to OtlParallel.

unit OtlParallel.DataFlow;

interface

uses
  OtlCommon;

type
  IOmniDFValue = interface
  end;

  IOmniDFNode = interface;
  IOmniDataFlow = interface;

  IOmniDFPipe = interface
    function Throttle(count: integer): IOmniDFNode;
  end;

  TOmniDFPredicate = reference to function (const value: IOmniDFValue): boolean;
  TOmniDFProcessValue = reference to procedure (const node: IOmniDFNode; const value: IOmniDFValue);
  TOmniDFProcessPipe = reference to procedure (const node: IOmniDFNode; const value: IOmniDFPipe);

  IOmniDFNode = interface
    function GetDataFlow: IOmniDataFlow;
  //
    function ConnectTo(const nodeName: string; const accept: TOmniDFPredicate = nil): IOmniDFPipe; overload;
    function ConnectTo(const node: IOmniDFNode; const accept: TOmniDFPredicate = nil): IOmniDFPipe; overload;
    function ConnectTo(const nodeNames: TArray<string>): IOmniDFPipe; overload;
    function ConnectTo(const nodeNames: TArray<IOmniDFNode>): IOmniDFPipe; overload;
    function NumTasks(value: integer): IOmniDFNode;
    function Execute(taskProc: TOmniDFProcessValue): IOmniDFNode; overload;
    function Execute(taskProc: TOmniDFProcessPipe): IOmniDFNode; overload;
    procedure Output(const value: TOmniValue); overload;
    procedure Output(const value: IOmniDFValue); overload;
    property DataFlow: IOmniDataFlow read GetDataFlow;
  end;

  IOmniDataFlow = interface
    function GetInput: IOmniDFNode;
    function GetOutput: IOmniDFNode;
    function GetNode(const name: string): IOmniDFNode;
  //
    procedure Start;
    procedure Stop;
    procedure StopWhenEmpty;
    function Add(const name: string = ''): IOmniDFNode;
    procedure Process(const value: TOmniValue); overload;
    procedure Process(const value: IOmniDFValue); overload;
    procedure Remove(const node: IOmniDFNode);
    function Running: boolean;
    property Input: IOmniDFNode read GetInput;
    property Output: IOmniDFNode read GetOutput;
    property Node[const name: string]: IOmniDFNode read GetNode; default;
  end;

implementation

procedure DownloadData(const node: IOmniDFNode; const value: IOmniDFValue);
var
  html: string;
begin
  // get html for value
  node.Output(html);
end;

procedure ParseData(const node: IOmniDFNode; const value: IOmniDFValue);
var
  link: string;
begin
//  for link in ExtractLinks(value) do
    node.Output(link);
//  end;
end;

procedure UpdateGUI(const node: IOmniDFNode; const pipe: IOmniDFPipe);
//var
//  value: IOmniDFValue;
begin
// lastTime := 0;
// for value in pipe do
//   if (lastTime = 0) or HasElapsed(lastTime, 500) then
//     lastTime := time;
//     Comm.Send(MSG_PARSING, value);
//  end;
end;

procedure SaveToDB(const node: IOmniDFNode; const pipe: IOmniDFPipe);
begin
  //Connect to database
  //if not connected - fail - how???
  //for value in pipe
  //  update database
  //Disconnect from the database
end;

procedure Test;
var
  df           : IOmniDataFlow;
  downloadNode : IOmniDFNode;
  parserNode   : IOmniDFNode;
  saveToDBNode : IOmniDFNode;
  updateGUINode: IOmniDFNode;
begin
  downloadNode := df.Add.NumTasks(4).Execute(DownloadData);
  parserNode := df.Add.NumTasks(2).Execute(ParseData);
  updateGUINode := df.Add.Execute(UpdateGUI);
  saveToDBNode := df.Add.Execute(SaveToDB);

  df.Input.ConnectTo(downloadNode);
  downloadNode.ConnectTo([parserNode, updateGUINode]);
  parserNode.ConnectTo(saveToDBNode);
  parserNode.ConnectTo(downloadNode,
    function (const value: IOmniDFValue): boolean
    begin
      // Result := NotAlreadySeen(value);
      Result := true;
    end).Throttle(100);

  df.Start;

  df.Process('http:\\www.omnithreadlibrary.com');
  df.StopWhenEmpty;

  while df.Running do
    ;

  df := nil;
end;

end.
