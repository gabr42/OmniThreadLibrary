//Temporary research unit, will be merged to OtlParallel.

unit OtlParallel.DataFlow;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  OtlCommon,
  OtlParallel, OtlTask;

type
  IOmniDFValue = interface
    function GetValue: TOmniValue;
    procedure SetValue(const value: TOmniValue);
  //
    property Value: TOmniValue read GetValue write SetValue;
  end;

  IOmniDFNode = interface;
  IOmniDataFlow = interface;

  IOmniDFPipe = interface
    function Throttle(count: integer): IOmniDFNode;
    function GetEnumerator: TEnumerator<IOmniDFValue>;
  end;

//  TOmniDFFilterResult = (frAccepted, ftRejected, frBusy);
//  TOmniDFFilter = reference to function (const value: IOmniDFValue): TOmniDFFilterResult;

  TOmniDFPredicate = reference to function (const value: IOmniDFValue): boolean;
  TOmniDFProcessValue = reference to procedure (const node: IOmniDFNode; const value: IOmniDFValue);
  TOmniDFProcessPipe = reference to procedure (const node: IOmniDFNode; const value: IOmniDFPipe);

  IOmniDFNode = interface
    function GetDataFlow: IOmniDataFlow;
    function GetTask: IOmniTask;
  //
    function ConnectTo(const node: IOmniDFNode; const accept: TOmniDFPredicate = nil): IOmniDFPipe; overload;
//    function ConnectTo(const node: IOmniDFNode; const accept: TOmniDFFilter = nil): IOmniDFPipe; overload;
    function ConnectTo(const nodes: TArray<IOmniDFNode>): IOmniDFPipe; overload;
    function NumTasks(value: integer): IOmniDFNode;
    procedure Output(const value: TOmniValue); overload;
    procedure Output(const value: IOmniDFValue); overload;
    function TaskConfig(const config: IOmniTaskConfig): IOmniDFNode;
    property DataFlow: IOmniDataFlow read GetDataFlow;
    property Task: IOmniTask read GetTask;
  end;

  IOmniDataFlow = interface
    function GetInput: IOmniDFNode;
    function GetOutput: IOmniDFNode;
    function GetNode(const nodeName: string): IOmniDFNode;
  //
    procedure Start;
    procedure Stop;
    procedure StopWhenEmpty;
    function Add(taskProc: TOmniDFProcessValue; const nodeName: string = ''): IOmniDFNode; overload;
    function Add(taskProc: TOmniDFProcessPipe; const nodeName: string = ''): IOmniDFNode; overload;
    procedure Process(const value: TOmniValue); overload;
    procedure Process(const value: IOmniDFValue); overload;
    procedure Remove(const node: IOmniDFNode);
    function Running: boolean;
    property Input: IOmniDFNode read GetInput;
    property Output: IOmniDFNode read GetOutput;
    procedure TaskConfig(const config: IOmniTaskConfig);
    property Node[const nodeName: string]: IOmniDFNode read GetNode;
  end;

implementation

uses
  OtlTaskControl,
  OtlComm;

const
  MSG_PARSING = 123;

procedure ShowCurrentPage(const task: IOmniTaskControl; const msg: TOmniMessage);
begin
  // show msg on screen
end;

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
var
  value: IOmniDFValue;
begin
// lastTime := 0;
 for value in pipe do begin
//   if (lastTime = 0) or HasElapsed(lastTime, 500) then
//     lastTime := time;
    node.Task.Comm.Send(MSG_PARSING, value);
  end;
end;

procedure SaveToDB(const node: IOmniDFNode; const pipe: IOmniDFPipe);
var
  value: IOmniDFValue;
begin
  if not false {Connect to database} then
    raise Exception.Create('DB error');
  for value in pipe do
    ; //  update database
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
  downloadNode := df.Add(DownloadData).NumTasks(4);
  parserNode := df.Add(ParseData).NumTasks(2);
  updateGUINode := df.Add(UpdateGUI).TaskConfig(
    Parallel.TaskConfig.OnMessage(MSG_PARSING, ShowCurrentPage));
  saveToDBNode := df.Add(SaveToDB);

  df.Input.ConnectTo(downloadNode);
  downloadNode.ConnectTo([parserNode, updateGUINode]);
  parserNode.ConnectTo(saveToDBNode);
  parserNode.ConnectTo(downloadNode,
    function (const value: IOmniDFValue): boolean
    begin
      // Result := NotAlreadySeen(value);
      Result := true;
    end).Throttle(100);

  df.TaskConfig(Parallel.TaskConfig.OnTerminated(
    procedure (const taskControl: IOmniTaskControl)
    begin
      df := nil;
    end));

  df.Start;

  df.Process('http:\\www.omnithreadlibrary.com');
  df.StopWhenEmpty;
end;

end.
