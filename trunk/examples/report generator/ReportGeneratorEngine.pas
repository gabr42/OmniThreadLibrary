unit ReportGeneratorEngine;

interface

uses
  Classes;

type
  IReportGenerator = interface;

  IReportInfo = interface ['{CDF09A38-11B0-4571-8908-AA5486D94A9A}']
    function GetClientName: string;
    function GetReportDelay_ms: integer;
    function GetReportName: string;
    function GetWorkerThread: cardinal;
  //
    property ClientName: string read GetClientName;
    property ReportName: string read GetReportName;
    property ReportDelay_ms: integer read GetReportDelay_ms;
    property WorkerThread: cardinal read GetWorkerThread;
  end;

  TReportGeneratorWorkerEvent = reference to procedure(Sender: IReportGenerator;
    const clientName: string);

  TReportGeneratorRequestDoneEvent = reference to procedure(Sender: IReportGenerator;
    const reportInfo: IReportInfo);

  IReportGenerator = interface
    function  GetOnCreateWorker: TReportGeneratorWorkerEvent;
    function  GetOnDestroyWorker: TReportGeneratorWorkerEvent;
    function  GetOnRequestDone_Asy: TReportGeneratorRequestDoneEvent;
    procedure SetOnCreateWorker(const value: TReportGeneratorWorkerEvent);
    procedure SetOnDestroyWorker(const value: TReportGeneratorWorkerEvent);
    procedure SetOnRequestDone_Asy(const value: TReportGeneratorRequestDoneEvent);
  //
    procedure Schedule(const clientName, reportName: string; reportDelay_ms: integer);
    procedure Stop;
    property OnCreateWorker: TReportGeneratorWorkerEvent read GetOnCreateWorker
      write SetOnCreateWorker;
    property OnDestroyWorker: TReportGeneratorWorkerEvent read GetOnDestroyWorker
      write SetOnDestroyWorker;
    property OnRequestDone_Asy: TReportGeneratorRequestDoneEvent read GetOnRequestDone_Asy
      write SetOnRequestDone_Asy;
  end;

function CreateReportGenerator: IReportGenerator;

implementation

uses
  Windows,
  SysUtils,
  Generics.Collections,
  OtlCommon,
  OtlSync,
  OtlParallel;

type
  IReportInfoEx = interface ['{626481CD-C43C-4C4F-95C6-12BC4A0FB51E}']
    procedure SetWorkerThread(value: cardinal);
  end;

  TReportInfo = class(TInterfacedObject, IReportInfo, IReportInfoEx)
  strict private
    FClientName    : string;
    FReportDelay_ms: integer;
    FReportName    : string;
    FWorkerThread  : cardinal;
  strict protected
    function  GetClientName: string;
    function  GetReportDelay_ms: integer;
    function  GetReportName: string;
    function  GetWorkerThread: cardinal;
    procedure SetWorkerThread(value: cardinal);
  public
    constructor Create(const clientName, reportName: string; reportDelay_ms: integer);
    property ClientName: string read GetClientName;
    property ReportDelay_ms: integer read GetReportDelay_ms;
    property ReportName: string read GetReportName;
    property WorkerThread: cardinal read GetWorkerThread;
  end;

  TReportWorkerInfo = record
    Worker       : IOmniBackgroundWorker;
    WorkItemCount: IOmniCounter;
  end;

  TReportGenerator = class(TInterfacedObject, IReportGenerator)
  strict private
    FDestroyList      : TStringList;
    FLock             : TOmniCS;
    FOnCreateWorker   : TReportGeneratorWorkerEvent;
    FOnDestroyWorker  : TReportGeneratorWorkerEvent;
    FOnRequestDone_Asy: TReportGeneratorRequestDoneEvent;
    FWorkerDict       : TDictionary<string,TReportWorkerInfo>;
  strict protected
    function  GetOnCreateWorker: TReportGeneratorWorkerEvent;
    function  GetOnDestroyWorker: TReportGeneratorWorkerEvent;
    function  GetOnRequestDone_Asy: TReportGeneratorRequestDoneEvent;
    procedure SetOnCreateWorker(const value: TReportGeneratorWorkerEvent);
    procedure SetOnDestroyWorker(const value: TReportGeneratorWorkerEvent);
    procedure SetOnRequestDone_Asy(const value: TReportGeneratorRequestDoneEvent);
  strict protected
    procedure GenerateReport(const workItem: IOmniWorkItem);
    function  AcquireWorker(const clientName: string): IOmniBackgroundWorker;
    procedure ProcessDestroyList(const scheduledClient: string);
    procedure ReleaseWorker_Asy(const clientName: string);
    procedure RequestDone_Asy(const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Schedule(const clientName, reportName: string; reportDelay_ms: integer);
    procedure Stop;
  end;

{ exports }

function CreateReportGenerator: IReportGenerator;
begin
  Result := TReportGenerator.Create;
end;

{ TReportInfo }

constructor TReportInfo.Create(const clientName, reportName: string;
  reportDelay_ms: integer);
begin
  inherited Create;
  FClientName := clientName;
  FReportName := reportName;
  FReportDelay_ms := reportDelay_ms;
end;

function TReportInfo.GetClientName: string;
begin
  Result := FClientName;
end;

function TReportInfo.GetReportDelay_ms: integer;
begin
  Result := FReportDelay_ms;
end;

function TReportInfo.GetReportName: string;
begin
  Result := FReportName;
end;

function TReportInfo.GetWorkerThread: cardinal;
begin
  Result := FWorkerThread;
end;

procedure TReportInfo.SetWorkerThread(value: cardinal);
begin
  FWorkerThread := value;
end;

{ TReportGenerator }

constructor TReportGenerator.Create;
begin
  inherited;
  FWorkerDict := TDictionary<string,TReportWorkerInfo>.Create;
  FDestroyList := TStringList.Create;
  GlobalParallelPool.MaxExecuting := -1; //unlimited tasks
end;

destructor TReportGenerator.Destroy;
begin
  Stop;
  FreeAndNil(FDestroyList);
  FreeAndNil(FWorkerDict);
  inherited;
end;

procedure TReportGenerator.GenerateReport(const workItem: IOmniWorkItem);
var
  reportInfo: IReportInfo;
begin
  reportInfo := workItem.Data.AsInterface as IReportInfo;
  // Simulate heavy work
  Sleep(reportInfo.ReportDelay_ms);
  // Return result
  (reportInfo as IReportInfoEx).SetWorkerThread(GetCurrentThreadID);
  workItem.Result := reportInfo;
end;

function TReportGenerator.GetOnRequestDone_Asy: TReportGeneratorRequestDoneEvent;
begin
  Result := FOnRequestDone_Asy;
end;

function TReportGenerator.AcquireWorker(const clientName: string): IOmniBackgroundWorker;
var
  workerInfo: TReportWorkerInfo;
begin
  FLock.Acquire;
  try
    if not FWorkerDict.TryGetValue(clientName, workerInfo) then begin
      workerInfo.Worker :=
        Parallel.BackgroundWorker.NumTasks(1)
          .OnRequestDone_Asy(RequestDone_asy)
          .Execute(GenerateReport);
      workerInfo.WorkItemCount := CreateCounter(0);
      FWorkerDict.Add(clientName, workerInfo);
      if assigned(FOnCreateWorker) then
        FOnCreateWorker(Self, clientName);
    end;
    workerInfo.WorkItemCount.Increment;
    ProcessDestroyList(clientName);
    Result := workerInfo.Worker;
  finally FLock.Release; end;
end;

function TReportGenerator.GetOnCreateWorker: TReportGeneratorWorkerEvent;
begin
  Result := FOnCreateWorker;
end;

function TReportGenerator.GetOnDestroyWorker: TReportGeneratorWorkerEvent;
begin
  Result := FOnDestroyWorker;
end;

procedure TReportGenerator.ProcessDestroyList(const scheduledClient: string);
var
  clientName: string;
  workerInfo: TReportWorkerInfo;
begin
  // This methods is only called when FLock is acquired
  // 'ScheduledClient' contains name of the client being scheduled which must not be destroyed
  for clientName in FDestroyList do begin
    if clientName = scheduledClient then
      continue;
    workerInfo := FWorkerDict[clientName];
    workerInfo.Worker.Terminate(INFINITE);
    workerInfo.Worker := nil;
    workerInfo.WorkItemCount := nil;
    FWorkerDict.Remove(clientName);
    if assigned(FOnDestroyWorker) then
      FOnDestroyWorker(Self, clientName);
  end;
  FDestroyList.Clear;
end;

procedure TReportGenerator.ReleaseWorker_Asy(const clientName: string);
var
  workerInfo: TReportWorkerInfo;
begin
  FLock.Acquire;
  try
    workerInfo := FWorkerDict[clientName];
    if workerInfo.WorkItemCount.Decrement = 0 then
      FDestroyList.Add(clientName);
  finally FLock.Release; end;
end;

procedure TReportGenerator.RequestDone_Asy(const Sender: IOmniBackgroundWorker;
  const workItem: IOmniWorkItem);
var
  reportInfo: IReportInfo;
begin
  reportInfo := (workItem.Result.AsInterface as IReportInfo);
  ReleaseWorker_Asy(reportInfo.ClientName);
  if assigned(FOnRequestDone_Asy) then
    FOnRequestDone_Asy(Self, reportInfo);
end;

procedure TReportGenerator.Schedule(const clientName, reportName: string;
  reportDelay_ms: integer);
var
  data  : IReportInfo;
  worker: IOmniBackgroundWorker;
begin
  worker := AcquireWorker(clientName);
  data := TReportInfo.Create(clientName, reportName, reportDelay_ms);
  worker.Schedule(worker.CreateWorkItem(data));
end;

procedure TReportGenerator.SetOnCreateWorker(const value: TReportGeneratorWorkerEvent);
begin
  FOnCreateWorker := value;
end;

procedure TReportGenerator.SetOnDestroyWorker(const value: TReportGeneratorWorkerEvent);
begin
  FOnDestroyWorker := value;
end;

procedure TReportGenerator.SetOnRequestDone_Asy(const value: TReportGeneratorRequestDoneEvent);
begin
  FOnRequestDone_Asy := value;
end;

procedure TReportGenerator.Stop;
var
  dictItem: TPair<string,TReportWorkerInfo>;
begin
  for dictItem in FWorkerDict do // tell all workers to stop
    dictItem.Value.Worker.Terminate(0);
  for dictItem in FWorkerDict do // wait for all workers to stop
    dictItem.Value.Worker.Terminate(INFINITE);
end;

end.

