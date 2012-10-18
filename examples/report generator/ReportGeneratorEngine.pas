unit ReportGeneratorEngine;

interface

type
  IReportGenerator = interface;

  IReportInfo = interface ['{CDF09A38-11B0-4571-8908-AA5486D94A9A}']
    function GetClientName: string;
    function GetReportDelay: integer;
    function GetReportName: string;
    function GetWorkerThread: cardinal;
  //
    property ClientName: string read GetClientName;
    property ReportName: string read GetReportName;
    property ReportDelay: integer read GetReportDelay;
    property WorkerThread: cardinal read GetWorkerThread;
  end;

  TReportGeneratorRequestDone = reference to procedure(Sender: IReportGenerator;
    const reportInfo: IReportInfo);

  IReportGenerator = interface
    function  GetOnRequestDone_Asy: TReportGeneratorRequestDone;
    procedure SetOnRequestDone_Asy(const value: TReportGeneratorRequestDone);
  //
    procedure Schedule(const clientName, reportName: string; reportDelay: integer);
    property OnRequestDone_Asy: TReportGeneratorRequestDone read GetOnRequestDone_Asy
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
    FClientName  : string;
    FReportDelay : integer;
    FReportName  : string;
    FWorkerThread: cardinal;
  strict protected
    function  GetClientName: string;
    function  GetReportDelay: integer;
    function  GetReportName: string;
    function  GetWorkerThread: cardinal;
    procedure SetWorkerThread(value: cardinal);
  public
    constructor Create(const clientName, reportName: string; reportDelay: integer);
    property ClientName: string read GetClientName;
    property ReportDelay: integer read GetReportDelay;
    property ReportName: string read GetReportName;
    property WorkerThread: cardinal read GetWorkerThread;
  end;

  TReportWorkerInfo = record
    Worker       : IOmniBackgroundWorker;
    WorkItemCount: IOmniCounter;
  end;

  TReportGenerator = class(TInterfacedObject, IReportGenerator)
  strict private
    FLock             : TOmniCS;
    FOnRequestDone_Asy: TReportGeneratorRequestDone;
    FWorkerDict       : TDictionary<string,TReportWorkerInfo>;
  strict protected
    procedure GenerateReport(const workItem: IOmniWorkItem);
    function  GetOnRequestDone_Asy: TReportGeneratorRequestDone;
    function  AcquireWorker(const clientName: string): IOmniBackgroundWorker;
    procedure RequestDone_Asy(const Sender: IOmniBackgroundWorker; const workItem: IOmniWorkItem);
    procedure Schedule(const clientName, reportName: string; reportDelay: integer);
    procedure SetOnRequestDone_Asy(const value: TReportGeneratorRequestDone);
    property OnRequestDone_Asy: TReportGeneratorRequestDone read GetOnRequestDone_Asy
      write SetOnRequestDone_Asy;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure ReleaseWorker_Asy(const clientName: string);
  end;

function CreateReportGenerator: IReportGenerator;
begin
  Result := TReportGenerator.Create;
end;

{ TReportInfo }

constructor TReportInfo.Create(const clientName, reportName: string; reportDelay:
  integer);
begin
  inherited Create;
  FClientName := clientName;
  FReportName := reportName;
  FReportDelay := reportDelay;
end;

function TReportInfo.GetClientName: string;
begin
  Result := FClientName;
end;

function TReportInfo.GetReportDelay: integer;
begin
  Result := FReportDelay;
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
  GlobalParallelPool.MaxExecuting := -1; //unlimited tasks
end;

destructor TReportGenerator.Destroy;
begin
  FreeAndNil(FWorkerDict);
  inherited;
end;

procedure TReportGenerator.GenerateReport(const workItem: IOmniWorkItem);
var
  reportInfo: IReportInfo;
begin
  reportInfo := workItem.Data.AsInterface as IReportInfo;
  // Simulate heavy work
  Sleep(reportInfo.ReportDelay);
  // Return result
  (reportInfo as IReportInfoEx).SetWorkerThread(GetCurrentThreadID);
  workItem.Result := reportInfo;
end;

function TReportGenerator.GetOnRequestDone_Asy: TReportGeneratorRequestDone;
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
    end;
    workerInfo.WorkItemCount.Increment;
    Result := workerInfo.Worker;
  finally FLock.Release; end;
end;

procedure TReportGenerator.ReleaseWorker_Asy(const clientName: string);
var
  workerInfo: TReportWorkerInfo;
begin
  FLock.Acquire;
  try
    workerInfo := FWorkerDict[clientName];
    if workerInfo.WorkItemCount.Decrement = 0 then begin
      workerInfo.Worker.Terminate(INFINITE);
      workerInfo.Worker := nil;
      workerInfo.WorkItemCount := nil;
      FWorkerDict.Remove(clientName);
    end;
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
  reportDelay: integer);
var
  worker: IOmniBackgroundWorker;
begin
  worker := AcquireWorker(clientName);
  worker.Schedule(worker.CreateWorkItem(TReportInfo.Create(clientName, reportName, reportDelay)));
end;

procedure TReportGenerator.SetOnRequestDone_Asy(const value: TReportGeneratorRequestDone);
begin
  FOnRequestDone_Asy := value;
end;

end.

