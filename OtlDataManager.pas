unit OtlDataManager;

interface

uses
  GpStuff,
  OtlCommon,
  OtlContainers,
  OtlCollections;

type
  ///<summary>Source provider capabilities.</summary>
  TOmniSourceProviderCapability = (
    spcCountable,  // source provider that knows how many data it holds
    spcFast,       // source provider operations are O(1)
    spcDataLimit   // data package can only hold limited amount of data
  );
  TOmniSourceProviderCapabilities = set of TOmniSourceProviderCapability;

  ///<summary>Wrapper around a (type specific) data package. All methods can and will be
  ///    called from multiple threads at the same time!</summary>
  TOmniDataPackage = class abstract
  public
    function  GetNext(var value: TOmniValue): boolean; overload; virtual; abstract;
    function  GetNext(var position: int64; var value: TOmniValue): boolean; overload; virtual; abstract;
    function  Split(package: TOmniDataPackage): boolean; virtual; abstract;
  end; { TOmniDataPackage }

  ///<summary>A data package queue between a single worker and shared data manager.</summary>
  TOmniLocalQueue = class abstract
  public
    function  GetNext(var value: TOmniValue): boolean; overload; virtual; abstract;
    function  GetNext(var position: int64; var value: TOmniValue): boolean; overload; virtual; abstract;
    function  Split(package: TOmniDataPackage): boolean; virtual; abstract;
  end; { TOmniLocalQueue }

  ///<summary>Wrapper around the data source. All methods can and will be called from
  ///    multiple threads at the same time!</summary>
  TOmniSourceProvider = class abstract
  public
    function  Count: int64; virtual; abstract;
    function  CreateDataPackage: TOmniDataPackage; virtual; abstract;
    function  GetCapabilities: TOmniSourceProviderCapabilities; virtual; abstract;
    function  GetPackage(dataCount: integer; package: TOmniDataPackage): boolean; virtual; abstract;
    function  GetPackageSizeLimit: integer; virtual; abstract;
  end; { TOmniSourceProvider }

  TOmniOutputBuffer = class abstract
  public
    procedure Submit(position: int64; const data: TOmniValue); virtual; abstract;
  end; { TOmniOutputBuffer }

  TOmniDataManagerOption = (dmoPreserveOrder);
  TOmniDataManagerOptions = set of TOmniDataManagerOption;

  ///<summary>Data manager. All methods can and will be called from multiple threads
  ///    at the same time!</summary>
  TOmniDataManager = class abstract
  public
    function  CreateLocalQueue: TOmniLocalQueue; virtual; abstract;
    function  AllocateOutputBuffer: TOmniOutputBuffer; virtual; abstract;
    procedure Flush; virtual; abstract;
    function  GetNext(package: TOmniDataPackage): boolean; virtual; abstract;
    procedure ReleaseOutputBuffer(buffer: TOmniOutputBuffer); virtual; abstract;
    procedure SetOutput(const queue: IOmniBlockingCollection); overload; virtual; abstract;
  end; { TOmniDataManager }

function  CreateSourceProvider(low, high: integer; step: integer = 1): TOmniSourceProvider; overload;
function  CreateSourceProvider(enumerator: TOmniValueEnumerator): TOmniSourceProvider; overload;
function  CreateSourceProvider(enumerator: IOmniValueEnumerator): TOmniSourceProvider; overload;
function  CreateSourceProvider(enumerator: IEnumerator): TOmniSourceProvider; overload;

function  CreateDataManager(sourceProvider: TOmniSourceProvider; numWorkers: integer;
  options: TOmniDataManagerOptions): TOmniDataManager;

(*
type
  TOmniLogger = class
  strict private
    eventList: TOmniBaseQueue;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Clear;
    procedure GetEventList(sl: TStringList);
    procedure Log(const msg: string; const params: array of const); overload;
    procedure Log(const msg: string); overload;
  end; { TOmniLogger }

var
  GLogger: TOmniLogger;
*)

implementation

uses
  Windows,
  SysUtils,
  Contnrs,
  Classes,
  DSiWin32,
  OtlSync;

type
  ///<summary>Base class for all data package classes.</summary>
  TOmniDataPackageBase = class abstract(TOmniDataPackage)
  private
    dpbGeneration: integer;
    dpbPosition  : int64;
    dpbQueue     : TOmniLocalQueue;
  strict protected
    procedure SetPosition(position: int64);
  public
    procedure NextGeneration; inline;
    property Generation: integer read dpbGeneration;
    property Position: int64 read dpbPosition;
    property Queue: TOmniLocalQueue read dpbQueue write dpbQueue;
  end; { TOmniDataPackageBase }

  ///<summary>Integer range data package.</summary>
  TOmniIntegerDataPackage = class(TOmniDataPackageBase)
  strict private
    idpHigh    : int64;
    idpHighSign: int64;
    idpLow     : TGp8AlignedInt64;
    idpPosition: integer;
    idpSign    : integer;
    idpStep    : integer;
    idpStepAbs : integer;
  public
    function  GetNext(var value: TOmniValue): boolean; overload; override;
    function  GetNext(var position: int64; var value: TOmniValue): boolean; overload; override;
    function  HasData: boolean;
    procedure Initialize(low, high, step, position: integer);
    function  Split(package: TOmniDataPackage): boolean; override;
  end; { TOmniIntegerDataPackage }

  ///<summary>Data package storing a list of TOmniValues.</summary>
  TOmniValueEnumeratorDataPackage = class(TOmniDataPackageBase)
  strict private
    const CMaxValueEnumeratorDataPackageSize = 1024; // pretty arbitrary, should do some performance tests
  strict private
    vedpApproxCount: TGp4AlignedInt;
    vedpDataQueue  : TOmniBaseQueue; //TOmniBaseBoundedQueue;
  public
    constructor Create;
    destructor  Destroy; override;
    class function GetPackageSizeLimit: integer;
    procedure Add(const value: TOmniValue);
    function  GetNext(var value: TOmniValue): boolean; overload; override;
    function  GetNext(var position: int64; var value: TOmniValue): boolean; overload; override;
    function  Prepare(dataCount: integer): integer;
    function  Split(package: TOmniDataPackage): boolean; override;
  end; { TOmniValueEnumeratorDataPackage }

  TOmniSourceProviderBase = class(TOmniSourceProvider)
  strict private
    spbStorePositions: boolean;
  public
    property StorePositions: boolean read spbStorePositions write spbStorePositions;
  end; { TOmniSourceProviderBase }

  ///<summary>Integer range source provider.</summary>
  TOmniIntegerRangeProvider = class(TOmniSourceProviderBase)
  strict private
    irpCount   : TGp4AlignedInt;
    irpHigh    : integer;
    irpLock    : TOmniCS;
    irpLow     : integer;
    irpPosition: integer;
    irpStep    : integer;
  strict protected
    function  CalcCount(low, high, step: integer): integer; inline;
  public
    constructor Create(low, high, step: integer);
    function  Count: int64; override;
    function  CreateDataPackage: TOmniDataPackage; override;
    function  GetCapabilities: TOmniSourceProviderCapabilities; override;
    function  GetPackage(dataCount: integer; package: TOmniDataPackage): boolean; override;
    function  GetPackageSizeLimit: integer; override;
  end; { TOmniIntegerRangeProvider }

  ///<summary>TOmniValue source provider supporting mutable collections and parallel enumerators.</summary>
  TOmniValueEnumeratorProvider = class(TOmniSourceProviderBase)
  strict protected
    vepEnumerator: IOmniValueEnumerator;
  public
    constructor Create(enumerator: IOmniValueEnumerator);
    function  Count: int64; override;
    function  CreateDataPackage: TOmniDataPackage; override;
    function  GetCapabilities: TOmniSourceProviderCapabilities; override;
    function  GetPackage(dataCount: integer; package: TOmniDataPackage): boolean; override;
    function  GetPackageSizeLimit: integer; override;
  end; { TOmniValueEnumeratorProvider }

  TOmniEnumeratorProvider = class(TOmniSourceProviderBase)
  strict private
    epEnumeratorIntf: IEnumerator;
    epEnumeratorObj : TOmniValueEnumerator;
    epEnumLock      : TOmniCS;
  public
    constructor Create(enumerator: IEnumerator); overload;
    constructor Create(enumerator: TOmniValueEnumerator); overload;
    function  Count: int64; override;
    function  CreateDataPackage: TOmniDataPackage; override;
    function  GetCapabilities: TOmniSourceProviderCapabilities; override;
    function  GetPackage(dataCount: integer; package: TOmniDataPackage): boolean; override;
    function  GetPackageSizeLimit: integer; override;
    property EnumeratorIntf: IEnumerator read epEnumeratorIntf;
    property EnumeratorObj: TOmniValueEnumerator read epEnumeratorObj;
  end; { TOmniEnumeratorProvider }

  TOmniBaseDataManager = class;

  ///<summary>Local queue implementation.</summary>
  TOmniLocalQueueImpl = class(TOmniLocalQueue)
  strict private
    lqiDataManager_ref: TOmniBaseDataManager;
    lqiDataPackage    : TOmniDataPackage;
  public
    constructor Create(owner: TOmniBaseDataManager);
    destructor  Destroy; override;
    function  GetNext(var value: TOmniValue): boolean; overload; override;
    function  GetNext(var position: int64; var value: TOmniValue): boolean; overload; override;
    function  Split(package: TOmniDataPackage): boolean; override;
  end; { TOmniLocalQueueImpl }

  TOmniOutputBufferImpl = class(TOmniOutputBuffer)
  strict private
    obiBuffer: TOmniBlockingCollection;
    obiOutput: IOmniBlockingCollection;
  public
    constructor Create(output: IOmniBlockingCollection);
    destructor  Destroy; override;
    procedure Submit(position: int64; const data: TOmniValue); override;
  end; { TOmniOutputBufferImpl }

  ///<summary>Base data manager class.</summary>
  TOmniBaseDataManager = class abstract (TOmniDataManager)
  strict private
    dmNextPosition      : int64;
    dmNumWorkers        : integer;
    dmOptions           : TOmniDataManagerOptions;
    dmOutputIntf        : IOmniBlockingCollection;
    dmPacketSizes       : array of integer;
    dmQueueList         : TObjectList;
    dmQueueLock         : TOmniCS;
    dmSourceProvider_ref: TOmniSourceProviderBase;
    dmStealIdx          : integer;
  strict protected
    function  GetSourceProvider: TOmniSourceProvider;
    procedure InitializePacketSizes;
  public
    constructor Create(sourceProvider: TOmniSourceProvider; numWorkers: integer;
      options: TOmniDataManagerOptions);
    destructor  Destroy; override;
    function  AllocateOutputBuffer: TOmniOutputBuffer; override;
    function  CreateLocalQueue: TOmniLocalQueue; override;
    procedure Flush; override;
    function  GetDataCountForGeneration(generation: integer): integer;
    function  GetNext(package: TOmniDataPackage): boolean; override;
    function  GetNextFromProvider(package: TOmniDataPackage;
      generation: integer): boolean; virtual; abstract;
    procedure LocalQueueDestroyed(queue: TOmniLocalQueue);
    procedure ReleaseOutputBuffer(buffer: TOmniOutputBuffer); override;
    procedure SetOutput(const queue: IOmniBlockingCollection); overload; override;
    function  StealPackage(package: TOmniDataPackage): boolean;
    property SourceProvider: TOmniSourceProvider read GetSourceProvider;
  end; { TOmniBaseDataManager }

  ///<summary>Data manager for countable data.</summary>
  TOmniCountableDataManager = class(TOmniBaseDataManager)
  public
    function  GetNextFromProvider(package: TOmniDataPackage;
      generation: integer): boolean; override;
  end; { TOmniCountableDataManager }

  ///<summary>Data manager for unbounded data.</summary>
  TOmniHeuristicDataManager = class(TOmniBaseDataManager)
  strict private
    const CFetchTimeout_ms = 10;
  strict private
    hdmEstimatedPackageSize: TGp4AlignedInt;
  public
    constructor Create(sourceProvider: TOmniSourceProvider; numWorkers: integer; options:
      TOmniDataManagerOptions);
    function  GetNextFromProvider(package: TOmniDataPackage;
      generation: integer): boolean; override;
  end; { TOmniHeuristicDataManager }

{ exports }

function CreateSourceProvider(low, high, step: integer): TOmniSourceProvider;
begin
  Result := TOmniIntegerRangeProvider.Create(low, high, step);
end; { CreateSourceProvider }

function CreateSourceProvider(enumerator: IOmniValueEnumerator): TOmniSourceProvider;
begin
  Result := TOmniValueEnumeratorProvider.Create(enumerator);
end; { CreateSourceProvider }

function CreateSourceProvider(enumerator: IEnumerator): TOmniSourceProvider; overload;
begin
  Result := TOmniEnumeratorProvider.Create(enumerator);
end; { CreateSourceProvider }

function CreateSourceProvider(enumerator: TOmniValueEnumerator): TOmniSourceProvider; overload;
begin
  Result := TOmniEnumeratorProvider.Create(enumerator);
end; { CreateSourceProvider }

function CreateDataManager(sourceProvider: TOmniSourceProvider; numWorkers: integer;
  options: TOmniDataManagerOptions): TOmniDataManager;
begin
  if spcCountable in sourceProvider.GetCapabilities then
    Result := TOmniCountableDataManager.Create(sourceProvider, numWorkers, options)
  else
    Result := TOmniHeuristicDataManager.Create(sourceProvider, numWorkers, options);
end; { CreateDataManager }

{ TOmniDataPackageBase }

procedure TOmniDataPackageBase.NextGeneration;
begin
  dpbGeneration := Abs((dpbGeneration SHL 1) + 3) SHR 1;
end; { TOmniDataPackageBase.NextGeneration }

procedure TOmniDataPackageBase.SetPosition(position: int64);
begin
  dpbPosition := position;
end; { TOmniDataPackageBase.SetPosition }

{ TOmniIntegerDataPackage }

function TOmniIntegerDataPackage.GetNext(var value: TOmniValue): boolean;
begin
  if not HasData then
    Result := false
  else begin
    value.AsInt64 := idpLow.Add(idpStep);
    if idpStep > 0 then
      Result := (value.AsInt64 <= idpHigh)
    else
      Result := (value.AsInt64 >= idpHigh);
  end;
end; { TOmniIntegerDataPackage.GetNext }

function TOmniIntegerDataPackage.GetNext(var position: int64; var value: TOmniValue):
  boolean;
begin
  Result := GetNext(value);
  if Result then begin
    position := idpPosition;
    Inc(idpPosition);
  end;
end; { TOmniIntegerDataPackage.GetNext }

function TOmniIntegerDataPackage.HasData: boolean;
begin
  Result := idpStep <> 0;
end; { TOmniIntegerDataPackage.HasData }

procedure TOmniIntegerDataPackage.Initialize(low, high, step, position: integer);
begin
  {$IFDEF Debug}Assert(step <> 0);{$ENDIF}
  idpLow.Value := low;
  idpHigh := high;
  idpStep := step;
  idpStepAbs := Abs(step);
  if idpStep > 0 then
    idpSign := 1
  else
    idpSign := -1;
  idpHighSign := idpHigh * idpSign;
  SetPosition(position);
end; { TOmniIntegerDataPackage.Initialize }

function TOmniIntegerDataPackage.Split(package: TOmniDataPackage): boolean;
var
  high      : integer;
  intPackage: TOmniIntegerDataPackage absolute package;
  low       : integer;
  midSteps  : integer;
begin
  {$IFDEF Debug}Assert(package is TOmniIntegerDataPackage);{$ENDIF}
  if not HasData then
    Result := false
  else begin
    low := idpLow.Value;
    if (low * idpSign) > idpHighSign then
      Result := false
    else begin
      midSteps := (((idpHigh - idpLow.Value) div 2 + 1) div idpStep) * idpStep;
      low := idpLow.Add(midSteps);
      high := low + midSteps - idpStep;
      Result := (low * idpSign <= idpHighSign);
      if Result and (high * idpSign > idpHighSign) then
        high := (idpHigh div idpStepAbs) * idpStepAbs;
      if Result then begin
        intPackage.Initialize(low, high, idpStep, 0);
      end;
    end;
  end;
end; { TOmniIntegerDataPackage.Split }

{ TOmniIntegerRangeProvider }

constructor TOmniIntegerRangeProvider.Create(low, high, step: integer);
begin
  inherited Create;
  {$IFDEF Debug}Assert(step <> 0);{$ENDIF}
  irpLow := low;
  irpHigh := high;
  irpStep := step;
  irpPosition := 1;
  irpCount.Value := CalcCount(low, high, step);
end; { TOmniIntegerRangeProvider.Create }

function TOmniIntegerRangeProvider.CalcCount(low, high, step: integer): integer;
begin
  if step > 0 then
    Result := (high - low + step) div step
  else
    Result := (low - high - step) div (-step);
end; { TOmniIntegerRangeProvider.CalcCount }

function TOmniIntegerRangeProvider.Count: int64;
begin
  Result := irpCount.Value;
end; { TOmniIntegerRangeProvider.Count }

function TOmniIntegerRangeProvider.CreateDataPackage: TOmniDataPackage;
begin
  Result := TOmniIntegerDataPackage.Create;
end; { TOmniIntegerRangeProvider.CreateDataPackage }

function TOmniIntegerRangeProvider.GetCapabilities: TOmniSourceProviderCapabilities;
begin
  Result := [spcCountable, spcFast];
end; { TOmniIntegerRangeProvider.GetCapabilities }

function TOmniIntegerRangeProvider.GetPackage(dataCount: integer; package:
  TOmniDataPackage): boolean;
var
  high      : int64;
  intPackage: TOmniIntegerDataPackage absolute package;
begin
  {$IFDEF Debug}Assert(package is TOmniIntegerDataPackage);{$ENDIF}
  {$IFDEF Debug}Assert(dataCount > 0);{$ENDIF}
  if irpCount.Value <= 0 then
    Result := false
  else begin
    irpLock.Acquire;
    try
      if dataCount > irpCount.Value then
        dataCount := irpCount.Value;
      high := irpLow + (dataCount - 1) * irpStep;
      intPackage.Initialize(irpLow, high, irpStep, irpPosition);
      Inc(irpPosition, dataCount);
      irpLow := high + irpStep;
      irpCount.Value := CalcCount(irpLow, irpHigh, irpStep);
    finally irpLock.Release; end;
    Result := true;
  end;
end; { TOmniIntegerRangeProvider.GetPackage }

function TOmniIntegerRangeProvider.GetPackageSizeLimit: integer;
begin
  raise Exception.Create('No limit for TOmniIntegerRangeProvider packages');
end; { TOmniIntegerRangeProvider.GetPackageSizeLimit }

{ TOmniValueEnumeratorDataPackage }

constructor TOmniValueEnumeratorDataPackage.Create;
begin
  inherited Create;
  vedpDataQueue := TOmniBaseQueue.Create; //TOmniBaseBoundedQueue.Create;
//  vedpDataQueue.Initialize(2*CMaxValueEnumeratorDataPackageSize+2, SizeOf(TOmniValue)); // there are bugs in the TOmniBaseBoundedQueue
end; { TOmniValueEnumeratorDataPackage.Create }

destructor TOmniValueEnumeratorDataPackage.Destroy;
begin
  FreeAndNil(vedpDataQueue);
  inherited;
end; { TOmniValueEnumeratorDataPackage.Destroy }

procedure TOmniValueEnumeratorDataPackage.Add(const value: TOmniValue);
var
  tmp: TOmniValue;
begin
//  {$IFDEF Debug} Assert(not vedpDataQueue.IsFull); {$ENDIF}
  tmp := value;
  if tmp.IsInterface then
    tmp.AsInterface._AddRef;
  vedpDataQueue.Enqueue(tmp);
  tmp.RawZero;
  vedpApproxCount.Increment;
//  {$IFDEF Debug} Assert(not vedpDataQueue.IsFull); {$ENDIF} // full queue corrupts data
end; { TOmniValueEnumeratorDataPackage.Add }

function TOmniValueEnumeratorDataPackage.GetNext(var value: TOmniValue): boolean;
var
  tmp: TOmniValue;
begin
  tmp.RawZero;
  Result := vedpDataQueue.TryDequeue(tmp);
  if not Result then
    Exit;
  value := tmp;
  if tmp.IsInterface then
    tmp.AsInterface._Release;
  vedpApproxCount.Decrement;
end; { TOmniValueEnumeratorDataPackage.GetNext }

function TOmniValueEnumeratorDataPackage.GetNext(var position: int64; var value:
  TOmniValue): boolean;
begin
  Result := false;
  // TODO 1 -oPrimoz Gabrijelcic : implement: TOmniValueEnumeratorDataPackage.GetNext
end; { TOmniValueEnumeratorDataPackage.GetNext }

class function TOmniValueEnumeratorDataPackage.GetPackageSizeLimit: integer;
begin
  Result := CMaxValueEnumeratorDataPackageSize;
end; { TOmniValueEnumeratorDataPackage.GetPackageSizeLimit }

function TOmniValueEnumeratorDataPackage.Prepare(dataCount: integer): integer;
begin
  // only called when the package is empty
  if dataCount <= CMaxValueEnumeratorDataPackageSize then
    Result := dataCount
  else
    Result := CMaxValueEnumeratorDataPackageSize;
  vedpApproxCount.Value := 0;
end; { TOmniValueEnumeratorDataPackage.Prepare }

function TOmniValueEnumeratorDataPackage.Split(package: TOmniDataPackage): boolean;
var
  intPackage: TOmniValueEnumeratorDataPackage absolute package;
  iValue    : integer;
  value     : TOmniValue;
begin
  {$IFDEF Debug} Assert(package is TOmniValueEnumeratorDataPackage); {$ENDIF}
  Result := false;
  for iValue := 1 to intPackage.Prepare(vedpApproxCount.Value div 2) do begin
    if not GetNext(value) then
      break; //for
    intPackage.Add(value);
    Result := true;
  end;
end; { TOmniValueEnumeratorDataPackage.Split }

{ TOmniValueEnumeratorProvider }

function TOmniValueEnumeratorProvider.Count: int64;
begin
  raise Exception.Create('IOmniValueEnumerable cannot be counted');
end; { TOmniValueEnumeratorProvider.Count }

constructor TOmniValueEnumeratorProvider.Create(enumerator: IOmniValueEnumerator);
begin
  inherited Create;
  vepEnumerator := enumerator;
end; { TOmniValueEnumeratorProvider.Create }

function TOmniValueEnumeratorProvider.CreateDataPackage: TOmniDataPackage;
begin
  Result := TOmniValueEnumeratorDataPackage.Create;
end; { TOmniValueEnumeratorProvider.CreateDataPackage }

function TOmniValueEnumeratorProvider.GetCapabilities: TOmniSourceProviderCapabilities;
begin
  Result := [spcDataLimit];
end; { TOmniValueEnumeratorProvider.GetCapabilities }

function TOmniValueEnumeratorProvider.GetPackage(dataCount: integer; package:
  TOmniDataPackage): boolean;
var
  iData     : integer;
  intPackage: TOmniValueEnumeratorDataPackage absolute package;
  timeout   : cardinal;
  value     : TOmniValue;
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TOmniValueEnumeratorProvider.GetPackage
  Assert(not StorePositions);

  Result := false;
  dataCount := intPackage.Prepare(dataCount);
  timeout := INFINITE;
  for iData := 1 to dataCount do begin
    if not vepEnumerator.TryTake(value, timeout) then
      break; //for
    intPackage.Add(value);
    timeout := 0;
    Result := true;
  end;
end; { TOmniValueEnumeratorProvider.GetPackage }

function TOmniValueEnumeratorProvider.GetPackageSizeLimit: integer;
begin
  Result := TOmniValueEnumeratorDataPackage.GetPackageSizeLimit;
end; { TOmniValueEnumeratorProvider.GetPackageSizeLimit }

{ TOmniEnumeratorProvider }

constructor TOmniEnumeratorProvider.Create(enumerator: IEnumerator);
begin
  inherited Create;
  epEnumeratorIntf := enumerator;
end; { TOmniEnumeratorProvider.Create }

constructor TOmniEnumeratorProvider.Create(enumerator: TOmniValueEnumerator);
begin
  inherited Create;
  epEnumeratorObj := enumerator;
end; { TOmniEnumeratorProvider.Create }

function TOmniEnumeratorProvider.Count: int64;
begin
  raise Exception.Create('IEnumerable cannot be counted');
end; { TOmniEnumeratorProvider.Count }

function TOmniEnumeratorProvider.CreateDataPackage: TOmniDataPackage;
begin
  Result := TOmniValueEnumeratorDataPackage.Create;
end; { TOmniEnumeratorProvider.CreateDataPackage }

function TOmniEnumeratorProvider.GetCapabilities: TOmniSourceProviderCapabilities;
begin
  Result := [spcDataLimit];
end; { TOmniEnumeratorProvider.GetCapabilities }

function TOmniEnumeratorProvider.GetPackage(dataCount: integer;
  package: TOmniDataPackage): boolean;
var
  iData     : integer;
  intPackage: TOmniValueEnumeratorDataPackage absolute package;
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TOmniEnumeratorProvider.GetPackage
  Assert(not StorePositions);

  Result := false;
  epEnumLock.Acquire;
  try
    dataCount := intPackage.Prepare(dataCount);
    if assigned(epEnumeratorObj) then begin
      for iData := 1 to dataCount do begin
        if not epEnumeratorObj.MoveNext then
          break; //for iData
        intPackage.Add(epEnumeratorObj.Current);
        Result := true;
      end;
    end
    else begin
      for iData := 1 to dataCount do begin
        if not epEnumeratorIntf.MoveNext then
          break; //for iData
        intPackage.Add(epEnumeratorIntf.Current);
        Result := true;
      end;
    end;
  finally epEnumLock.Release; end;
end; { TOmniEnumeratorProvider.GetPackage }

function TOmniEnumeratorProvider.GetPackageSizeLimit: integer;
begin
  Result := TOmniValueEnumeratorDataPackage.GetPackageSizeLimit;
end; { TOmniEnumeratorProvider.GetPackageSizeLimit }

{ TOmniLocalQueueImpl }

constructor TOmniLocalQueueImpl.Create(owner: TOmniBaseDataManager);
begin
  inherited Create;
  lqiDataManager_ref := owner;
  lqiDataPackage := lqiDataManager_ref.SourceProvider.CreateDataPackage;
  (lqiDataPackage as TOmniDataPackageBase).Queue := Self;
end; { TOmniLocalQueueImpl.Create }

destructor TOmniLocalQueueImpl.Destroy;
begin
  if assigned(lqiDataManager_ref) then
    lqiDataManager_ref.LocalQueueDestroyed(Self);
  FreeAndNil(lqiDataPackage); // keep this destruction order! (see Split for more info)
  inherited;
end; { TOmniLocalQueueImpl.Destroy }

function TOmniLocalQueueImpl.GetNext(var value: TOmniValue): boolean;
begin
  Result := lqiDataPackage.GetNext(value);
  if not Result then begin
    Result := lqiDataManager_ref.GetNext(lqiDataPackage);
    if Result then
      Result := lqiDataPackage.GetNext(value);
  end;
end; { TOmniLocalQueueImpl.GetNext }

function TOmniLocalQueueImpl.GetNext(var position: int64; var value: TOmniValue): boolean;
begin
  Result := lqiDataPackage.GetNext(position, value);
  if not Result then begin
    Result := lqiDataManager_ref.GetNext(lqiDataPackage);
    if Result then
      Result := lqiDataPackage.GetNext(position, value);
  end;
end; { TOmniLocalQueueImpl.GetNext }

function TOmniLocalQueueImpl.Split(package: TOmniDataPackage): boolean;
begin
  // this method is called when queue list is locked, therefore lqiDataPackage is
  // guaranteed to still be created
  {$IFDEF Debug} Assert(assigned(lqiDataPackage)); {$ENDIF}
  Result := lqiDataPackage.Split(package);
end; { TOmniLocalQueueImpl.Split }

{ TOmniOutputBufferImpl }

constructor TOmniOutputBufferImpl.Create(output: IOmniBlockingCollection);
begin
  inherited Create;
  obiOutput := output;
  obiBuffer := TOmniBlockingCollection.Create;
end; { TOmniOutputBufferImpl.Create }

destructor TOmniOutputBufferImpl.Destroy;
begin
  FreeAndNil(obiBuffer);
  inherited;
end; { TOmniOutputBufferImpl.Destroy }

procedure TOmniOutputBufferImpl.Submit(position: int64; const data: TOmniValue);
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TOmniOutputBufferImpl.Submit
end; { TOmniOutputBufferImpl.Submit }

{ TOmniBaseDataManager }

constructor TOmniBaseDataManager.Create(sourceProvider: TOmniSourceProvider; numWorkers:
  integer; options: TOmniDataManagerOptions);
begin
  inherited Create;
  dmSourceProvider_ref := (sourceProvider as TOmniSourceProviderBase);
  dmQueueList := TObjectList.Create;
  dmNumWorkers := numWorkers;
  dmOptions := options;
  dmSourceProvider_ref.StorePositions := (dmoPreserveOrder in dmOptions);
  dmNextPosition := 1;
  InitializePacketSizes;
end; { TOmniBaseDataManager.Create }

destructor TOmniBaseDataManager.Destroy;
begin
  dmOutputIntf := nil;
  FreeAndNil(dmQueueList);
  inherited;
end; { TOmniBaseDataManager.Destroy }

function TOmniBaseDataManager.AllocateOutputBuffer: TOmniOutputBuffer;
begin
  Assert(assigned(dmOutputIntf));
  Result := TOmniOutputBufferImpl.Create(dmOutputIntf);
end; { TOmniBaseDataManager.AllocateOutputBuffer }

procedure TOmniBaseDataManager.LocalQueueDestroyed(queue: TOmniLocalQueue);
begin
  dmQueueLock.Acquire;
  try
    dmQueueList.Extract(queue);
  finally dmQueueLock.Release; end;
end; { TOmniBaseDataManager.LocalQueueDestroyed }

function TOmniBaseDataManager.CreateLocalQueue: TOmniLocalQueue;
begin
  Result := TOmniLocalQueueImpl.Create(Self);
  dmQueueLock.Acquire;
  try
    dmQueueList.Add(Result);
  finally dmQueueLock.Release; end;
end; { TOmniBaseDataManager.CreateLocalQueue }

procedure TOmniBaseDataManager.Flush;
begin
  { TODO 1 : implement: Flush }
end; { TOmniBaseDataManager.Flush }

function TOmniBaseDataManager.GetDataCountForGeneration(generation: integer): integer;
begin
  if generation >= High(dmPacketSizes) then
    Result := dmPacketSizes[High(dmPacketSizes)]
  else
    Result := dmPacketSizes[generation];
end; { TOmniBaseDataManager.GetDataCountForGeneration }

function TOmniBaseDataManager.GetNext(package: TOmniDataPackage): boolean;
var
  intPackage: TOmniDataPackageBase absolute package;
begin
  Result := GetNextFromProvider(package, intPackage.Generation);
  // package starts at intPackage.Position
  // !!! get position too; store position for the current thread; set 'active' token in the buffer for that thread
  if not Result then
    Result := StealPackage(package);
  if Result then
    intPackage.NextGeneration;
end; { TOmniBaseDataManager.GetNext }

function TOmniBaseDataManager.GetSourceProvider: TOmniSourceProvider;
begin
  Result := dmSourceProvider_ref;
end; { TOmniBaseDataManager.GetSourceProvider }

procedure TOmniBaseDataManager.InitializePacketSizes;
var
  iGen        : integer;
  maxDataCount: integer;
begin
  if (spcCountable in sourceProvider.GetCapabilities) and
     (dmNumWorkers = 1) or (spcFast in sourceProvider.GetCapabilities) then
  begin
    SetLength(dmPacketSizes, 1);
    dmPacketSizes[0] := (SourceProvider.Count + dmNumWorkers - 1) div dmNumWorkers;
  end
  else begin
    SetLength(dmPacketSizes, 5);
    if spcCountable in sourceProvider.GetCapabilities then
      maxDataCount := sourceProvider.GetPackageSizeLimit
    else
      maxDataCount := High(integer);
    for iGen := 0 to 4 do begin
      dmPacketSizes[iGen] := Round(Exp(2 * iGen * Ln(3))); // 3^(2*iGen)
      if dmPacketSizes[iGen] > maxDataCount then begin
        SetLength(dmPacketSizes, iGen);
        break; //for iGen
      end;
    end;
  end;
end; { TOmniBaseDataManager.InitializePacketSizes }

procedure TOmniBaseDataManager.ReleaseOutputBuffer(buffer: TOmniOutputBuffer);
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TOmniBaseDataManager.ReleaseOutputBuffer
  // must put buffer 'away' so that it will be read from when data manager is flushed
  // (or even before)
end; { TOmniBaseDataManager.ReleaseOutputBuffer }

procedure TOmniBaseDataManager.SetOutput(const queue: IOmniBlockingCollection);
begin
  dmOutputIntf := queue;
end; { TOmniBaseDataManager.SetOutput }

function TOmniBaseDataManager.StealPackage(package: TOmniDataPackage): boolean;
var
  iQueue  : integer;
  queue   : TOmniLocalQueue;
  queueCnt: integer;
begin
  // try to steal package from other workers
  // TODO 1 -oPrimoz Gabrijelcic : Do not steal in dmoPreserveOrder mode
  Result := true;
  dmQueueLock.Acquire;
  try
    queueCnt := dmQueueList.Count;
    for iQueue := dmStealIdx to dmStealIdx + queueCnt - 1 do begin
      queue := TOmniLocalQueue(dmQueueList[iQueue mod queueCnt]);
      if (TOmniDataPackageBase(package).Queue <> queue) and queue.Split(package) then begin
        dmStealIdx := (iQueue + 1) mod queueCnt;
        Exit;
      end;
    end;
  finally dmQueueLock.Release; end;
  Result := false;
end; { TOmniBaseDataManager.StealPackage }

{ TOmniCountableDataManager }

function TOmniCountableDataManager.GetNextFromProvider(package: TOmniDataPackage;
  generation: integer): boolean;
begin
  Result := SourceProvider.GetPackage(GetDataCountForGeneration(generation), package);
end; { TOmniCountableDataManager.GetNextFromProvider }

{ TOmniHeuristicDataManager }

constructor TOmniHeuristicDataManager.Create(sourceProvider: TOmniSourceProvider;
  numWorkers: integer; options: TOmniDataManagerOptions);
begin
  inherited Create(sourceProvider, numWorkers, options);
  hdmEstimatedPackageSize.Value := GetDataCountForGeneration(High(integer)); // hope for the best
end; { TOmniHeuristicDataManager.Create }

function TOmniHeuristicDataManager.GetNextFromProvider(package: TOmniDataPackage;
  generation: integer): boolean;
const
  CDataLimit = Trunc(High(integer) / CFetchTimeout_ms);
var
  dataPerMs: cardinal;
  dataSize : integer;
  time     : int64;
begin
  // the goal is to fetch as much (but not exceeding <fetch_limit>) data as possible in
  // <fetch_timeout> milliseconds; highest amount of data is limited by the
  // GetDataCountForGeneration method.
  dataSize := GetDataCountForGeneration(generation);
  if dataSize > hdmEstimatedPackageSize.Value then
    dataSize := hdmEstimatedPackageSize.Value;
  time := DSiTimeGetTime64;
  Result := SourceProvider.GetPackage(dataSize, package);
  time := DSiTimeGetTime64 - time;
  if Result then begin
    if time = 0 then
      dataPerMs := CDataLimit
    else begin
      dataPerMs := Round(dataSize / time);
      if dataPerMs >= CDataLimit then
        dataPerMs := CDataLimit;
    end;
    // average over last four fetches for dynamic adaptation
    hdmEstimatedPackageSize.Value := Round((hdmEstimatedPackageSize.Value / 4 * 3) + (dataPerMs / 4) * CFetchTimeout_ms);
  end;
end; { TOmniHeuristicDataManager.GetNextFromProvider }

(*
{ TOmniLogger }

constructor TOmniLogger.Create;
begin
  inherited Create;
  eventList := TOmniBaseQueue.Create;
end; { TOmniLogger.Create }

destructor TOmniLogger.Destroy;
begin
  FreeAndNil(eventList);
  inherited;
end; { TOmniLogger.Destroy }

procedure TOmniLogger.Clear;
var
  tmp: TOmniValue;
begin
  while eventList.TryDequeue(tmp) do begin
    tmp := '';
    ;
  end;
end; { TOmniLogger.Clear }

procedure TOmniLogger.GetEventList(sl: TStringList);
var
  tmp: TOmniValue;
begin
  while eventList.TryDequeue(tmp) do
    sl.Add(tmp);
end; { TOmniLogger.GetEventList }

procedure TOmniLogger.Log(const msg: string; const params: array of const);
begin
  Log(Format(msg, params));
end; { TOmniLogger.Log }

procedure TOmniLogger.Log(const msg: string);
begin
  eventList.Enqueue(Format('[%d] %s', [GetCurrentThreadID, msg]));
end; { TOmniLogger.Log }
*)

initialization
//  GLogger := TOmniLogger.Create;
finalization
//  FreeAndNil(GLogger);
end.
