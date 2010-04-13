unit OtlDataManager;

{$IF CompilerVersion >= 21}
  {$DEFINE OTL_ERTTI}
{$IFEND}

interface

uses
  GpStuff,
  OtlCommon;

type
  ///<summary>Source provider capabilities.</summary>
  TOmniSourceProviderCapability = (spcCountable);
  TOmniSourceProviderCapabilities = set of TOmniSourceProviderCapability;

  ///<summary>Wrapper around a (type specific) data package. Split method will be
  ///    called from the context of a non-owning thread!</summary>
  TOmniDataPackage = class abstract
  public
    function  GetNext(var value: TOmniValue): boolean; virtual; abstract;
    function  HasData: boolean; virtual; abstract;
    function  Split(package: TOmniDataPackage): boolean; virtual; abstract;
  end; { TOmniDataPackage }

  ///<summary>A data package queue between a single worker and shared data manager.</summary>
  TOmniLocalQueue = class abstract
  public
    function  GetNext(var value: TOmniValue): boolean; virtual; abstract;
  end; { TOmniLocalQueue }

  ///<summary>Wrapper around the data source. All methods can and will be called from
  ///    multiple threads at the same time!</summary>
  TOmniSourceProvider = class abstract
  public
    function  Count: int64; virtual; abstract;
    function  CreateDataPackage: TOmniDataPackage; virtual; abstract;
    function  GetCapabilities: TOmniSourceProviderCapabilities; virtual; abstract;
    function  GetPackage(dataCount: integer; var package: TOmniDataPackage): boolean; virtual; abstract;
  end; { TOmniSourceProvider }

  ///<summary>Data manager. CreateLocalQueue method will be called from the context of
  ///    a non-owning thread!</summary>
  TOmniDataManager = class abstract
  public
    function  CreateLocalQueue: TOmniLocalQueue; virtual; abstract;
    function  GetNext(package: TOmniDataPackage): boolean; virtual; abstract;
  end; { TOmniDataManager }

function  CreateSourceProvider(low, high: integer; step: integer = 1): TOmniSourceProvider; overload;
function  CreateSourceProvider(enumerable: IOmniValueEnumerable): TOmniSourceProvider; overload;
function  CreateSourceProvider(enumerable: IEnumerable): TOmniSourceProvider; overload;

{$IFDEF OTL_ERTTI}
function  CreateSourceProvider(enumerable: TObject): TOmniSourceProvider; overload;
{$ENDIF OTL_ERTTI}

function  CreateDataManager(sourceProvider: TOmniSourceProvider): TOmniDataManager;

implementation

uses
  SysUtils,
  Classes,
  Contnrs,
  DSiWin32,
  OtlSync;

type
  ///<summary>Integer range data package.</summary>
  TOmniIntegerDataPackage = class(TOmniDataPackage)
  strict private
    idpHigh: int64;
    idpLow : TGp8AlignedInt64;
    idpStep: integer;
  public
    function  GetNext(var value: TOmniValue): boolean; override;
    function  HasData: boolean; override;
    procedure Initialize(low, high, step: integer);
    function  Split(package: TOmniDataPackage): boolean; override;
  end; { TOmniIntegerDataPackage }

  ///<summary>Integer range source provider.</summary>
  TOmniIntegerRangeProvider = class(TOmniSourceProvider)
  strict private
    irpCount: TGp4AlignedInt;
    irpHigh : integer;
    irpLock : TOmniCS;
    irpLow  : integer;
    irpStep : integer;
  strict protected
    procedure RecalcCount; inline;
  public
    constructor Create(low, high, step: integer);
    function  Count: int64; override;
    function  CreateDataPackage: TOmniDataPackage; override;
    function  GetCapabilities: TOmniSourceProviderCapabilities; override;
    function  GetPackage(dataCount: integer; var package: TOmniDataPackage): boolean; override;
  end; { TOmniIntegerRangeProvider }

  TOmniValueEnumerableProvider = class(TOmniSourceProvider)
  end; { TOmniValueEnumerableProvider }

  TOmniEnumerableProvider = class(TOmniSourceProvider)
  end; { TOmniEnumerableProvider }

  TOmniDelphiEnumeratorProvider = class(TOmniSourceProvider)
  end; { TOmniDelphiEnumeratorProvider }

  TOmniBaseDataManager = class;

  ///<summary>Local queue implementation.</summary>
  TOmniLocalQueueImpl = class(TOmniLocalQueue)
  strict private
    lqiDataManager_ref: TOmniBaseDataManager;
    lqiDataPackage    : TOmniDataPackage;
  public
    constructor Create(owner: TOmniBaseDataManager);
    destructor  Destroy; override;
    function  GetNext(var value: TOmniValue): boolean; override;
  end; { TOmniLocalQueueImpl }

  ///<summary>Base data manager class.</summary>
  TOmniBaseDataManager = class abstract (TOmniDataManager)
  strict private
    dmQueueList         : TObjectList;
    dmQueueLock         : TOmniCS;
    dmSourceProvider_ref: TOmniSourceProvider;
  public
    constructor Create(sourceProvider: TOmniSourceProvider);
    destructor  Destroy; override;
    function  CreateLocalQueue: TOmniLocalQueue; override;
    procedure LocalQueueDestroyed(queue: TOmniLocalQueue);
    property SourceProvider: TOmniSourceProvider read dmSourceProvider_ref;
  end; { TOmniBaseDataManager }

  ///<summary>Data manager for countable data.</summary>
  TOmniCountableDataManager = class(TOmniBaseDataManager)
  public
    function  GetNext(package: TOmniDataPackage): boolean; override;
  end; { TOmniCountableDataManager }

  ///<summary>Data manager for unbounded data.</summary>
  TOmniHeuristicDataManager = class(TOmniBaseDataManager)
  public
    function  GetNext(package: TOmniDataPackage): boolean; override;
  end; { TOmniHeuristicDataManager }

{ exports }

function CreateSourceProvider(low, high, step: integer): TOmniSourceProvider;
begin
  Result := TOmniIntegerRangeProvider.Create(low, high, step);
end; { CreateSourceProvider }

function CreateSourceProvider(enumerable: IOmniValueEnumerable): TOmniSourceProvider;
begin
//  Result := TOmniValueEnumerableProvider.Create(enumerable);
  Result := nil;
end; { CreateSourceProvider }

function CreateSourceProvider(enumerable: IEnumerable): TOmniSourceProvider; overload;
begin
//  Result := TOmniEnumerableProvider.Create(enumerable);
  Result := nil;
end; { CreateSourceProvider }

{$IFDEF OTL_ERTTI}
function CreateSourceProvider(enumerable: TObject): TOmniSourceProvider;
begin
//  Result := TOmniDelphiEnumeratorProvider.Create(enumerable);
  Result := nil;
end; { CreateSourceProvider }
{$ENDIF OTL_ERTTI}

function CreateDataManager(sourceProvider: TOmniSourceProvider): TOmniDataManager;
begin
  if spcCountable in sourceProvider.GetCapabilities then
    Result := TOmniCountableDataManager.Create(sourceProvider)
  else
    Result := TOmniHeuristicDataManager.Create(sourceProvider);
end; { CreateDataManager }

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

function TOmniIntegerDataPackage.HasData: boolean;
begin
  Result := idpStep <> 0;
end; { TOmniIntegerDataPackage.HasData }

procedure TOmniIntegerDataPackage.Initialize(low, high, step: integer);
begin
  {$IFDEF Debug}Assert(step <> 0);{$ENDIF}
  idpLow.Value := low;
  idpHigh := high;
  idpStep := step;
end; { TOmniIntegerDataPackage.Initialize }

function TOmniIntegerDataPackage.Split(package: TOmniDataPackage): boolean;
var
  intPackage: TOmniIntegerDataPackage absolute package;
  value     : TOmniValue;
begin
  // TODO 3 -oPrimoz Gabrijelcic : Can benefit from overloaded GetNext returning integer.
  {$IFDEF Debug}Assert(package is TOmniIntegerDataPackage);{$ENDIF}
  Result := GetNext(value);
  if Result then
    intPackage.Initialize(value, value, 1);
end; { TOmniIntegerDataPackage.Split }

{ TOmniIntegerRangeProvider }

constructor TOmniIntegerRangeProvider.Create(low, high, step: integer);
begin
  inherited Create;
  {$IFDEF Debug}Assert(step <> 0);{$ENDIF}
  irpLow := low;
  irpHigh := high;
  irpStep := step;
  RecalcCount;
end; { TOmniIntegerRangeProvider.Create }

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
  Result := [spcCountable];
end; { TOmniIntegerRangeProvider.GetCapabilities }

function TOmniIntegerRangeProvider.GetPackage(dataCount: integer;
  var package: TOmniDataPackage): boolean;
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
      intPackage.Initialize(irpLow, high, irpStep);
      irpLow := high + irpStep;
      RecalcCount;
    finally irpLock.Release; end;
    Result := true;
  end;
end; { TOmniIntegerRangeProvider.GetPackage }

procedure TOmniIntegerRangeProvider.RecalcCount;
begin
  if irpStep > 0 then
    irpCount.Value := (irpHigh - irpLow + irpStep) div irpStep
  else
    irpCount.Value := (irpLow - irpHigh - irpStep) div (-irpStep);
end; { TOmniIntegerRangeProvider.RecalcCount }

{ TOmniLocalQueueImpl }

constructor TOmniLocalQueueImpl.Create(owner: TOmniBaseDataManager);
begin
  inherited Create;
  lqiDataManager_ref := owner;
  lqiDataPackage := lqiDataManager_ref.SourceProvider.CreateDataPackage;
end; { TOmniLocalQueueImpl.Create }

destructor TOmniLocalQueueImpl.Destroy;
begin
  FreeAndNil(lqiDataPackage);
  if assigned(lqiDataManager_ref) then
    lqiDataManager_ref.LocalQueueDestroyed(Self);
  inherited;
end; { TOmniLocalQueueImpl.Destroy }

function TOmniLocalQueueImpl.GetNext(var value: TOmniValue): boolean;
begin
  Result := lqiDataPackage.GetNext(value);
  if not Result then
    Result := lqiDataManager_ref.GetNext(lqiDataPackage);
end; { TOmniLocalQueueImpl.GetNext }

{ TOmniBaseDataManager }

constructor TOmniBaseDataManager.Create(sourceProvider: TOmniSourceProvider);
begin
  inherited Create;
  dmSourceProvider_ref := sourceProvider;
  dmQueueList := TObjectList.Create;
end; { TOmniBaseDataManager.Create }

destructor TOmniBaseDataManager.Destroy;
begin
  FreeAndNil(dmQueueList);
  inherited;
end; { TOmniBaseDataManager.Destroy }

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

{ TOmniCountableDataManager }

function TOmniCountableDataManager.GetNext(package: TOmniDataPackage): boolean;
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TOmniCountableDataManager.GetNext
  Result := false;
end; { TOmniCountableDataManager.GetNext }

{ TOmniHeuristicDataManager }

function TOmniHeuristicDataManager.GetNext(package: TOmniDataPackage): boolean;
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TOmniHeuristicDataManager.GetNext
  Result := false;
end; { TOmniHeuristicDataManager.GetNext }

end.
