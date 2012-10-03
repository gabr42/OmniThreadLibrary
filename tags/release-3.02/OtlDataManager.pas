///<summary>Data consumer with parallelization support (backend for the OtlParallel unit).</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2010, Primoz Gabrijelcic
///All rights reserved.
///
///Redistribution and use in source and binary forms, with or without modification,
///are permitted provided that the following conditions are met:
///- Redistributions of source code must retain the above copyright notice, this
///  list of conditions and the following disclaimer.
///- Redistributions in binary form must reproduce the above copyright notice,
///  this list of conditions and the following disclaimer in the documentation
///  and/or other materials provided with the distribution.
///- The name of the Primoz Gabrijelcic may not be used to endorse or promote
///  products derived from this software without specific prior written permission.
///
///THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
///ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
///WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
///DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
///ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
///(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
///LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
///ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
///(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
///SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
///</license>
///<remarks><para>
///   Home              : http://otl.17slon.com
///   Support           : http://otl.17slon.com/forum/
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///     Web             : http://gp.17slon.com
///   Contributors      : GJ, Lee_Nover
///
///   Creation date     : 2010-04-13
///   Last modification : 2010-12-10
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2010-12-10
//        - Created.
///</para></remarks>

unit OtlDataManager;

{$I OtlOptions.inc}

interface

uses
  GpStuff,
  OtlCommon,
  OtlContainers,
  OtlCollections;

type
  ///<summary>Source provider capabilities.</summary>
  TOmniSourceProviderCapability = (
    spcCountable,  // source provider that knows how much data it holds
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

  ///<summary>Output data buffer.</summary>
  TOmniOutputBuffer = class abstract
  public
    procedure Submit(position: int64; const data: TOmniValue); virtual; abstract;
  end; { TOmniOutputBuffer }

  ///<summary>A data package queue between a single worker and shared data manager.</summary>
  TOmniLocalQueue = class abstract
  public
    procedure AssociateBuffer(buffer: TOmniOutputBuffer); virtual; abstract;
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

  TOmniDataManagerOption = (dmoPreserveOrder);
  TOmniDataManagerOptions = set of TOmniDataManagerOption;

  ///<summary>Data manager. All methods can and will be called from multiple threads
  ///    at the same time!</summary>
  TOmniDataManager = class abstract
  public
    function  CreateLocalQueue: TOmniLocalQueue; virtual; abstract;
    function  AllocateOutputBuffer: TOmniOutputBuffer; virtual; abstract;
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

implementation

uses
  Windows,
  SysUtils,
  Contnrs,
  Classes,
  {$IFDEF OTL_HasSystemTypes}
  System.Types,
  {$ENDIF}
  DSiWin32,
  GpLists,
  OtlSync;

const
  CNumBuffersInSet = 2;

type
  TOmniPositionRange = record
    First: int64;
    Last : int64;
    constructor Create(aFirst, aLast: int64);
    class function Empty: TOmniPositionRange; static;
  end; { TOmniPositionRange }

  ///<summary>Base class for all data package classes.</summary>
  TOmniDataPackageBase = class abstract(TOmniDataPackage)
  private
    dpbGeneration: integer;
    dpbRange     : TOmniPositionRange;
    dpbQueue     : TOmniLocalQueue;
  public
    procedure NextGeneration; inline;
    property Generation: integer read dpbGeneration;
    property Range: TOmniPositionRange read dpbRange write dpbRange;
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
    procedure Initialize(low, high, step: integer; newRange: TOmniPositionRange);
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

  ///<summary>Output buffer implementation.</summary>
  TOmniOutputBufferImpl = class(TOmniOutputBuffer)
  strict private
    obiBuffer         : TOmniBlockingCollection;
    obiDataManager_ref: TOmniBaseDataManager;
    obiEmptyHandle    : THandle;
    obiFull           : boolean;
    obiHasData        : boolean;
    obiNextPosition   : int64;
    obiOutput         : IOmniBlockingCollection;
    obiRange          : TOmniPositionRange;
  strict protected
    procedure SetRange(range: TOmniPositionRange);
  public
    constructor Create(owner: TOmniBaseDataManager; output: IOmniBlockingCollection);
    destructor  Destroy; override;
    procedure CopyToOutput;
    procedure MarkFull;
    procedure Submit(position: int64; const data: TOmniValue); override;
    property EmptyHandle: THandle read obiEmptyHandle;
    property IsFull: boolean read obiFull;
    property Range: TOmniPositionRange read obiRange write SetRange;
  end; { TOmniOutputBufferImpl }

  TOmniOutputBufferSet = class(TOmniOutputBuffer)
  strict private
    obsActiveBuffer_ref: TOmniOutputBufferImpl;
    obsActiveIndex     : integer;
    obsBuffers         : array [1..CNumBuffersInSet] of TOmniOutputBufferImpl;
    obsWaitHandles     : array [1..CNumBuffersInSet] of THandle;
  public
    constructor Create(owner: TOmniBaseDataManager; output: IOmniBlockingCollection);
    destructor  Destroy; override;
    procedure ActivateBuffer;
    procedure Submit(position: int64; const data: TOmniValue); override;
    property ActiveBuffer: TOmniOutputBufferImpl read obsActiveBuffer_ref;
  end; { TOmniOutputBufferSet }

  ///<summary>Local queue implementation.</summary>
  TOmniLocalQueueImpl = class(TOmniLocalQueue)
  strict private
    lqiBufferSet      : TOmniOutputBufferSet;
    lqiDataManager_ref: TOmniBaseDataManager;
    lqiDataPackage    : TOmniDataPackageBase;
  public
    constructor Create(owner: TOmniBaseDataManager);
    destructor  Destroy; override;
    procedure AssociateBuffer(buffer: TOmniOutputBuffer); override;
    function  GetNext(var value: TOmniValue): boolean; overload; override;
    function  GetNext(var position: int64; var value: TOmniValue): boolean; overload; override;
    function  Split(package: TOmniDataPackage): boolean; override;
  end; { TOmniLocalQueueImpl }

  ///<summary>Base data manager class.</summary>
  TOmniBaseDataManager = class abstract (TOmniDataManager)
  strict private
    const CMaxPreserveOrderPackageSize = 1024; // pretty arbitrary, should do some performance tests
  strict private
    dmBufferRangeList   : TGpInt64ObjectList;
    dmBufferRangeLock   : TOmniCS;
    dmNextPosition      : int64;
    dmNumWorkers        : integer;
    dmOptions           : TOmniDataManagerOptions;
    dmOutputIntf        : IOmniBlockingCollection;
    dmPacketSizes       : array of integer;
    dmQueueList         : TObjectList;
    dmQueueLock         : TOmniCS;
    dmSourceProvider_ref: TOmniSourceProviderBase;
    dmStealIdx          : integer;
    dmUnusedBuffers     : TObjectList;
    dmUnusedBuffersLock : TOmniCS;
  strict protected
    function  GetBufferList(idxBuffer: integer): TOmniOutputBufferImpl; inline;
    function  GetSourceProvider: TOmniSourceProvider;
    procedure InitializePacketSizes;
    property BufferList[idxBuffer: integer]: TOmniOutputBufferImpl read GetBufferList;
  public
    constructor Create(sourceProvider: TOmniSourceProvider; numWorkers: integer;
      options: TOmniDataManagerOptions);
    destructor  Destroy; override;
    function  AllocateOutputBuffer: TOmniOutputBuffer; override;
    function  CreateLocalQueue: TOmniLocalQueue; override;
    function  GetDataCountForGeneration(generation: integer): integer;
    function  GetNext(package: TOmniDataPackage): boolean; override;
    function  GetNextFromProvider(package: TOmniDataPackage;
      generation: integer): boolean; virtual; abstract;
    procedure LocalQueueDestroyed(queue: TOmniLocalQueue);
    procedure NotifyBufferFull(buffer: TOmniOutputBufferImpl);
    procedure NotifyBufferRangeChanged(buffer: TOmniOutputBufferImpl);
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

{ TOmniPositionRange }

constructor TOmniPositionRange.Create(aFirst, aLast: int64);
begin
  First := aFirst;
  Last := aLast;
end; { TOmniPositionRange.Create }

class function TOmniPositionRange.Empty: TOmniPositionRange;
begin
  Result := TOmniPositionRange.Create(0, 0);
end; { TOmniPositionRange.Empty }

{ TOmniDataPackageBase }

procedure TOmniDataPackageBase.NextGeneration;
begin
  dpbGeneration := Abs((dpbGeneration SHL 1) + 3) SHR 1;
end; { TOmniDataPackageBase.NextGeneration }

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

procedure TOmniIntegerDataPackage.Initialize(low, high, step: integer;
  newRange: TOmniPositionRange);
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
  idpHighSign := idpHigh  * idpSign;
  Range := newRange;
  idpPosition := Range.First;
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
        intPackage.Initialize(low, high, idpStep, TOmniPositionRange.Empty);
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
      intPackage.Initialize(irpLow, high, irpStep, TOmniPositionRange.Create(irpPosition, irpPosition + dataCount - 1));
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

procedure TOmniLocalQueueImpl.AssociateBuffer(buffer: TOmniOutputBuffer);
begin
  lqiBufferSet := buffer as TOmniOutputBufferSet;
end; { TOmniLocalQueueImpl.AssociateBuffer }

constructor TOmniLocalQueueImpl.Create(owner: TOmniBaseDataManager);
begin
  inherited Create;
  lqiDataManager_ref := owner;
  lqiDataPackage := lqiDataManager_ref.SourceProvider.CreateDataPackage as TOmniDataPackageBase;
  lqiDataPackage.Queue := Self;
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
    {$IFDEF Debug}Assert(not assigned(lqiBufferSet));{$ENDIF Debug}
    Result := lqiDataManager_ref.GetNext(lqiDataPackage);
    if Result then
      Result := lqiDataPackage.GetNext(value);
  end;
end; { TOmniLocalQueueImpl.GetNext }

function TOmniLocalQueueImpl.GetNext(var position: int64; var value: TOmniValue): boolean;
begin
  Result := lqiDataPackage.GetNext(position, value);
  if not Result then begin
    {$IFDEF Debug}Assert(assigned(lqiBufferSet));{$ENDIF Debug}
    lqiBufferSet.ActiveBuffer.MarkFull;
    lqiBufferSet.ActivateBuffer; // this will block if alternate buffer is also full
    Result := lqiDataManager_ref.GetNext(lqiDataPackage);
    if Result then begin
      Result := lqiDataPackage.GetNext(position, value);
      if Result then
        lqiBufferSet.ActiveBuffer.Range := lqiDataPackage.Range;
    end;
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

constructor TOmniOutputBufferImpl.Create(owner: TOmniBaseDataManager;
  output: IOmniBlockingCollection);
begin
  inherited Create;
  obiDataManager_ref := owner;
  obiOutput := output;
  obiBuffer := TOmniBlockingCollection.Create;
  obiEmptyHandle := CreateEvent(nil, false, true, nil);
end; { TOmniOutputBufferImpl.Create }

destructor TOmniOutputBufferImpl.Destroy;
begin
  DSiCloseHandleAndNull(obiEmptyHandle);
  FreeAndNil(obiBuffer);
  inherited;
end; { TOmniOutputBufferImpl.Destroy }

procedure TOmniOutputBufferImpl.CopyToOutput;
var
  value: TOmniValue;
begin
  while obiBuffer.TryTake(value) do
    obiOutput.Add(value);
  SetEvent(obiEmptyHandle);
  obiFull := false;
end; { TOmniOutputBufferImpl.CopyToOutput }

procedure TOmniOutputBufferImpl.MarkFull;
begin
  obiFull := true;
  if obiHasData then
    obiDataManager_ref.NotifyBufferFull(Self);
  obiHasData := false;
end; { TOmniOutputBufferImpl.MarkFull }

procedure TOmniOutputBufferImpl.SetRange(range: TOmniPositionRange);
begin
  obiRange := range;
  obiDataManager_ref.NotifyBufferRangeChanged(Self);
  obiNextPosition := obiRange.First;
  obiHasData := true;
end; { TOmniOutputBufferImpl.SetRange }

procedure TOmniOutputBufferImpl.Submit(position: int64; const data: TOmniValue);
begin
  {$IFDEF DEBUG}Assert(position >= obiNextPosition, Format('%d < %d', [position, obiNextPosition])); obiNextPosition := position + 1;{$ENDIF}
  obiBuffer.Add(data);
end; { TOmniOutputBufferImpl.Submit }

{ TOmniOutputBufferSet }

constructor TOmniOutputBufferSet.Create(owner: TOmniBaseDataManager;
  output: IOmniBlockingCollection);
var
  iBuffer: integer;
begin
  for iBuffer := 1 to CNumBuffersInSet do begin
    obsBuffers[iBuffer] := TOmniOutputBufferImpl.Create(owner, output);
    obsWaitHandles[iBuffer] := obsBuffers[iBuffer].EmptyHandle;
  end;
  ActivateBuffer;
end; { TOmniOutputBufferSet.Create }

destructor TOmniOutputBufferSet.Destroy;
var
  iBuffer: integer;
begin
  for iBuffer := 1 to CNumBuffersInSet do
    obsBuffers[iBuffer].Free;
  inherited;
end; { TOmniOutputBufferSet.Destroy }

procedure TOmniOutputBufferSet.ActivateBuffer;
var
  awaited: cardinal;
begin
  awaited := WaitForMultipleObjects(CNumBuffersInSet, @obsWaitHandles, false, INFINITE);
  Assert({(awaited >= WAIT_OBJECT_0) and } (awaited < (WAIT_OBJECT_0 + CNumBuffersInSet)));
  obsActiveIndex := awaited - WAIT_OBJECT_0 + 1;
  obsActiveBuffer_ref := obsBuffers[obsActiveIndex];
end; { TOmniOutputBufferSet.ActivateBuffer }

procedure TOmniOutputBufferSet.Submit(position: int64; const data: TOmniValue);
begin
  obsActiveBuffer_ref.Submit(position, data);
end; { TOmniOutputBufferSet.Submit }

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
  if dmoPreserveOrder in dmOptions then begin
    dmBufferRangeList := TGpInt64ObjectList.Create(false);
    dmBufferRangeList.Sorted := true;
    dmBufferRangeList.Duplicates := dupError;
    dmUnusedBuffers := TObjectList.Create;
  end;
  InitializePacketSizes;
end; { TOmniBaseDataManager.Create }

destructor TOmniBaseDataManager.Destroy;
begin
  dmOutputIntf := nil;
  FreeAndNil(dmQueueList);
  FreeAndNil(dmBufferRangeList);
  FreeAndNil(dmUnusedBuffers);
  inherited;
end; { TOmniBaseDataManager.Destroy }

function TOmniBaseDataManager.AllocateOutputBuffer: TOmniOutputBuffer;
begin
  Assert(assigned(dmOutputIntf));
  Result := TOmniOutputBufferSet.Create(Self, dmOutputIntf);
end; { TOmniBaseDataManager.AllocateOutputBuffer }

procedure TOmniBaseDataManager.LocalQueueDestroyed(queue: TOmniLocalQueue);
begin
  dmQueueLock.Acquire;
  try
    dmQueueList.Extract(queue);
  finally dmQueueLock.Release; end;
end; { TOmniBaseDataManager.LocalQueueDestroyed }

procedure TOmniBaseDataManager.NotifyBufferFull(buffer: TOmniOutputBufferImpl);
begin
  // Remove buffer from the list. Check if next buffer is waiting in the list.
  // Copy buffer if it is full and repeat the process.
  dmBufferRangeLock.Acquire;
  try
    while (dmBufferRangeList.Count > 0) and
          (BufferList[0].Range.First = dmNextPosition) and
          BufferList[0].IsFull do
    begin
      buffer := TOmniOutputBufferImpl(dmBufferRangeList.ExtractObject(0));
      dmNextPosition := buffer.Range.Last + 1;
      buffer.CopyToOutput; // this will put the 'buffer' back into 'empty' state so from this point onwards 'buffer' must not be used!
    end;
  finally dmBufferRangeLock.Release; end;
end; { TOmniBaseDataManager.NotifyBufferFull }

procedure TOmniBaseDataManager.NotifyBufferRangeChanged(buffer: TOmniOutputBufferImpl);
begin
  dmBufferRangeLock.Acquire;
  try
    dmBufferRangeList.AddObject(buffer.Range.First, buffer);
  finally dmBufferRangeLock.Release; end;
end; { TOmniBaseDataManager.NotifyBufferRangeChanged }

function TOmniBaseDataManager.CreateLocalQueue: TOmniLocalQueue;
begin
  Result := TOmniLocalQueueImpl.Create(Self);
  dmQueueLock.Acquire;
  try
    dmQueueList.Add(Result);
  finally dmQueueLock.Release; end;
end; { TOmniBaseDataManager.CreateLocalQueue }

function TOmniBaseDataManager.GetBufferList(idxBuffer: integer): TOmniOutputBufferImpl;
begin
  Result := TOmniOutputBufferImpl(dmBufferRangeList.Objects[idxBuffer]);
end; { TOmniBaseDataManager.GetBufferList }

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
     ( (dmNumWorkers = 1) or
       ( (spcFast in sourceProvider.GetCapabilities) and
         (not (dmoPreserveOrder in dmOptions)))) then
  begin
    SetLength(dmPacketSizes, 1);
    dmPacketSizes[0] := (SourceProvider.Count + dmNumWorkers - 1) div dmNumWorkers;
  end
  else begin
    SetLength(dmPacketSizes, 5);
    if not (spcFast in sourceProvider.GetCapabilities) then
      maxDataCount := sourceProvider.GetPackageSizeLimit
    else if dmoPreserveOrder in dmOptions then
      maxDataCount := CMaxPreserveOrderPackageSize
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
  (buffer as TOmniOutputBufferSet).ActiveBuffer.MarkFull;
  dmUnusedBuffersLock.Acquire;
  try
    dmUnusedBuffers.Add(buffer);
  finally dmUnusedBuffersLock.Release; end;
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
  if not (dmoPreserveOrder in dmOptions) then begin
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
  end;
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

end.
