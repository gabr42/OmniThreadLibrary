unit OtlPlatform.HeavyPool;
{$I OtlOptions.inc}

interface
uses OtlParallel.Platform.Interfaces, System.SysUtils, Generics.Collections,
     OtlSync.Platform.Interfaced, System.Classes,
     OtlSync.Platform.Atomic;

//Heavy Pool
//===============
//Overview
//A heavy pool is a pool of re-usable heavy weight objects. The pool can be constrained by:
//•	A maximum number of created objects (available or in-use)
//•	Available (that is to say, not in current use) pooled objects can have a maximum age.
//   If they exceed this age, then are destroyed.
//•	A minimum number of available objects. When less, a background house keeping pool will slowly grow the pool.
//The user supplies functions to create/destroy the objects, or take action on
//   the event of Acquire from/ release back to the pool. Removing too-old pool
//   objects, or growing the pool to the specified minimum are house-keeping
//   jobs performed by a task on a periodic basis.

type
  TAquireResult = (aRecycle, aNew, aOverPop, aNoGenerator);

  THeavyPoolEx<T> = class( TInterfacedObject, IHeavyPoolEx<T>)
  private type
    RBasket = record
      FObject: T;
      FMaturity: TDateTime;
    end;
  private type
    TBasketList = class( TList<RBasket>)
    end;

  private
    FDatum: pointer;
    FMaxPop: cardinal;
    FMinReserve: cardinal;
    FMaxAge: TDateTime;
    FFreePool: TBasketList;
    FAcquired: TList<T>;
    FGate: ILock;
    FHouseKeeperThread: TThread;
    FWakeHouseKeeper: IEvent;
    FDefaultFlavour: IResourceFlavour;
    [Volatile] FIsShutdown: TVolatileInt32;
    FVanillaGenFunc: TGenerate<T>;
    FGenFunc       : TGenerateEx<T>;
    FRelFunc       : TDestroy<T>;
    FTaste         : TTaste<T>;

    /// <summary>Number of times the generate function has been called to create a new object instance.</summary>
    FGenerateCount: uint64;

    /// <summary>Number of times the release function has been called to permanently dispose of an object instance.</summary>
    FDestroyCount: uint64;

    /// <summary>Number of times a client has succesfuly acquired an object from the heavy pool.</summary>
    FAcquireCount: uint64;

    /// <summary>Number of times a client has returned a previously acquired object back to the heavy pool.</summary>
    FReleaseCount: uint64;

    function  GetMaxAge: double;
    procedure SetMaxAge( const Value: double);
    function  GetMaxPop: cardinal;
    procedure SetMaxPop( const Value: cardinal);
    function  GetMinReserve: cardinal;
    procedure SetMinReserve( const Value: cardinal);
    function  GetDatum: pointer;
    procedure ShutDown;
    function  GetStats: RHeavyPoolStats;
    function  GetDefaultFlavour: IResourceFlavour;
    procedure SetDefaultFlavour( const Value: IResourceFlavour);
    function  Acquire( const MatchingFlavour: IResourceFlavour): T;
    function  AcquireVanilla: T;
    procedure Release( const Retiree: T);
    procedure ReleaseNoRecycle( const Retiree: T);
    procedure PopAnOldOne;
    function  FreeItemMatches( FreePoolIdx: integer; const MatchingFlavour: IResourceFlavour): boolean;
    function  NextToMature( const MatchingFlavour: IResourceFlavour): integer;
    function  NextToMatureAny: integer;
    procedure HouseKeepProper;
    procedure Touch( var Item: RBasket);
    function  LiveCount: cardinal;
    procedure Pop( Idx: integer);

  public
    constructor CreateHeavyEx(
      ADatum: pointer; AMaxPopulation, AMinReserve: cardinal; MaxAge: double;
      const ADefaultFlavour: IResourceFlavour;
      AGenFunc: TGenerateEx<T>; ARelFunc: TDestroy<T>;
      ATasteItem: TTaste<T>);
    destructor Destroy; override;
  end;

  THeavyPool<T> = class( THeavyPoolEx<T>, IHeavyPool<T>)
  private
    function IHeavyPool<T>.Acquire = AcquireVanilla;
  public
    constructor CreateHeavy(
      ADatum: pointer; AMaxPopulation, AMinReserve: cardinal; MaxAge: double;
      AGenFunc: TGenerate<T>; ARelFunc: TDestroy<T>);
  end;

type
  THouseKeeperThread<T> = class( TThread)
  private
    FOwner: THeavyPoolEx<T>;
    FGate: ILock;

    procedure HouseKeepProper;

  protected
    procedure Execute; override;
  public
    constructor CreateHouseKeeper( AOwner: THeavyPoolEx<T>);
    destructor Destroy; override;
  end;



implementation








uses System.SyncObjs;

constructor THeavyPoolEx<T>.CreateHeavyEx(
  ADatum: pointer; AMaxPopulation, AMinReserve: cardinal; MaxAge: double;
  const ADefaultFlavour: IResourceFlavour;
  AGenFunc: TGenerateEx<T>; ARelFunc: TDestroy<T>;
  ATasteItem: TTaste<T>);
begin
  FDatum      := ADatum;
  FMaxPop     := AMaxPopulation;
  FMinReserve := AMinReserve;
  FMaxAge     := MaxAge;
  FFreePool   := TBasketList.Create;
  FAcquired   := TList<T>.Create;
  FGate       := _CreateCritLockIntf( nil);
  FGenFunc    := AGenFunc;
  FVanillaGenFunc := function(): T
    begin
      if assigned( @FGenFunc) then
          result := FGenFunc( FDefaultFlavour)
        else
          result := Default(T)
    end;
  FRelFunc    := ARelFunc;
  FTaste      := ATasteItem;
  FDefaultFlavour := ADefaultFlavour;
  FGenerateCount := 0;
  FDestroyCount  := 0;
  FAcquireCount  := 0;
  FReleaseCount  := 0;
  FWakeHouseKeeper  := _CreateKernelEventIntf( nil, False, True); // Auto-reset. Starts set.
  FIsShutdown.Initialize( 0);
  FHouseKeeperThread := THouseKeeperThread<T>.CreateHouseKeeper( self);
  FWakeHouseKeeper.SetEvent
end;

destructor THeavyPoolEx<T>.Destroy;
begin
  Shutdown;
  FIsShutdown.Finalize;
  FWakeHouseKeeper := nil;
  FHouseKeeperThread := nil;
  FGate := nil;
  FreeAndNil( FAcquired);
  FreeAndNil( FFreePool);
  inherited
end;




constructor THouseKeeperThread<T>.CreateHouseKeeper( AOwner: THeavyPoolEx<T>);
begin
  inherited Create;
  FOwner := AOwner;
  FGate  := FOwner.FGate
end;

destructor THouseKeeperThread<T>.Destroy;
begin
  inherited;
end;

procedure THouseKeeperThread<T>.Execute;
var
  doBreak: boolean;
begin
  doBreak := False;
  try
    repeat
      case FOwner.FWakeHouseKeeper.WaitFor( 1000) of
        wrSignaled                          : HouseKeepProper;
        wrTimeout                           : ;
        wrAbandoned, wrError, wrIOCompletion: doBreak := True;
      end;
    until doBreak or (FOwner.FIsShutdown.Read > 0)
  except
    doBreak := True
    end;
  FOwner.FIsShutdown.Write( 2)
end;




procedure THouseKeeperThread<T>.HouseKeepProper;
begin
  FOwner.HouseKeepProper
end;


function THeavyPoolEx<T>.FreeItemMatches( FreePoolIdx: integer; const MatchingFlavour: IResourceFlavour): boolean;
var
  ItemFlavour: IResourceFlavour;
begin
  if assigned( @FTaste) then
      ItemFlavour := FTaste( FFreepool[ FreePoolIdx].FObject)
    else
      ItemFlavour := nil;
  result := FlavourMatches( ItemFlavour, MatchingFlavour)
end;

function THeavyPoolEx<T>.NextToMature( const MatchingFlavour: IResourceFlavour): integer;
var
  i: integer;
  MinMat, Mat: TDateTime;
begin
  MinMat := 0;
  result := -1;
  for i := 0 to FFreePool.Count - 1 do
    begin
    Mat := FFreePool[i].FMaturity;
    if (Mat < MinMat) or (MinMat <= 0) and FreeItemMatches( i, MatchingFlavour) then
      begin
        MinMat := Mat;
        result := i
      end
    end
end;

function THeavyPoolEx<T>.NextToMatureAny: integer;
var
  i: integer;
  MinMat, Mat: TDateTime;
begin
  MinMat := 0;
  result := -1;
  for i := 0 to FFreePool.Count - 1 do
    begin
    Mat := FFreePool[i].FMaturity;
    if (Mat < MinMat) or (MinMat <= 0) then
      begin
        MinMat := Mat;
        result := i
      end
    end
end;

procedure THeavyPoolEx<T>.PopAnOldOne;
var
  Idx, i: integer;
begin
  Idx := NextToMatureAny;
  if Idx <> -1 then
    Pop( Idx)
end;


function THeavyPoolEx<T>.AcquireVanilla: T;
begin
  result := Acquire( GetDefaultFlavour)
end;


function THeavyPoolEx<T>.Acquire( const MatchingFlavour: IResourceFlavour): T;
var
  MinIdx: integer;
  Res: TAquireResult;
  S: String;
begin
  S := MatchingFlavour.Descriptor;
  FGate.Enter;
  try
    if cardinal( FAcquired.Count) >= FMaxPop then
        Res := aOverPop
      else
        begin
        MinIdx := NextToMature( MatchingFlavour);
        if MinIdx >= 0 then
            begin
            Res    := aRecycle;
            result := FFreePool[ MinIdx].FObject;
            FFreePool.Delete( MinIdx);
            Inc( FAcquireCount);
            FAcquired.Add( result)
            end

          else if assigned( @FGenFunc) and (LiveCount < FMaxPop) then
            begin
            Res    := aNew;
            result := FGenFunc( MatchingFlavour);
            Inc( FGenerateCount);
            Inc( FAcquireCount);
            FAcquired.Add( result)
            end

          else if assigned( @FGenFunc) and (FFreePool.Count > 0) then
            begin
            PopAnOldOne;
            Res    := aNew;
            result := FGenFunc( MatchingFlavour);
            Inc( FGenerateCount);
            Inc( FAcquireCount);
            FAcquired.Add( result)
            end

          else if assigned( @FGenFunc) then
            Res := aOverPop

          else
            Res := aNoGenerator
        end
  finally
    FGate.Leave;
    FWakeHouseKeeper.SetEvent
  end;
  case Res of
    aOverPop    : raise Exception.Create( 'THeavyPoolEx<T>.Acquire() - Over-population');
    aNoGenerator: raise Exception.Create( 'THeavyPoolEx<T>.Acquire() - No generator function defined');
  end;
end;



function THeavyPoolEx<T>.GetDatum: pointer;
begin
  result := FDatum;
end;

function THeavyPoolEx<T>.GetDefaultFlavour: IResourceFlavour;
begin
  FGate.Enter;
  try
    result := FDefaultFlavour
  finally
    FGate.Leave
  end
end;

function THeavyPoolEx<T>.GetMaxAge: double;
begin
  FGate.Enter;
  result := FMaxAge;
  FGate.Leave
end;

function THeavyPoolEx<T>.GetMaxPop: cardinal;
begin
  FGate.Enter;
  result := FMaxPop;
  FGate.Leave
end;

function THeavyPoolEx<T>.GetMinReserve: cardinal;
begin
  FGate.Enter;
  result := FMinReserve;
  FGate.Leave
end;

function THeavyPoolEx<T>.GetStats: RHeavyPoolStats;
begin
  FGate.Enter;
  result.FGenerateCount  := FGenerateCount;
  result.FDestroyCount   := FDestroyCount;
  result.FAcquireCount   := FAcquireCount;
  result.FReleaseCount   := FReleaseCount;
  result.FFreeQueueCount := FFreePool.Count;
  result.FInWorkCount    := FAcquired.Count;
  FGate.Leave
end;

procedure THeavyPoolEx<T>.Pop( Idx: integer);
begin
  if assigned( FRelFunc) then
    FRelFunc( FFreePool[ Idx].FObject);
  FFreePool.Delete( Idx);
  Inc( FDestroyCount)
end;

procedure THeavyPoolEx<T>.HouseKeepProper;
var
  MinIdx, i: integer;
  dNow: TDateTime;
  Addend:  THeavyPoolEx<T>.RBasket;
  didAnAction: boolean;

begin
  repeat
    didAnAction := False;
    FGate.Enter;
    try
      if assigned( FRelFunc) then
        begin
        // Check for the Maximum Population restriction. Enforce it.
        if (LiveCount > FMaxPop) and (FFreePool.Count > 0) then
          begin
          MinIdx := NextToMatureAny;
          if MinIdx >= 0 then
            begin
            Pop( MinIdx);
            didAnAction := True
            end
          end;

        // Check for the Maxiumum Age restriction. Retire items that are stale (too old).
        dNow := Now;
        if not didAnAction then
          for i := FFreePool.Count - 1 downto 0 do
            begin
              if FFreePool[ i].FMaturity < dNow then
                begin
                Pop( MinIdx);
                didAnAction := True;
                break
                end
            end
        end;

      // Check for the Minimum Free Pool Requirement. Pre-build, if required.
      if assigned( @FGenFunc) and
         (cardinal( FFreePool.Count) < FMinReserve) and (LiveCount < FMaxPop) then
            begin
            if didAnAction then
                // Do it soon, but not just now.
                // Don't want to do too much at once.
                FWakeHouseKeeper.SetEvent
              else
                begin
                Addend.FObject := FGenFunc( FDefaultFlavour);
                Touch( Addend);
                FFreePool.Add( Addend);
                Inc( FGenerateCount);
                didAnAction := True
                end
            end
    finally
      FGate.Leave
    end
  until not didAnAction
end;




function THeavyPoolEx<T>.LiveCount: cardinal;
begin
  result := cardinal( FAcquired.Count + FFreePool.Count)
end;

procedure THeavyPoolEx<T>.Release( const Retiree: T);
var
  Idx: integer;
  Addend: RBasket;
begin
  FGate.Enter;
  try
  Idx := FAcquired.IndexOf( Retiree);
  if Idx = -1 then
      begin
      if assigned( FRelFunc) then
        FRelFunc( Retiree);
      Inc( FDestroyCount)
      end
    else
      begin
      Inc( FReleaseCount);
      FAcquired.Delete( Idx);
      Addend.FObject := Retiree;
      Touch( Addend);
      FFreePool.Add( Addend);
      FWakeHouseKeeper.SetEvent
      end;
  finally
    FGate.Leave
  end
end;

procedure THeavyPoolEx<T>.ReleaseNoRecycle( const Retiree: T);
var
  Idx: integer;
  Addend: RBasket;
begin
  FGate.Enter;
  try
  Idx := FAcquired.IndexOf( Retiree);
  if Idx <> -1 then
    begin
    Inc( FReleaseCount);
    FAcquired.Delete( Idx);
    FWakeHouseKeeper.SetEvent
    end;
  Inc( FDestroyCount);
  if assigned( FRelFunc) then
    FRelFunc( Retiree)
  finally
    FGate.Leave
  end
end;

procedure THeavyPoolEx<T>.SetDefaultFlavour( const Value: IResourceFlavour);
begin
  FGate.Enter;
  try
    FDefaultFlavour := Value
  finally
    FGate.Leave
  end
end;

procedure THeavyPoolEx<T>.SetMaxAge( const Value: double);
begin
  FGate.Enter;
  FMaxAge := Value;
  FGate.Leave;
  FWakeHouseKeeper.SetEvent
end;

procedure THeavyPoolEx<T>.SetMaxPop( const Value: cardinal);
begin
  FGate.Enter;
  FMaxPop := Value;
  FGate.Leave;
  FWakeHouseKeeper.SetEvent
end;

procedure THeavyPoolEx<T>.SetMinReserve( const Value: cardinal);
begin
  FGate.Enter;
  FMinReserve := Value;
  FGate.Leave;
  FWakeHouseKeeper.SetEvent
end;

procedure THeavyPoolEx<T>.ShutDown;
var
  i: integer;
begin
  if not FIsShutdown.CompareAndExchange( 0, 1) then exit;
  FWakeHouseKeeper.SetEvent;
  FHouseKeeperThread.WaitFor;
  FGenFunc := nil;
  if assigned( FRelFunc) then
    begin
    FGate.Enter;
    try
      for i := FFreePool.Count - 1 downto 0 do
        begin
        FRelFunc( FFreePool[i].FObject);
        Inc( FReleaseCount)
        end
    finally
      FGate.Leave;
      FRelFunc := nil
      end
    end;
  FIsShutdown.Finalize;
  FWakeHouseKeeper := nil;
  FGate := nil;
  FreeAndNil( FAcquired);
  FreeAndNil( FFreePool)
end;



procedure THeavyPoolEx<T>.Touch( var Item: RBasket);
begin
  Item.FMaturity := Now() + FMaxAge
end;


constructor THeavyPool<T>.CreateHeavy(
  ADatum: pointer; AMaxPopulation,
  AMinReserve: cardinal; MaxAge: double; AGenFunc: TGenerate<T>;
  ARelFunc: TDestroy<T>);
begin
  inherited CreateHeavyEx( ADatum, AMaxPopulation, AMinReserve, MaxAge, nil,
      function( const OfFlavour: IResourceFlavour): T
        begin
          if assigned( @FVanillaGenFunc) then
              result := FVanillaGenFunc
            else
              result := Default( T)
        end,
      ARelFunc, nil);
  FVanillaGenFunc := AGenFunc
end;

end.
