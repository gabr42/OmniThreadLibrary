unit Otl.Parallel.HeavyPool;
{$I OtlOptions.inc}

interface
uses Otl.Parallel.Extras, System.SysUtils, Generics.Collections,
     Otl.Parallel.SynchroPrimitives.InterfaceLevel, System.Classes,
     Otl.Parallel.Atomic;

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

  THeavyPool<T> = class( TInterfacedObject, IHeavyPool<T>)
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
    [Volatile] FIsShutdown: TVolatileInt32;
    FGenFunc: TGenerate<T>;
    FRelFunc: TDestroy<T>;

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
    function  Acquire: T;
    procedure Release( const Retiree: T);
    function  NextToMature: integer;
    procedure HouseKeepProper;
    procedure Touch( var Item: RBasket);
    function  LiveCount: cardinal;
    procedure Pop( Idx: integer);

  public
    constructor CreateHeavy( ADatum: pointer; AMaxPopulation, AMinReserve: cardinal; MaxAge: double; AGenFunc: TGenerate<T>; ARelFunc: TDestroy<T>);
    destructor Destroy; override;
  end;


type
  THouseKeeperThread<T> = class( TThread)
  private
    FOwner: THeavyPool<T>;
    FGate: ILock;

    procedure HouseKeepProper;

  protected
    procedure Execute; override;
  public
    constructor CreateHouseKeeper( AOwner: THeavyPool<T>);
  end;



implementation








uses System.SyncObjs;

constructor THeavyPool<T>.CreateHeavy(
  ADatum: pointer; AMaxPopulation, AMinReserve: cardinal; MaxAge: double;
  AGenFunc: TGenerate<T>; ARelFunc: TDestroy<T>);
begin
  FDatum      := ADatum;
  FMaxPop     := AMaxPopulation;
  FMinReserve := AMinReserve;
  FMaxAge     := MaxAge;
  FFreePool   := TBasketList.Create;
  FAcquired   := TList<T>.Create;
  FGate       := _CreateCritLockIntf( nil);
  FGenFunc    := AGenFunc;
  FRelFunc    := ARelFunc;
  FGenerateCount := 0;
  FDestroyCount  := 0;
  FAcquireCount  := 0;
  FReleaseCount  := 0;
  FWakeHouseKeeper  := _CreateKernalEventIntf( nil, False, True); // Auto-reset. Starts set.
  FIsShutdown.Initialize( 0);
  FHouseKeeperThread := THouseKeeperThread<T>.CreateHouseKeeper( self);
  FWakeHouseKeeper.SetEvent
end;

destructor THeavyPool<T>.Destroy;
begin
  Shutdown;
  FIsShutdown.Finalize;
  FWakeHouseKeeper := nil;
  FHouseKeeperThread := nil;
  FGate := nil;
  FAcquired.Free;
  FFreePool.Free;
  inherited
end;




constructor THouseKeeperThread<T>.CreateHouseKeeper( AOwner: THeavyPool<T>);
begin
  FOwner := AOwner;
  FGate  := FOwner.FGate
end;

procedure THouseKeeperThread<T>.Execute;
var
  doBreak: boolean;
begin
  doBreak := False;
  repeat
    case FOwner.FWakeHouseKeeper.WaitFor( 1000) of
      wrSignaled                          : HouseKeepProper;
      wrTimeout                           : ;
      wrAbandoned, wrError, wrIOCompletion: doBreak := True;
    end;
  until doBreak or (FOwner.FIsShutdown.Read > 0);
  FOwner.FIsShutdown.Write( 2)
end;




procedure THouseKeeperThread<T>.HouseKeepProper;
begin
  FOwner.HouseKeepProper
end;



function THeavyPool<T>.NextToMature: integer;
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



function THeavyPool<T>.Acquire: T;
var
  MinIdx: integer;
  Res: TAquireResult;
begin
  FGate.Enter;
  try
    if cardinal( FAcquired.Count) >= FMaxPop then
        Res := aOverPop
      else
        begin
        MinIdx := NextToMature;
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
            result := FGenFunc;
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
    aOverPop    : raise Exception.Create( 'THeavyPool<T>.Acquire() - Over-population');
    aNoGenerator: raise Exception.Create( 'THeavyPool<T>.Acquire() - No generator function defined');
  end;
end;



function THeavyPool<T>.GetDatum: pointer;
begin
  result := FDatum
end;

function THeavyPool<T>.GetMaxAge: double;
begin
  FGate.Enter;
  result := FMaxAge;
  FGate.Leave
end;

function THeavyPool<T>.GetMaxPop: cardinal;
begin
  FGate.Enter;
  result := FMaxPop;
  FGate.Leave
end;

function THeavyPool<T>.GetMinReserve: cardinal;
begin
  FGate.Enter;
  result := FMinReserve;
  FGate.Leave
end;

function THeavyPool<T>.GetStats: RHeavyPoolStats;
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

procedure THeavyPool<T>.Pop( Idx: integer);
begin
  FRelFunc( FFreePool[ Idx].FObject);
  FFreePool.Delete( Idx);
  Inc( FDestroyCount)
end;

procedure THeavyPool<T>.HouseKeepProper;
var
  MinIdx, i: integer;
  dNow: TDateTime;
  Addend:  THeavyPool<T>.RBasket;
  didAnAction: boolean;

begin
  didAnAction := False;
  repeat
    FGate.Enter;
    try
      if assigned( FRelFunc) then
        begin
        // Check for the Maximum Population restriction. Enforce it.
        if (LiveCount > FMaxPop) and (FFreePool.Count > 0) then
          begin
          MinIdx := NextToMature;
          if MinIdx >= 0 then
            begin
            Pop( MinIdx);
            didAnAction := True
            end
          end;

        // Check for the Maxiumum Age restriction. Retire items that are stale (too old).
        if not didAnAction then
          for i := FFreePool.Count - 1 downto 0 do
            begin
              if FFreePool[ i].FMaturity >= dNow then
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
                Addend.FObject := FGenFunc;
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




function THeavyPool<T>.LiveCount: cardinal;
begin
  result := cardinal( FAcquired.Count + FFreePool.Count)
end;

procedure THeavyPool<T>.Release( const Retiree: T);
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
        FRelFunc( Retiree)
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

procedure THeavyPool<T>.SetMaxAge( const Value: double);
begin
  FGate.Enter;
  FMaxAge := Value;
  FGate.Leave;
  FWakeHouseKeeper.SetEvent
end;

procedure THeavyPool<T>.SetMaxPop( const Value: cardinal);
begin
  FGate.Enter;
  FMaxPop := Value;
  FGate.Leave;
  FWakeHouseKeeper.SetEvent
end;

procedure THeavyPool<T>.SetMinReserve( const Value: cardinal);
begin
  FGate.Enter;
  FMinReserve := Value;
  FGate.Leave;
  FWakeHouseKeeper.SetEvent
end;

procedure THeavyPool<T>.ShutDown;
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
  FAcquired.Free;
  FFreePool.Free
end;



procedure THeavyPool<T>.Touch( var Item: RBasket);
begin
  Item.FMaturity := Now() + FMaxAge
end;

end.
