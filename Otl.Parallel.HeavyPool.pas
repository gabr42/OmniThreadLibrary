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
//•	Available (that is to say, not in current use) pooled objects can have a maximum age. If they exceed this age, then are destroyed.
//•	A minimum number of available objects. When less, a background house keeping pool will slowly grow the pool.
//The user supplies functions to create/destroy the objects, or take action on the event of Acquire from/ release back to the pool. Removing too-old pool objects, or growing the pool to the specified minimum are house-keeping jobs performed by a task on a periodic basis.

type
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
    FAcquisitionCount: uint64;
    FReleaseCount: uint64;
    FHouseKeeperThread: TThread;
    FWakeHouseKeeper: IEvent;
    [Volatile] FIsShutdown: TVolatileInt32;
    FGenFunc: TGenerate<T>;
    FRelFunc: TDestroy<T>;

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
  FAcquisitionCount := 0;
  FReleaseCount     := 0;
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
begin
  FGate.Enter;
  try
    if FAcquired.Count >= FMaxPop then
        result := Default(T)
      else
        begin
        MinIdx := NextToMature;
        if MinIdx >= 0 then
            begin
            result := FFreePool[ MinIdx].FObject;
            FFreePool.Delete( MinIdx);
            FAcquired.Add( result)
            end

          else if assigned( @FGenFunc) and ((FAcquired.Count + FFreePool.Count) < FMaxPop) then
            begin
            result := FGenFunc;
            FAcquired.Add( result)
            end

          else
            result := Default(T)
        end
  finally
    FGate.Leave;
    FWakeHouseKeeper.SetEvent
  end
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
  with result do
    begin
    FGenerateCount  := 0;
    FDestroyCount   := 0;
    FAcquireCount   := 0;
    FReleaseCount   := 0;
    FFreeQueueCount := 0;
    FInWorkCount    := 0
    end;
  FGate.Leave
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
        if ((FAcquired.Count + FFreePool.Count) > FMaxPop) and (FFreePool.Count > 0) then
          begin
          MinIdx := NextToMature;
          if MinIdx >= 0 then
            begin
            FRelFunc( FFreePool[ MinIdx].FObject);
            FFreePool.Delete( MinIdx);
            didAnAction := True
            end
          end;

        // Check for the Maxiumum Age restriction. Retire items that are stale (too old).
        if not didAnAction then
          for i := FFreePool.Count - 1 downto 0 do
            begin
              if FFreePool[ i].FMaturity >= dNow then
                begin
                FRelFunc( FFreePool[ MinIdx].FObject);
                FFreePool.Delete( MinIdx);
                didAnAction := True;
                break
                end
            end
        end;

      // Check for the Minimum Free Pool Requirement. Pre-build, if required.
      if (not didAnAction) and assigned( @FGenFunc) and
         (FFreePool.Count < FMinReserve) and ((FAcquired.Count + FFreePool.Count) < FMaxPop) then
            begin
            Addend.FObject   := FGenFunc;
            Touch( Addend);
            FFreePool.Add( Addend);
            didAnAction := True
            end;
    finally
      FGate.Leave
    end
  until not didAnAction
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
        FRelFunc( FFreePool[i].FObject)
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
