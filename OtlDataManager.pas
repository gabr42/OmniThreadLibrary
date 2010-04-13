unit OtlDataManager;

{$IF CompilerVersion >= 21}
  {$DEFINE OTL_ERTTI}
{$IFEND}

interface

uses
  GpStuff,
  OtlCommon;

type
  TOmniSourceProviderCapability = (spcCountable);
  TOmniSourceProviderCapabilities = set of TOmniSourceProviderCapability;

// TODO 1 -oPrimoz Gabrijelcic : Handle negative step!

  ///<summary>Wrapper around a (type specific) data package. Split method will be
  ///    called from a context of a non-owning thread!</summary>
  TOmniDataPackage = class abstract
    function  GetNext(var value: TOmniValue): boolean; virtual; abstract;
    function  Split(package: TOmniDataPackage): boolean; virtual; abstract;
  end; { TOmniDataPackage }

  TOmniLocalQueue = class abstract
    function  GetNext(var value: TOmniValue): boolean; virtual; abstract;
  end; { TOmniLocalQueue }

  ///<summary>Wrapper around the data source. All methods can and will be called from
  ///    multiple threads at the same time!</summary>
  TOmniSourceProvider = class abstract
    function  Count: int64; virtual; abstract;
    function  CreateDataPackage: TOmniDataPackage; virtual; abstract;
    function  GetCapabilities: TOmniSourceProviderCapabilities; virtual; abstract;
    function  GetPackage(dataCount: integer; var package: TOmniDataPackage): boolean; virtual; abstract;
  end; { TOmniSourceProvider }

  TOmniDataManager = class
  end; { TOmniDataManager }

function CreateSourceProvider(low, high: int64; step: int64 = 1): TOmniSourceProvider; overload;
function CreateSourceProvider(enumerable: IOmniValueEnumerable): TOmniSourceProvider; overload;
function CreateSourceProvider(enumerable: IEnumerable): TOmniSourceProvider; overload;

{$IFDEF OTL_ERTTI}
function CreateSourceProvider(enumerable: TObject): TOmniSourceProvider; overload;
{$ENDIF OTL_ERTTI}

implementation

uses
  SysUtils,
  DSiWin32,
  OtlSync;

type
  TOmniIntegerDataPackage = class(TOmniDataPackage)
  strict private
    idpHigh: int64;
    idpLow : TGp8AlignedInt64;
    idpStep: int64;
  public
    function  GetNext(var value: TOmniValue): boolean; override;
    procedure Initialize(low, high, step: int64);
    function  Split(package: TOmniDataPackage): boolean; override;
  end; { TOmniIntegerDataPackage }

  TOmniIntegerRangeProvider = class(TOmniSourceProvider)
  strict private
    irpCount: TGp8AlignedInt64;
    irpHigh : int64;
    irpLock : TOmniCS;
    irpLow  : int64;
    irpStep : int64;
  strict protected
    procedure RecalcCount; inline;
  public
    constructor Create(low, high, step: int64);
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

{ exports }

function CreateSourceProvider(low, high, step: int64): TOmniSourceProvider;
begin
  Result := TOmniIntegerRangeProvider.Create(low, high, step);
end; { CreateSourceProvider }

function CreateSourceProvider(enumerable: IOmniValueEnumerable): TOmniSourceProvider;
begin
//  Result := TOmniValueEnumerableProvider.Create(enumerable);
end; { CreateSourceProvider }

function CreateSourceProvider(enumerable: IEnumerable): TOmniSourceProvider; overload;
begin
//  Result := TOmniEnumerableProvider.Create(enumerable);
end; { CreateSourceProvider }

{$IFDEF OTL_ERTTI}
function CreateSourceProvider(enumerable: TObject): TOmniSourceProvider;
begin
//  Result := TOmniDelphiEnumeratorProvider.Create(enumerable);
end; { CreateSourceProvider }
{$ENDIF OTL_ERTTI}

{ TOmniIntegerDataPackage }

function TOmniIntegerDataPackage.GetNext(var value: TOmniValue): boolean;
begin
  // TODO 1 -oPrimoz Gabrijelcic : Problem! Multiple GetNext calls when idpLow > idpHigh can wrap around idpLow (if idpHigh is close to High(int64))
  value.AsInt64 := idpLow.Increment(idpStep) - idpStep;
  Result := (value.AsInt64 <= idpHigh);
end; { TOmniIntegerDataPackage.GetNext }

procedure TOmniIntegerDataPackage.Initialize(low, high, step: int64);
begin
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
  Result := intPackage.GetNext(value);
  if Result then
    intPackage.Initialize(value, value, 1);
end; { TOmniIntegerDataPackage.Split }

{ TOmniIntegerRangeProvider }

constructor TOmniIntegerRangeProvider.Create(low, high, step: int64);
begin
  inherited Create;
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
  irpCount.Value := (irpHigh - irpLow + irpStep) div irpStep;
end; { TOmniIntegerRangeProvider.RecalcCount }

end.
