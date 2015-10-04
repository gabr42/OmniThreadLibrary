unit OtlAsyncStreams.Common;

interface

uses
  GpStuff,
  OtlAsyncStreams;

type
  TOmniAsyncDriverCapability = (
    capAsynchronous, //does not block the thread
    capRandomAccess, //supports SetPosition
    capResizable,    //supports SetSize
    capCanRead,      //supports Read
    capCanWrite      //supports Write
  );
  TOmniAsyncDriverCapabilities = set of TOmniAsyncDriverCapability;

  TOmniAsyncDriverStatusResult = (resOK, resFailed);

  IOmniAsyncDriverStatus = interface ['{900E9C58-4C2A-4F68-A9B2-B0A3D3DC9312}']
    function GetErrorCode: integer;
    function GetResult: TOmniAsyncDriverStatusResult;
  //
    property Result: TOmniAsyncDriverStatusResult read GetResult;
    property ErrorCode: integer read GetErrorCode;
  end;

  TOmniAsyncDriverDataHandler = reference to procedure (
    const data: IGpBuffer;
    const status: IOmniAsyncDriverStatus);

  IOmniAsyncDriver = interface ['{5B1CE750-DA1A-4AC4-B391-9CD6A6A41C11}']
    function  Open: boolean;
    procedure Close;
    function  Capabilities: TOmniAsyncDriverCapabilities;
  end;

  IOmniAsyncReadDriver = interface ['{9A79C907-8C7C-4968-BB64-8481D1BF9171}']
    function Read(size: int64; handler: TOmniAsyncDriverDataHandler): boolean;
  end;

  IOmniSyncReadDriver = interface ['{2CA3A553-DC63-4B0C-B6CC-D56B41F2395B}']
    function Read(size: int64; out buffer: IGpBuffer): boolean;
  end;

  IOmniAsyncWriteDriver = interface ['{A61BA963-6318-4B77-AF2B-EE132754FB44}']
  end;

  IOmniSyncWriteDriver = interface ['{D185B39D-BACF-47C9-97CB-93803126B1D5}']
  end;

  IOmniRandomAccessDriver = interface ['{E4F81D7A-9F44-455E-B9C7-E3987F210A33}']
    procedure SetPosition(position: int64);
  end;

  IOmniResizableDriver = interface ['{B46B5593-3520-4D91-AB2C-B6A7816E660D}']
    procedure SetSize(size: int64);
  end;

function CreateOmniAsyncStream(const driver: IOmniAsyncDriver): IOmniAsyncStream;

implementation

uses
  System.SysUtils;

type
  TOmniAsyncStream = class(TInterfacedObject, IOmniAsyncStream)
  strict private
    FDriver            : IOmniAsyncDriver;
    FDriverCapabilities: TOmniAsyncDriverCapabilities;
    FIsOpen            : boolean;
    FOnDataHandler     : TOmniAsyncStreamDataHandler;
    FOnDoneHandler     : TOmniAsyncStreamDoneHandler;
    FOnErrorHandler    : TOmniAsyncStreamErrorHandler;
    FOnExceptionHandler: TOmniAsyncStreamExceptionHandler;
  strict protected
    procedure CheckIsOpen(expectedOpen: boolean = true);
    function  GetPosition: int64;
    function  GetSize: int64;
    procedure SetPosition(value: int64);
    procedure SetSize(value: int64);
  public
    constructor Create(const driver: IOmniAsyncDriver);
    function  Open:  boolean;
    procedure Close;
    procedure Cancel;
    function  Config(strategy: IOmniAsyncStreamStrategy): IOmniAsyncStream;
    function  OnData(handler: TOmniAsyncStreamDataHandler): IOmniAsyncStream;
    function  OnError(handler: TOmniAsyncStreamErrorHandler): IOmniAsyncStream;
    function  OnException(handler: TOmniAsyncStreamExceptionHandler): IOmniAsyncStream;
    function  OnDone(handler: TOmniAsyncStreamDoneHandler): IOmniAsyncStream;
    procedure Read(howMuch: integer = OTL_READ_ALL);
    procedure Write(const buffer: IGpBuffer);
    property Position: int64 read GetPosition write SetPosition;
    property Size: int64 read GetSize write SetSize;
  end;

function CreateOmniAsyncStream(const driver: IOmniAsyncDriver): IOmniAsyncStream;
begin
  Result := TOmniAsyncStream.Create(driver);
end;

{ TOmniAsyncStream }

constructor TOmniAsyncStream.Create(const driver: IOmniAsyncDriver);
begin
  inherited Create;
  FDriver := driver;
  FDriverCapabilities := FDriver.Capabilities;
end;

procedure TOmniAsyncStream.Cancel;
begin

end;

procedure TOmniAsyncStream.CheckIsOpen(expectedOpen: boolean = true);
begin
  if FIsOpen <> expectedOpen then
    raise Exception.Create('Stream is not ' + IFF(expectedOpen, 'open', 'closed'));
end;

procedure TOmniAsyncStream.Close;
begin
  CheckIsOpen;
  FDriver.Close;
  FIsOpen := false;
end;

function TOmniAsyncStream.Config(strategy: IOmniAsyncStreamStrategy): IOmniAsyncStream;
begin
end;

function TOmniAsyncStream.GetPosition: int64;
begin
end;

function TOmniAsyncStream.GetSize: int64;
begin
end;

function TOmniAsyncStream.OnData(handler: TOmniAsyncStreamDataHandler): IOmniAsyncStream;
begin
  FOnDataHandler := handler;
  Result := Self;
end;

function TOmniAsyncStream.OnDone(handler: TOmniAsyncStreamDoneHandler): IOmniAsyncStream;
begin
  FOnDoneHandler := handler;
  Result := Self;
end;

function TOmniAsyncStream.OnError(
  handler: TOmniAsyncStreamErrorHandler): IOmniAsyncStream;
begin
  FOnErrorHandler := handler;
  Result := Self;
end;

function TOmniAsyncStream.OnException(
  handler: TOmniAsyncStreamExceptionHandler): IOmniAsyncStream;
begin
  FOnExceptionHandler := handler;
  Result := Self;
end;

function TOmniAsyncStream.Open: boolean;
begin
  CheckIsOpen(false);
  FIsOpen := FDriver.Open;
  Result := FIsOpen;
end;

procedure TOmniAsyncStream.Read(howMuch: integer);
begin
  CheckCapability(capCanRead);
  // - all file
  // - not all file but more than the driver's buffer
  // - less than the driver's buffer
  // in all cases just use one read buffer at the time; everything else would get too complicated atm

  if capAsynchronous in FDriverCapabilities then begin
    FDriver.Read(howmuch?, whattodo?)
  end
  else begin

  end;

end;

procedure TOmniAsyncStream.SetPosition(value: int64);
begin
  // TODO 1 Implement: TOmniAsyncStream.SetPosition
end;

procedure TOmniAsyncStream.SetSize(value: int64);
begin
  // TODO 1 Implement: TOmniAsyncStream.SetSize
end;

procedure TOmniAsyncStream.Write(const buffer: IGpBuffer);
begin

end;

end.
