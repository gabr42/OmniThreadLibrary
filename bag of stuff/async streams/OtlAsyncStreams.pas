unit OtlAsyncStreams;

// file stream
// any stream (threaded worker wrapper)
// Indy stream
// ICS stream

// streams with direct access (SetPosition)
// unidirectional streams

// asynch operations
// threaded workers

interface

uses
  System.SysUtils,
  GpStuff;

const
  OTL_READ_ALL = -1;

type
  IOmniAsyncStreamStrategy = interface ['{53937182-A709-4C2B-98FC-F038CB904DF8}']
    function Buffer(bufferSize: integer): IOmniAsyncStreamStrategy;
  end;

  TOmniAsyncStreamDataHandler = reference to procedure (const buffer: IGpBuffer);
  TOmniAsyncStreamErrorHandler = reference to procedure;
  TOmniAsyncStreamExceptionHandler = reference to procedure (E: Exception);
  TOmniAsyncStreamDoneHandler = reference to procedure;

  IOmniAsyncStream = interface ['{948A09AB-93B9-42F4-8B7E-B81365B3DD60}']
    function  GetPosition: int64;
    function  GetSize: int64;
    procedure SetPosition(value: int64);
    procedure SetSize(value: int64);
  //
    function  Open: boolean;
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

  function CreateOmniAsyncStreamStrategy: IOmniAsyncStreamStrategy;

implementation

type
  TOmniAsyncStreamStrategy = class(TInterfacedObject, IOmniAsyncStreamStrategy)
  public
    function Buffer(bufferSize: integer): IOmniAsyncStreamStrategy;
  end;

function CreateOmniAsyncStreamStrategy: IOmniAsyncStreamStrategy;
begin
  Result := TOmniAsyncStreamStrategy.Create;
end;

{ TOmniAsyncStreamStrategy }

function TOmniAsyncStreamStrategy.Buffer(bufferSize: integer): IOmniAsyncStreamStrategy;
begin
  Result := Self;
end;

end.
