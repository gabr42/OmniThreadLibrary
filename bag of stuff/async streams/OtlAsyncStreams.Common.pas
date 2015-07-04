unit OtlAsyncStreams.Common;

interface

uses
  GpStuff,
  OtlAsyncStreams;

type
  IOmniAsyncStreamDriver = interface ['{5B1CE750-DA1A-4AC4-B391-9CD6A6A41C11}']
  end;

function CreateOmniAsyncStream(const driver: IOmniAsyncStreamDriver): IOmniAsyncStream;

implementation

type
  TOmniAsyncStream = class(TInterfacedObject, IOmniAsyncStream)
  public
    constructor Create(const driver: IOmniAsyncStreamDriver);
    function  Open: boolean;
    procedure Close;
    procedure Cancel;
    function  Config(strategy: IOmniAsyncStreamStrategy): IOmniAsyncStream;
    function  OnData(handler: TOmniAsyncStreamDataHandler): IOmniAsyncStream;
    function  OnError(handler: TOmniAsyncStreamErrorHandler): IOmniAsyncStream;
    function  OnException(handler: TOmniAsyncStreamExceptionHandler): IOmniAsyncStream;
    function  OnDone(handler: TOmniAsyncStreamDoneHandler): IOmniAsyncStream;
    procedure Read(howMuch: integer);
    procedure Write(const buffer: IGpBuffer);
  end;

function CreateOmniAsyncStream(const driver: IOmniAsyncStreamDriver): IOmniAsyncStream;
begin
  Result := TOmniAsyncStream.Create(driver);
end;

{ TOmniAsyncStream }

constructor TOmniAsyncStream.Create(const driver: IOmniAsyncStreamDriver);
begin
end;

procedure TOmniAsyncStream.Cancel;
begin

end;

procedure TOmniAsyncStream.Close;
begin

end;

function TOmniAsyncStream.Config(strategy: IOmniAsyncStreamStrategy): IOmniAsyncStream;
begin

end;

function TOmniAsyncStream.OnData(handler: TOmniAsyncStreamDataHandler): IOmniAsyncStream;
begin

end;

function TOmniAsyncStream.OnDone(handler: TOmniAsyncStreamDoneHandler): IOmniAsyncStream;
begin

end;

function TOmniAsyncStream.OnError(
  handler: TOmniAsyncStreamErrorHandler): IOmniAsyncStream;
begin

end;

function TOmniAsyncStream.OnException(
  handler: TOmniAsyncStreamExceptionHandler): IOmniAsyncStream;
begin

end;

function TOmniAsyncStream.Open: boolean;
begin

end;

procedure TOmniAsyncStream.Read(howMuch: integer);
begin

end;

procedure TOmniAsyncStream.Write(const buffer: IGpBuffer);
begin

end;

end.
