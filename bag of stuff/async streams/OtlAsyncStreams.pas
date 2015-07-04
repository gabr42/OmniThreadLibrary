unit OtlAsyncStreams;

// file stream
// any stream (threaded worker wrapper)
// Indy stream
// ICS stream

interface

uses
  System.SysUtils,
  GpStuff;

const
  OTL_READ_ALL = -1;

type
  IOmniAsyncStreamStrategy = interface ['{53937182-A709-4C2B-98FC-F038CB904DF8}']
  end;

  TOmniAsyncStreamDataHandler = reference to procedure (const buffer: IGpBuffer);
  TOmniAsyncStreamErrorHandler = reference to procedure;
  TOmniAsyncStreamExceptionHandler = reference to procedure (E: Exception);
  TOmniAsyncStreamDoneHandler = reference to procedure;

  IOmniAsyncStream = interface ['{948A09AB-93B9-42F4-8B7E-B81365B3DD60}']
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

implementation

end.
