program Project73;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  GpStuff,
  OtlAsyncStreams,
  OtlAsyncStreams.FileSystem;

var
  reader, writer: IOmniAsyncStream;

  procedure HandleRead(const buffer: IGpBuffer);
  begin
    writer.Write(buffer);
  end;

  procedure ReportReadError;
  begin
  end;

  procedure ReportWriteError;
  begin
  end;

begin
  reader := CreateOmniAsyncFileStream(ParamStr(1), fmOpenRead)
              .Config(CreateOmniAsyncStreamStrategy.Buffer(10240))
              .OnData(HandleRead)
              .OnError(ReportReadError)
              .OnDone(procedure begin reader := nil; writer := nil end);
  writer := CreateOmniAsyncFileStream(ParamStr(2), fmCreate)
              .OnError(procedure begin reader.Cancel; ReportWriteError; end);
  if reader.Open and writer.Open then
    reader.Read;
end.

