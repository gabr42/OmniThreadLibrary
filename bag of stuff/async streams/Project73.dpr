program Project73;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

  // how to handle dispatch to form/thread seamlessly?
  procedure HandleRead(err: ?; buffer: ?);
  begin
    if not err then begin
      writer.Write(buffer, ?error handling?);
      reader.Read(blockSize, HandleRead);
    end;
  end;

  procedure HandleRead2(buffer: ????); //cannot be in error state
  begin
    writer.Write(buffer).Fail(
      procedure begin
        ReportWriteError;
        reader.Cancel;
      end
    reader.Read(blockSize).&Then(HandleRead2).Fail(ReportReadError);
  end;

  procedure HandleRead3(err: ???; buffer: ???);
  begin
    if not err then begin
      if not assigned(writer) then begin
        writer := CreateOmniAsyncFileStream(fileName, fmCreate);
        if not writer.Open then begin
          ReportWriteError;
          Exit;
        end;
      end;
      writer.Write(buffer).Fail(ReportWriteError);
      //reader.Read(?blocksize?) <- automatic
    end;
  end;

begin

  reader := CreateOmniAsyncFileStream;
  if reader.Open(fileName, fmOpenRead) then begin
    writer := CreateOmniAsyncFileStream;
    if writer.Open(fileName2, fmCreate) then
      reader.Read(blockSize, HandleRead);
  end;

  CreateOmniAsyncStream(someStream);

  // file stream
  // any stream
  // Indy stream
  // ICS stream

  // promises? [http://12devs.co.uk/articles/promises-an-alternative-way-to-approach-asynchronous-javascript/]

  reader := CreateOmniAsyncFileStream(fileName, fmOpenRead);
  if reader.Open then begin
    writer := CreateOmniAsyncFileStream(fileName2, fmCreate);
    if writer.Open then
      reader.Read(blockSize).&Then(HandleRead2).Fail(ReportReadError);
  end;

  // pipelining?

  // progress notification? [http://documentup.com/kriskowal/q]

  // exception handling/pipelining?

  reader := CreateOmniAsyncICSStream('http://...');
  if reader.Open then
    reader.Read(HandleRead3);

  // API

  IOmniAsyncStream = interface
    function Open: boolean;
//    function Read(howMuch: integer = OTL_READ_ALL; strategy: IOmniAsyncStreamStrategy = nil {implementation specific})
    function Read(HandleRead; HandleError);
    function Read(howMuch: integer; HandleRead; HandleError);
    function Read(howMuch: integer; strategy; HandleRead; HandleError);
  end;

end.
