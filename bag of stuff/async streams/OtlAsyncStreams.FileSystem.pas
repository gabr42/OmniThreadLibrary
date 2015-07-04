unit OtlAsyncStreams.FileSystem;

interface

uses
  OtlAsyncStreams;

function CreateOmniAsyncFileStream(const fileName: string; mode: integer): IOmniAsyncStream;

implementation

uses
  OtlAsyncStreams.Common;

type
  TOmniAsyncFileStream = class(TInterfacedObject, IOmniAsyncStreamDriver)

  end;

function CreateOmniAsyncFileStream(const fileName: string; mode: integer):
  IOmniAsyncStream;
begin
  Result := CreateOmniAsyncStream(TOmniAsyncFileStream.Create);
end;

end.
