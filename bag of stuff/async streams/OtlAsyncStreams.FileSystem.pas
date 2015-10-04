unit OtlAsyncStreams.FileSystem;

interface

uses
  Winapi.Windows, //what about a compatible non-Windows implementation?
  OtlAsyncStreams;

function CreateOmniAsyncFileStream(const fileName: string; access: integer): IOmniAsyncStream; overload;
function CreateOmniAsyncFileStream(const fileName: string; access, share: integer;
  securityAttributes: PSecurityAttributes = nil): IOmniAsyncStream; overload;

implementation

uses
  System.SysUtils,
  System.Classes,
  DSiWin32,
  OtlAsyncStreams.Common;

type
  TOmniAsyncFileStream = class(TInterfacedObject, IOmniAsyncStreamDriver)
  private
    FCreationDisposition: DWORD;
    FDesiredAccess      : DWORD;
    FFileName           : string;
    FHandle             : THandle;
    FSecurityAttributes : PSecurityAttributes;
    FShareMode          : DWORD;
  public
    constructor Create(const fileName: string; access, share: integer; securityAttributes:
      PSecurityAttributes);
    destructor Destroy; override;
    function  Capabilities: TOmniAsyncStreamDriverCapabilities;
    procedure Close;
    function  Open: boolean;
  end;

function CreateOmniAsyncFileStream(const fileName: string; access: integer): IOmniAsyncStream;
begin
  Result := CreateOmniAsyncFileStream(fileName, access, fmShareExclusive, nil);
end;

function CreateOmniAsyncFileStream(const fileName: string; access, share: integer;
  securityAttributes: PSecurityAttributes): IOmniAsyncStream;
begin
  Result := CreateOmniAsyncStream(TOmniAsyncFileStream.Create(fileName, access, share, securityAttributes));
end;

{ TOmniAsyncFileStream }

constructor TOmniAsyncFileStream.Create(const fileName: string; access, share: integer;
  securityAttributes: PSecurityAttributes);
begin
  inherited Create;
  FFileName := fileName;
  if access = fmCreate then
    FCreationDisposition := CREATE_ALWAYS
  else
    FCreationDisposition := OPEN_EXISTING;
  if access = fmOpenRead then
    FDesiredAccess := GENERIC_READ
  else if access = fmOpenWrite then
    FDesiredAccess := GENERIC_WRITE
  else
    FDesiredAccess := GENERIC_READ OR GENERIC_WRITE;
  if share = fmShareDenyWrite then
    FShareMode := FILE_SHARE_READ
  else if share = fmShareDenyRead then
    FShareMode := FILE_SHARE_WRITE
  else if share = fmShareDenyNone then
    FShareMode := FILE_SHARE_READ OR FILE_SHARE_WRITE;
  FSecurityAttributes := securityAttributes;
end;

destructor TOmniAsyncFileStream.Destroy;
begin
  Close;
  inherited;
end;

function TOmniAsyncFileStream.Capabilities: TOmniAsyncStreamDriverCapabilities;
begin
  Result := [capAsynchronous, capRandomAccess, capResizable, capCanRead, capCanWrite];
end;

procedure TOmniAsyncFileStream.Close;
begin
  DSICloseHandleAndInvalidate(FHandle);
end;

function TOmniAsyncFileStream.Open: boolean;
begin
  FHandle := CreateFile(PChar(FFileName), FDesiredAccess, FShareMode, FSecurityAttributes,
    FCreationDisposition, FILE_ATTRIBUTE_NORMAL OR FILE_FLAG_OVERLAPPED, 0);
  Result := (FHandle <> INVALID_HANDLE_VALUE);
end;

end.
