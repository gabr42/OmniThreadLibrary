{

Fast Memory Manager: Debug Info Support DLL 1.01

Description:
 Support DLL for FastMM. With this DLL available, FastMM will report debug info
 (unit name, line numbers, etc.) for stack traces.

Usage:
 1) To compile you will need the JCL library (http://sourceforge.net/projects/jcl/)
 2) Place in the same location as the replacement borlndmm.dll or your
 application's executable module.

Change log:
 Version 1.00 (9 July 2005):
  - Initial release.
 Version 1.01 (13 July 2005):
  - Added the option to use madExcept instead of the JCL Debug library. (Thanks
    to Martin Aignesberger.) 

}

{--------------------Start of options block-------------------------}

{Set the option below to use madExcept instead of the JCL Debug units.}
{.$define madExcept}

{--------------------End of options block-------------------------}

library FastMM_DebugInfo;

uses
  SysUtils, {$ifndef madExcept}JCLDebug{$else}madStackTrace{$endif};

{$R *.res}

{Gets the text debug info for the given address into APDebugInfo and sets the
 number of characters returned in ANumChars. On entry ANumChars must be the
 size of the buffer pointer to by APDebugInfo.}
{$ifndef madExcept}
procedure GetDebugInfoForAddress(AAddress: Pointer;
  APDebugInfo: PChar; var ANumChars: integer);
var
  LInfo: TJCLLocationInfo;
  LTempStr: string;
begin
  GetLocationInfo(AAddress, LInfo);
  {Build the result string}
  LTempStr := ' ';
  if LInfo.SourceName <> '' then
    LTempStr := LTempStr + '[' + LInfo.SourceName + ']';
  if LInfo.UnitName <> '' then
    LTempStr := LTempStr + '[' + LInfo.UnitName + ']';
  if LInfo.ProcedureName <> '' then
    LTempStr := LTempStr + '[' + LInfo.ProcedureName + ']';
  if LInfo.LineNumber <> 0 then
    LTempStr := LTempStr + '[' + IntToStr(LInfo.LineNumber) + ']';
  {Return the result}
  if ANumChars > length(LTempStr) then
    ANumChars := length(LTempStr)
  else
    Dec(ANumChars);
  StrLCopy(APDebugInfo, PChar(LTempStr), ANumChars);
end;
{$else}
procedure GetDebugInfoForAddress(AAddress: Pointer;
  APDebugInfo: PChar; var ANumChars: integer);
var
  LTempStr: string;
begin
  LTempStr := madStackTrace.StackAddrToStr(AAddress);
  if ANumChars > length(LTempStr) then
    ANumChars := length(LTempStr)
  else
    Dec(ANumChars);
  StrLCopy(APDebugInfo, PChar(LTempStr), ANumChars);
end;
{$endif}

exports GetDebugInfoForAddress;

begin
end.
