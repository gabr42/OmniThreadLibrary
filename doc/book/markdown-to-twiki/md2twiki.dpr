{$REGION 'Documentation'}
///	<summary>
///	  Markdown syntax to TWiki syntax converter.
///	</summary>
{$ENDREGION}
program md2twiki;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.RegularExpressions,
  Classes,
  DSiWin32,
  GpString,
  GpStuff,
  GpStreams,
  GpTextStream;

type
  TReplacer = class
    function ReplaceLink(const match: TMatch): string;
  end;

var
  GReplacer: TReplacer;

function TReplacer.ReplaceLink(const match: TMatch): string;
begin
  Result := Format('[[book:%s|%s]]', [
    StringReplace(match.Groups[2].Value, '-', ':', [rfReplaceAll]),
    match.Groups[1].Value]);
end;

procedure Usage;
begin
  Writeln('Markdown to TWiki syntax converter');
  Writeln('Usage: ms2twiki <file mask> <target folder>');
  Halt;
end;

function ConvertPara(var para: string): string;
begin
  Result := para;

  Result := TRegEx.Replace(Result, '\!\[\]\(images\/(.*?)/(.*?)\)', '{{ :book:\1:\2?nolink|}}');
  Result := TRegEx.Replace(Result, '\*(.*)\*', '//\1//', [roMultiLine]);
  Result := TRegEx.Replace(Result, '`(.*?)`', '''''\1''''');
  Result := TRegEx.Replace(Result, '^### (.*)$', '====== \1 ======');
  Result := TRegEx.Replace(Result, '\[(.*?)\]\(#(.*?)\)', GReplacer.ReplaceLink);
  Result := StringReplace(Result, 'â€'#$201C, '-', [rfReplaceAll]);
  Result := StringReplace(Result, 'â€™', '''', [rfReplaceAll]);
  Result := StringReplace(Result, 'â€¦', '...', [rfReplaceAll]);
  para := '';
end;

procedure Convert(const mdFile, twFile: string);
var
  codeTag  : integer;
  mdFileExp: string;
  mdStr    : TGpTextStream;
  para     : string;
  srcLine  : string;
  twFileExp: string;
  twStr    : TGpTextStream;

  procedure AppendLine;
  begin
    if para <> '' then
      para := para + #13#10;
    para := para + srcLine;
  end;

  procedure FlushPara;
  begin
    if para <> '' then
      twStr.Writeln(ConvertPara(para));
    para := '';
  end;

begin { Convert }
  twFileExp := ExpandFileName(twFile);
  mdFileExp := ExpandFileName(mdFile);
  ForceDirectories(ExtractFilePath(twFileExp));
  if FileExists(twFileExp) and
     (DSiGetFileTime(mdFileExp, ftLastModification) <= DSiGetFileTime(twFileExp, ftLastModification))
  then
    Exit;
  Writeln(mdFile);

  mdStr := TGpTextStream.Create(AutoDestroyStream(SafeCreateFileStream(mdFileExp, fmOpenRead)).Stream, tsaccRead, []);
  AutoDestroyStream(mdStr);
  twStr := TGpTextStream.Create(AutoDestroyStream(SafeCreateFileStream(twFileExp, fmCreate)).Stream, tsaccWrite, []);
  AutoDestroyStream(twStr);

  para := '';
  codeTag := 0;
  while not mdStr.Eof do begin
    srcLine := mdStr.Readln;
    if srcLine = '{:lang="pascal"}' then begin
      FlushPara;
      codeTag := 1;
    end
    else if srcLine = '~~~~~~~~~~~~~~~' then begin
      para := '';
      if codeTag = 1 then
        twStr.Writeln('<code Delphi>')
      else begin
        twStr.Writeln('</code>');
        twStr.Writeln;
      end;
      codeTag := 3 - codeTag;
    end
    else if codeTag = 2 then
      twStr.Writeln(srcLine)
    else if Trim(srcLine) = '' then begin
      AppendLine;
      FlushPara;
    end
    else
      AppendLine;
  end;
  FlushPara;
end;

var
  baseMdFolder: string;
  baseTwFolder: string;
  mdFile      : string;

begin
  try
    if ParamCount <> 2 then
      Usage;

    GReplacer := TReplacer.Create;
    try
      baseMdFolder := ExtractFilePath(ParamStr(1));
      baseTwFolder := IncludeTrailingPathDelimiter(ParamStr(2));
      for mdFile in EnumFiles(ParamStr(1), 0, true, true) do
        if Last(mdFile, 1) <> '\' then
          Convert(mdFile, baseTwFolder + ButFirst(mdFile, Length(baseMdFolder)));
    finally FreeAndNil(GReplacer); end;

    if DebugHook <> 0 then
      Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
