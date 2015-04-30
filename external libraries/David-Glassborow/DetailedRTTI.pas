unit DetailedRTTI;

  // Some functions for playing with rich RTTI in objects
  // Free to use in anyway, at your own risk.
  // David Glassborow [http://davidglassborow.blogspot.com/2006/05/class-rtti.html]

interface

uses
  TypInfo,
  ObjAuto;

  {$IFDEF ConditionalExpressions}
    {$IF RTLVersion >= 18} {$DEFINE HAS_RECORDHELPERS} {$IFEND}
    {$IF RTLVersion >= 24} {$DEFINE NO_SHORT_GETMETHODINFO} {$IFEND}
  {$ENDIF}

{$IFDEF HAS_RECORDHELPERS}
type

  TParamInfoHelper = record helper for TParamInfo
  public
   function AsString: string;
   function NextParam: PParamInfo;
  end;

  TReturnInfoHelper = record helper for TReturnInfo
  public
   function AsString: string;
  end;

  TMethodInfoHeaderHelper = record helper for TMethodInfoHeader
  private
    function GetReturnInfo: PReturnInfo;
  public
    property ReturnInfo: PReturnInfo read GetReturnInfo;
  end;

  TObjectHelper = class helper for TObject
  public
    function RTTIMethodsAsString: string;
  end;

  function DescriptionOfMethod( Obj: TObject; MethodName: string ): string;

  {$ENDIF HAS_RECORDHELPERS}
implementation

uses
  SysUtils;

const
  SHORT_LEN = sizeof(ShortString) - 1;

{$IFDEF HAS_RECORDHELPERS}

function DescriptionOfMethod( Obj: TObject; MethodName: string ): string;
var
  header: PMethodInfoHeader;
  headerEnd: Pointer;
  Params, Param: PParamInfo;
  returnInfo: PReturnInfo;
begin
  header := ObjAuto.GetMethodInfo( Obj, {$IFNDEF NO_SHORT_GETMETHODINFO}ShortString{$ENDIF}(MethodName) );
  // Check the length is greater than just that of the name
  if Header.Len <= SizeOf(TMethodInfoHeader) - SHORT_LEN + Length(Header.Name) then
  begin
    Result := 'No rich RTTI';
    exit;
  end;

  headerEnd := Pointer(Integer(header) + header^.Len);
  // Get a pointer to the param info
  Params := PParamInfo(Integer(header) + SizeOf(header^) - SHORT_LEN + SizeOf(TReturnInfo) + Length(header^.Name));
  // Loop over the parameters
  Param := Params;
  Result := '';
  while Integer(Param) < Integer(headerEnd) do
  begin
    Result := Result + Param.AsString + '; ';
    // Find next param
    Param := Param.NextParam;
  end;
  Delete( Result, Length(Result)-1,2 );

  // Now the return
  returnInfo := header.ReturnInfo;
  if assigned( returnInfo.ReturnType ) then
    Result := Format( 'function %s( %s ): %s', [ MethodName, Result, returnInfo.AsString ] )
  else
    Result := Format( 'procedure %s( %s )%s', [ MethodName, Result, returnInfo.AsString ] );
end;

{ TParamInfoHelper }

function TParamInfoHelper.AsString: string;
begin
  Result := '';
  if pfResult in Flags then exit;         // Seems to be extra info about the return function, not sure what it means
  Result := string(Name) + ': ' + string(ParamType^.Name);
  if pfVar in self.Flags then             // Should really handle the other flags here
    Result := 'var ' + Result;
end;

function TParamInfoHelper.NextParam: PParamInfo;
begin
  Result := PParamInfo(Integer(@self) + SizeOf(self) - SHORT_LEN + Length(Name));
  {$IF CompilerVersion >= 21}
  // Skip attribute data
  Result := PParamInfo(cardinal(Result) + PWord(Result)^);
  {$IFEND}
end;

{ TMethodInfoHeaderHelper }

function TMethodInfoHeaderHelper.GetReturnInfo: PReturnInfo;
begin
  Result := PReturnInfo(Integer(@self) + SizeOf(TMethodInfoHeader) - SHORT_LEN + Length(Name));
end;

{ TReturnInfoHelper }

function TReturnInfoHelper.AsString: string;
var
  c: string;
begin
  Assert( Version = 1, 'Version of ReturnInfo incorrect' );
  if assigned( ReturnType ) then
    Result := string(ReturnType^.Name);
  Result := Result + ';';
  case CallingConvention of
    {$IF CompilerVersion >= 26}TCallConv.{$IFEND}ccCdecl: c := 'cdecl';
    {$IF CompilerVersion >= 26}TCallConv.{$IFEND}ccPascal: c := 'pascal';
    {$IF CompilerVersion >= 26}TCallConv.{$IFEND}ccStdCall: c := 'stdcall';
    {$IF CompilerVersion >= 26}TCallConv.{$IFEND}ccSafeCall: c := 'safecall';
    else c := '';
  end;
  if c <> '' then Result := Result + ' ' + c + ';';
end;

{ TObjectHelper }

function TObjectHelper.RTTIMethodsAsString: string;
var
  MethodInfo: Pointer;
  Count: Integer;
  method: PMethodInfoHeader;
  i: Integer;
begin
    MethodInfo := PPointer(Integer(PPointer(self)^) + vmtMethodTable)^;
    if MethodInfo <> nil then
    begin
      // Scan method and get string about each
      Count := PWord(MethodInfo)^;
      Inc({$IFDEF CPUX64}NativeInt{$ELSE}integer{$ENDIF}(MethodInfo), 2);
      method := MethodInfo;
      for i := 0 to Count - 1 do
      begin
        Result := Result + DescriptionOfMethod(self, string(method.Name)) + sLineBreak;
        Inc({$IFDEF CPUX64}NativeInt{$ELSE}Integer{$ENDIF}(method), PMethodInfoHeader(method)^.Len); // Get next method
      end;
    end;
end;

{$ENDIF HAS_RECORDHELPERS} 
end.
