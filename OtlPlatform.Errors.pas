unit OtlPlatform.Errors;

{$I OtlOptions.inc}

interface
uses System.SysUtils;

type

  TParallelException = class( Exception)
    public
      FErrCode: integer;
      constructor Create( ErrCode: integer; const Args: array of const); overload;
      constructor Create( ErrCode: integer);                             overload;
    end;

const
  EIsSignalledNotSupported = 1;
  ESignalCountUpDownRange  = 2;
  ECannotDestroySynchroFactory = 3;
  ECompositeNeedsOneFactor = 4;
  EOnlyModularCombinable = 5;
  ECompositeSynchroMixedBag = 6;

  ParallelExceptionMessages: array[ 1 .. 10 ] of record ErrCode: integer; MsgFmt: string end = (
    (ErrCode: EIsSignalledNotSupported; MsgFmt:'isSignalled() not supported on this class.'),
    (ErrCode: ESignalCountUpDownRange; MsgFmt:'Signal() out of range for TCountUp/Down'),
    (ErrCode: ECannotDestroySynchroFactory; MsgFmt:'Cannot destroy TSynchroFactory while there are still allocated synchros.'),
    (ErrCode: ECompositeNeedsOneFactor; MsgFmt:'TCompositeSynchro needs at least one factor.'),
    (ErrCode: EOnlyModularCombinable; MsgFmt:'TCompositeSynchro - all factors must be modular sychros.'),
    (ErrCode: ECompositeSynchroMixedBag; MsgFmt:'TConditionEvent created with invalid mixed bag of direct and indirect members.'),
    (ErrCode: 0; MsgFmt:''),
    (ErrCode: 0; MsgFmt:''),
    (ErrCode: 0; MsgFmt:''),
    (ErrCode: 0; MsgFmt:'')
    );

implementation






constructor TParallelException.Create(
  ErrCode: integer; const Args: array of const);
var
  I: integer;
  MsgFmt: string;
begin
  for I := Low( ParallelExceptionMessages) to High( ParallelExceptionMessages) do
    begin
    if ParallelExceptionMessages[I].ErrCode <> ErrCode then continue;
    MsgFmt := ParallelExceptionMessages[I].MsgFmt;
    break
    end;
  if MsgFmt = '' then
      MsgFmt := Format( 'Unknown error (%d)', [ErrCode])
    else if MsgFmt.Contains('%') and (Length( Args) > 0) then
      MsgFmt := Format( MsgFmt, Args);
  FErrCode := ErrCode;
  inherited Create( MsgFmt)
end;

constructor TParallelException.Create( ErrCode: integer);
begin
  Create( ErrCode, [])
end;

end.
