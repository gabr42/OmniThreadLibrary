// Include this file at most ONCE in your project to emit some extra
//  helpful hints relating to project options.
{$REGION 'Hint reporting directives'}
{$IFDEF HINT_REPORTING_ACTUAL}

  {$IFDEF DEBUG}
    {$Message Hint 'Config = DEBUG'}
  {$ENDIF}

  {$IFDEF RELEASE}
    {$Message Hint 'Config = RELEASE'}
  {$ENDIF}

  {$IF (not defined( DEBUG)) and (not defined( RELEASE))}
    {$Message Hint 'Config = neither DEBUG nor RELEASE'}
  {$ENDIF}

  {$IFDEF CPUINTEL}
    {$Message Hint 'CPU = INTEL'}
  {$ENDIF}

  {$IFDEF CPUARM}
    {$Message Hint 'CPU = ARM'}
  {$ENDIF}

  {$IFDEF CPU32BITS}
    {$Message Hint 'Architecture = 32-bit'}
  {$ENDIF}

  {$IFDEF CPU64BITS}
    {$Message Hint 'Architecture = 64-bit'}
  {$ENDIF}

  {$IFDEF USE_EMBARCADERO_TConditionVariableCS}
    {$Message Hint 'Relying on integrity of class TConditionVariableCS.'}
  {$ELSE}
    {$Message Hint 'Not using class TConditionVariableCS.'}
  {$ENDIF}

  {$IFDEF DONT_TRUST_ALIGNMENT_OF_MEMORY_MANAGERS}
    {$Message Hint 'Not trusting the alignment of object instances returned by memory manager allocations.'}
  {$ELSE}
    {$Message Hint 'Trusting the alignment of object instances returned by memory manager allocations.'}
  {$ENDIF}

  {$IFDEF USE_SLACKSPACE_ALIGNMENT}
    {$Message Hint 'Using slack-space alignment for AtomicXXX() operands.'}
  {$ELSE}
    {$Message Hint 'Using instrinsic alignment for AtomicXXX() operands.'}
  {$ENDIF}

  {$IFDEF AUTOREFCOUNT}
     {$Message Hint 'AUTOREFCOUNT compiler.'}
  {$ENDIF}

{$ENDIF HINT_REPORTING_ACTUAL}
{$ENDREGION}
