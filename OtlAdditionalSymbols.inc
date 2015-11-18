//        OmniThread Library Common Source Include File.
//        ==============================================
//
// Contents:
//  1. Configuration block
//  2. Symbol computation block
//  3. Symbol explaination block.


// ============ CONFIGURATION BLOCK ============================================
// =============================================================================
// Define for undefine the following symbols as required.
//
//  Symbol                                    |  Significance                                | Default and recommended value  | Why?
//  ------------------------------------------+----------------------------------------------+--------------------------------+--------------------------------------------------------------------
//  USE_EMBARCADERO_TConditionVariableCS      |  Define to allow the library to leverage     | UNDEFINE                       | There are no published unit tests for TConditionVariableCS.
//                                            |   the RTL TConditionVariableCS class.        |                                | At this stage, given the opaqueness of the RTL code for this class,
//                                            |  Undefine to work-around it with other       |                                | and the show-stopper defects in the Delphi XE7 parallel RTL units,
//                                            |   primitives.                                |                                | I think it is safest to leave undefined. Perhaps with a later
//                                            |                                              |                                | compiler and some supporting unit tests, this could be turned on.
//  DONT_TRUST_ALIGNMENT_OF_MEMORY_MANAGERS   |  Define if it cannot be guarenteed that      | DEFINE                         | This needs to be defined until either: (1) Embarcadero publishes
//                                            |   object instance memory spaces, as returned |                                | a statement about what the standard memory manager is guarenteed to
//                                            |   by the memory manager, will be aligned to  |                                | do in relation to object instance alignment (and to what range of
//                                            |   the machine architecture (32-bit or 64-bit)|                                | compilers this statement applies to); or (2) There is strong unit
//                                            |  Undefine if this can be guarenteed.         |                                | testing evidence that the standard memory manager does align object
//                                            |                                              |                                | instances.
//                                            |                                              |                                | compiler and some supporting unit tests, this could be turned on.
//  DONT_TRUST_EMBARCADERO_THREADING_LIBRARY  |  Define if to avoid using the Embarcadero    | DEFINE                         |
//                                            |   threading library classes (in unit         |                                |
//                                            |   System.SyncObjs) ...                       |                                |
//                                            |     TCountdownEvent                          |                                |
//                                            |     TLightweightEvent                        |                                |
//                                            |     TConditionVariableCS.                    |                                |
//                                            |  Defining this conditional trumps the        |                                |
//                                            |   USE_EMBARCADERO_TConditionVariableCS cond'l|                                |
//                                            |                                              |                                |
//                                            |                                              |                                |
//  HINT_REPORTING                            |  Define you want the state of conditional    | DEFINE                         | Because it's helpful.
//                                            |   symbols to be reported as compiler hints.  |                                |
//                                            |  Only in force for DEBUG configurations.     |                                |
//  ------------------------------------------+----------------------------------------------+--------------------------------+--------------------------------------------------------------------
//
   {$undef  USE_EMBARCADERO_TConditionVariableCS}
   {$define DONT_TRUST_ALIGNMENT_OF_MEMORY_MANAGERS}
   {$define HINT_REPORTING}
   {$define DONT_TRUST_EMBARCADERO_THREADING_LIBRARY}
//
// ============ END CONFIGURATION BLOCK ========================================
// =============================================================================







// ============ SYMBOL COMPUTATION BLOCK =======================================
// =============================================================================
// Do not change. Changes only need to be made in the configuration block.
//
// Define symbol USE_EMBARCADERO_TConditionVariableCS if and when
//  there are published unit tests which give confidence in the robustness
//  of Embardadero's TConditionVariableCS class for all platforms and all compilers (XE7+).
//  Until then, leave this symbol undefined.

  {$ifdef CONDITIONALEXPRESSIONS}
    {$if CompilerVersion >= 25.0}
      {$LEGACYIFEND OFF}
    {$ifend}
  {$endif}

  {$IFDEF CPU386} {$define CPUINTEL} {$ENDIF}
  {$IFDEF CPUX64} {$define CPUINTEL} {$ENDIF}

  {$IF (defined( CPU32BITS) and defined( CPU64BITS)) or ((not defined( CPU32BITS)) and (not defined( CPU64BITS)))}
    {$undef CPU32BITS}
    {$undef CPU64BITS}
    {$IF SizeOf( pointer) = 4}
      {$define  CPU32BITS}
    {$ENDIF}
    {$IF SizeOf( pointer) = 8}
      {$define   CPU64BITS}
    {$ENDIF}
  {$ENDIF}

{$IFDEF CPU64BITS}
  {$ALIGN 8}
{$ELSE}
  {$ALIGN 4}
{$ENDIF}

  {$IF (defined( CPUARM) and defined( CPUINTEL)) or ((not defined( CPUARM)) and (not defined( CPUINTEL)))}
    {$Message Fatal 'CPU = ?'}
  {$ENDIF}

  {$IF (defined( CPU32BITS) and defined( CPU64BITS)) or ((not defined( CPU32BITS)) and (not defined( CPU64BITS)))}
    {$Message Fatal 'Architecture = ?'}
  {$ENDIF}

{$IFDEF DONT_TRUST_EMBARCADERO_THREADING_LIBRARY}
  {$undef USE_EMBARCADERO_TConditionVariableCS}
{$ENDIF}

{$IFDEF CPUARM}
  {$IFDEF DONT_TRUST_ALIGNMENT_OF_MEMORY_MANAGERS}
    {$define USE_SLACKSPACE_ALIGNMENT}
    // ARM CPU's require that the operands to AtomicXXX() and TInterlocked.XXX()
    //  functions be aligned within cache lines. If memory managers cannot be
    //  trusted to align object instances, then we cannot rely on the $ALIGN
    //  directive to ensure that the object data members (as AtomicXXX() operands
    //  meet this cache line requirement. "Cache line" refers to the L1 CPU cache
    //  in the ARM and INTEL processors.
  {$ELSE}
    {$undef USE_SLACKSPACE_ALIGNMENT}
    // If $ALIGN works to create machine-architecture aligned data members,
    //  we can be sure that the data members do not cross cashe lines.
  {$ENDIF}
{$ENDIF}

{$IFDEF CPUINTEL}
  {$undef USE_SLACKSPACE_ALIGNMENT}
  // Intel processors do not require operands to the AtomicXXX() and TInterlocked.XXX()
  //  functions to be aligned in any way, however, data-size alignment (32-bit
  //  alignment for integers; 64-bit alignment for int64) does help performance.
{$ENDIF}

{$IFDEF HINT_REPORTING}
  {$IFDEF DEBUG}
    {$define HINT_REPORTING_ACTUAL}
  {$ELSE}
    {$undef HINT_REPORTING_ACTUAL}
  {$ENDIF}
{$ELSE}
  {$undef HINT_REPORTING_ACTUAL}
{$ENDIF}

// ============ END SYMBOL COMPUTATION BLOCK ===================================
// =============================================================================














// ============ SYMBOL EXPLAINATION BLOCK ======================================
// =============================================================================
//
//  Symbol                                    |  Significance
//  ------------------------------------------+----------------------------------------------
//  USE_EMBARCADERO_TConditionVariableCS      |  Refer to the CONFIGURATION BLOCK.
//  DONT_TRUST_ALIGNMENT_OF_MEMORY_MANAGERS   |  Refer to the CONFIGURATION BLOCK.
//  CPU32BITS                                 |  Refer wiki page (below).
//  CPU64BITS                                 |  Refer wiki page (below).
//  CPUARM                                    |  Refer wiki page (below).
//  CPUINTEL                                  |  Refer wiki page (below).
//  USE_SLACKSPACE_ALIGNMENT                  |  Use the slack-space technique to force data
//                                            |   (which might be subject to AtomicXXX()
//                                            |   functions) into cache line alignment,
//                                            |   not trusting the memory manager to ensure
//                                            |   that object instances are machine aligned.
//  HINT_REPORTING                            |  Refer to the CONFIGURATION BLOCK.
//  HINT_REPORTING_ACTUAL                     |  Whether or not to do hint reporting in the current
//                                            |   project configuration.
//  ------------------------------------------+----------------------------------------------
//
//  The wiki page for standard conditionals is located at ...
//    http://docwiki.embarcadero.com/RADStudio/XE8/en/Conditional_compilation_(Delphi)
//
// ============ END SYMBOL EXPLAINATION BLOCK ==================================
// =============================================================================

