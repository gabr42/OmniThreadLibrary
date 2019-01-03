///<summary>Common compilation settings and conditional defines for the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2019 Primoz Gabrijelcic
///All rights reserved.
///
///Redistribution and use in source and binary forms, with or without modification,
///are permitted provided that the following conditions are met:
///- Redistributions of source code must retain the above copyright notice, this
///  list of conditions and the following disclaimer.
///- Redistributions in binary form must reproduce the above copyright notice,
///  this list of conditions and the following disclaimer in the documentation
///  and/or other materials provided with the distribution.
///- The name of the Primoz Gabrijelcic may not be used to endorse or promote
///  products derived from this software without specific prior written permission.
///
///THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
///ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
///WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
///DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
///ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
///(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
///LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
///ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
///(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
///SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
///</license>
///<remarks><para>
///   Author            : Primoz Gabrijelcic
///   Creation date     : 2010-07-01
///   Last modification : 2019-01-03
///   Version           : 2.0
///</para><para>
///   History:
///     2.01: 2018-05-28
///       - Introduced OTL_NameThreadHasStringParameter and OTL_HasVolatileAttribute.
///     1.06: 2019-01-03
///       - Introduced OTL_TypeInfoHasTypeData.
///     1.05: 2018-05-26
///       - Introduced OTL_NameThreadHasStringParameter and OTL_CanInlineOperators.
///       - OTL_StrPasInAnsiStrings moved from XE5 to XE4 section.
///     2.0: 2018-05-12
///       - Removed defines associated with Delphi 2007 do XE as these versions are
///         no longer supported.
///     1.04: 2012-09-23
///       - Introduced OTL_HasSystemTypes and OTL_LongGetMethodInfo.
///     1.03: 2011-07-23
///       - Assertions must be always enabled.
///     1.02: 2010-12-02
///       - Removed OTL_ParallelAggregate.
///     1.01: 2010-07-25
///       - Introduced OTL_ParallelAggregate.
///     1.0: 2010-07-01
///       - Released.
///</para></remarks>

// Defines below this point should not be changed. Changing them could break OmniThreadLibrary.

{$IFNDEF CONDITIONALEXPRESSIONS}
'OmniThreadLibrary requires Delphi XE2 or newer'
{$ENDIF}

{$IF CompilerVersion >= 26}{$LEGACYIFEND ON}{$IFEND}

{$IF CompilerVersion < 23}
{$MESSAGE ERROR 'OmniThreadLibrary requires Delphi XE2 or newer'}
{$IFEND}

{$ALIGN 8}
{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$MINENUMSIZE 1}
{$OPENSTRINGS ON}
{$OVERFLOWCHECKS OFF}
{$TYPEDADDRESS OFF}
{$ASSERTIONS ON}
{$TYPEINFO OFF}

{$IF CompilerVersion = 23} //DXE2
  {$DEFINE OTL_ForceThreadTracking} //passing False to TSpinLock.Create causes problems on XE2
{$IFEND}

{$IF CompilerVersion >= 24} //DXE3
  {$DEFINE OTL_LongGetMethodInfo}
  {$DEFINE OTL_CountdownHasSpinCount}
  {$DEFINE OTL_NameThreadHasStringParameter}
  {$DEFINE OTL_TypeInfoHasTypeData}
{$IFEND}

{$IF CompilerVersion >= 25} //DXE4
  {$DEFINE OTL_StrPasInAnsiStrings}
  {$DEFINE OTL_HasVolatileAttribute}
{$IFEND}

{$IF CompilerVersion >= 28} //DX7
  {$DEFINE OTL_FixedGenericIncompletelyDefined}
{$IFEND}

{$IF CompilerVersion >= 29} //DXE8
  {$DEFINE OTL_MobileSupport}
  {$ZEROBASEDSTRINGS OFF}
{$IFEND}

{$IF CompilerVersion >= 30} //Seattle
  {$DEFINE OTL_CanInlineOperators}
{$IFEND}

{$IF CompilerVersion >= 32} //Tokyo
  {$DEFINE OTL_HasForceQueue}
{$IFEND}

{$IFNDEF MSWINDOWS}
{$IFNDEF OTL_MobileSupport}
{$MESSAGE ERROR 'Only Windows compilation is supported on this Delphi version (at least XE8 is required for non-Windows platforms.'}
{$ENDIF ~OTL_MobileSupport}
{$ENDIF ~MSWINDOWS}

{$IFDEF MSWINDOWS}
  {$DEFINE OTL_HaveCmpx16b}
{$ELSE}
  //We don't have a platform-independant way of using cmpx8b/cmpx16b
  //(5-parameter OtlSync.CAS) so we can't use bus-locking on non-Windows targets.
  {$UNDEF OTL_HaveCmpx16b}
{$ENDIF ~MSWINDOWS}

