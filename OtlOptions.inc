///<summary>Common compilation settings and conditional defines for the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2010 Primoz Gabrijelcic
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
///   Last modification : 2012-09-23
///   Version           : 1.04
///</para><para>
///   History:
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

// See the Configuration block below to configure your build process.

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

{$IF CompilerVersion >= 26}{$LEGACYIFEND ON}{$IFEND}

{$IF CompilerVersion < 19} //D2007
  {$DEFINE OTL_NeedsWindowsAPIs}
{$IFEND}

{$IF CompilerVersion >= 20} //D2009
  {$DEFINE OTL_Anonymous}
  {$DEFINE OTL_Generics}
  {$DEFINE OTL_HasInterlockedCompareExchangePointer}
  {$DEFINE OTL_NUMASupport}
{$IFEND}

{$IF CompilerVersion >= 21} //D2010
  {$DEFINE OTL_ERTTI}
  {$DEFINE OTL_DeprecatedResume}
  {$DEFINE OTL_KnowsParamCount}
  {$DEFINE OTL_HasTkPointer}
  {$DEFINE OTL_HasArrayOfT}
  {$DEFINE OTL_HasNameThreadForDebugging}
{$IFEND}

{$IF CompilerVersion >= 22} //DXE
  {$DEFINE OTL_TOmniValueImplicitDateTime}
  {$DEFINE OTL_HasThreadID}
  {$DEFINE OTL_HasTInterlocked}
  {$DEFINE OTL_GoodGenerics}
  {$DEFINE OTL_HasTThreadYield}
{$IFEND}

{$IF CompilerVersion >= 23} //DXE2
  {$DEFINE OTL_Supports64Bit}
  {$DEFINE OTL_HasSystemTypes}
  {$DEFINE OTL_HasCorrectNativeInt}
{$IFEND}

{$IF CompilerVersion >= 24} //DXE3
  {$DEFINE OTL_LongGetMethodInfo}
{$IFEND}

{$IF CompilerVersion >= 26} //DXE5
  {$DEFINE OTL_StrPasInAnsiStrings}
{$IFEND}

{$IF CompilerVersion >= 28} //DX7
  {$DEFINE OTL_FixedGenericIncompletelyDefined}
{$IFEND}

{$IF CompilerVersion >= 29} //DXE8
  {$DEFINE OTL_MobileSupport}
{$IFEND}

{$IFNDEF MSWINDOWS}
{$IFNDEF OTL_MobileSupport}
{$MESSAGE ERROR 'Only Windows compilation is supported on this Delphi version (at least XE8 is required for non-Windows platforms.'}
{$ENDIF ~OTL_MobileSupport}
{$ENDIF ~MSWINDOWS}

