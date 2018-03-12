///<summary>Stuff common to the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2018, Primoz Gabrijelcic
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
///   Home              : http://www.omnithreadlibrary.com
///   Support           : https://plus.google.com/communities/112307748950248514961
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///   Contributors      : GJ, Lee_Nover, scarre, Sean B. Durkin
///
///   Creation date     : 2011-08-31
///   Last modification : 2018-02-26
///   Version           : 1.0b
///</para><para>
///   History:
///     1.0b: 2018-02-26
///       - Semantics of OTL_DontSetThreadName was reversed.
///     1.0a: 2017-11-28
///       - Did not include OtlOptions.inc
///     1.0: 2011-08-31
///       - [Lee_Nover] SetThreadName implementation moved here. Disabled debug info for
///         the unit. That way, debugger doesn't stop on SetThreadName while 
///         single-stepping in another thread.
///</para></remarks>

unit OtlCommon.Utils;

{$I OtlOptions.inc}
{$DEBUGINFO OFF}

interface

procedure SetThreadName(const name: string);

implementation

{$IFDEF OTL_HasNameThreadForDebugging}
uses
  Classes;
{$ELSE ~OTL_HasNameThreadForDebugging}
{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF MSWINDOWS}
{$ENDIF ~OTL_HasNameThreadForDebugging}

threadvar
  LastThreadName: string[255];

{$IFDEF OTL_DontSetThreadName}
procedure SetThreadName(const name: string);
begin
  // do nothing
end; { SetThreadName }
{$ELSE}

{$IFDEF OTL_HasNameThreadForDebugging}

procedure SetThreadName(const name: string);
var
  ansiName: AnsiString;
begin
  ansiName := AnsiString(name);
  if ansiName = LastThreadName then
    Exit;

  TThread.NameThreadForDebugging(string(name));
  LastThreadName := ansiName;
end; { SetThreadName }

{$ELSE ~OTL_HasNameThreadForDebugging}
{$IFDEF MSWINDOWS}

procedure SetThreadName(const name: string);
type
  TThreadNameInfo = record
    FType    : LongWord; // must be 0x1000
    FName    : PAnsiChar;// pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags   : LongWord; // reserved for future use, must be zero
  end; { TThreadNameInfo }
var
  ansiName      : AnsiString;
  threadNameInfo: TThreadNameInfo;
begin
  if DebugHook <> 0 then begin
    ansiName := AnsiString(name);
    if ansiName = LastThreadName then
      Exit;
    threadNameInfo.FType := $1000;
    threadNameInfo.FName := PAnsiChar(ansiName);
    threadNameInfo.FThreadID := $FFFFFFFF;
    threadNameInfo.FFlags := 0;
    try
      RaiseException($406D1388, 0, SizeOf(threadNameInfo) div SizeOf(LongWord), @threadNameInfo);
    except {ignore} end;
    LastThreadName := ansiName;
  end;
end; { SetThreadName }

{$ELSE ~MSWINDOWS}

procedure SetThreadName(const name: string);
begin
end; { SetThreadName }

{$ENDIF ~MSWINDOWS}
{$ENDIF ~OTL_HasNameThreadForDebugging}
{$ENDIF ~OTL_DontSetThreadName}

end.
