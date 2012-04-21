//<summary>Stuff common to the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2011, Primoz Gabrijelcic
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
///   Home              : http://otl.17slon.com
///   Support           : http://otl.17slon.com/forum/
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///     Web             : http://gp.17slon.com
///   Contributors      : GJ, Lee_Nover, scarre
///
///   Creation date     : 2011-08-31
///   Last modification : 2011-08-31
///   Version           : 1.0
///</para><para>
///   History:
///     1.22: 2011-08-31
///       - [Lee_Nover] SetThreadName implementation moved here. Disabled debug info for
///         the unit. That way, debugger doesn't stop on SetThreadName while 
///         single-stepping in another thread.
///</para></remarks>

unit OtlCommon.Utils;

{$DEBUGINFO OFF}

interface

procedure SetThreadName(const name: string);

implementation

uses Windows;

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
    threadNameInfo.FType := $1000;
    threadNameInfo.FName := PAnsiChar(ansiName);
    threadNameInfo.FThreadID := $FFFFFFFF;
    threadNameInfo.FFlags := 0;
    try
      RaiseException($406D1388, 0, SizeOf(threadNameInfo) div SizeOf(LongWord), @threadNameInfo);
    except {ignore} end;
  end;
end; { SetThreadName }

end.
