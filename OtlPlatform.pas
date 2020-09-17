///<summary>Platform compatibility layer for the OmniThreadLibrary project.</summary>
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
///   Support           : https://en.delphipraxis.net/forum/32-omnithreadlibrary/
///   Author            : Primoz Gabrijelcic
///     E-Mail          : primoz@gabrijelcic.org
///     Blog            : http://thedelphigeek.com
///   Creation date     : 2018-05-16
///   Last modification : 2018-05-16
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2018-05-16
///       - Released.
///</para></remarks>

unit OtlPlatform;

{$I OtlOptions.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  DSiWin32,
  {$ENDIF MSWINDOWS}
  {$IFDEF OTL_MobileSupport}
  System.Classes,
  {$ENDIF OTL_MobileSupport}
  System.Diagnostics,
  SysUtils;

type
  TTimeSource = record
  private
    FStopwatch: TStopwatch;
  public
    class function Create: TTimeSource; static;
    function  Elapsed_ms(startTime_ms: int64): int64; inline;
    function  HasElapsed(startTime_ms, timeout_ms: int64): boolean;
    function  Timestamp_ms: int64; inline;
  end; { TTimeSource }
  PTimeSource = ^TTimeSource;

  TPlatform = record
  private
    class function  GetThreadAffinity: string; static;
    class function  GetThreadID: TThreadID; static;
    class procedure SetThreadAffinity(const value: string); static;
  public
    class property ThreadAffinity: string read GetThreadAffinity write SetThreadAffinity;
    class property ThreadID: TThreadID read GetThreadID;
  end; { TPlatform }

function Time: PTimeSource; inline;

// must be global for inlining
var
  GTimeSource: TTimeSource;

implementation

{ exports }

function Time: PTimeSource;
begin
  Result := @GTimeSource;
end; { Time }

{ TTimeSource }

class function TTimeSource.Create: TTimeSource;
begin
  Result.FStopwatch := TStopwatch.StartNew;
end; { TTimeSource.Create }

function TTimeSource.Timestamp_ms: int64;
begin
  {$IFDEF MSWINDOWS}
  if FStopwatch.IsHighResolution then
    Result := Round(FStopwatch.ElapsedTicks / FStopwatch.Frequency * 1000)
  else
    Result := DSiTimeGetTime64;
  {$ELSE}
  Result := Round(FStopwatch.ElapsedTicks / FStopwatch.Frequency * 1000)
  {$ENDIF ~MSWINDOWS}
end; { TTimeSource.Timestamp_ms }

function TTimeSource.Elapsed_ms(startTime_ms: int64): int64;
begin
  Result := Timestamp_ms - startTime_ms;
end; { TTimeSource.Elapsed_ms }

function TTimeSource.HasElapsed(startTime_ms, timeout_ms: int64): boolean;
begin
  if timeout_ms <= 0 then
    Result := true
  else if timeout_ms = INFINITE then
    Result := false
  else
    Result := (Elapsed_ms(startTime_ms) >= timeout_ms);
end; { TTimeSource.HasElapsed }

{ TPlatform }

class function TPlatform.GetThreadAffinity: string;
{$IFNDEF MSWINDOWS}
const
  CPUIDs = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz@$';
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  Result := DSiGetThreadAffinity;
  {$ELSE}
  Result := Copy(CPUIDs, 1, TThread.ProcessorCount);
  {$ENDIF ~MSWINDOWS}
end; { TPlatform.GetThreadAffinity }

class function TPlatform.GetThreadID: TThreadID;
begin
  Result := {$IFDEF OTL_MobileSupport}TThread.CurrentThread.ThreadID{$ELSE}GetCurrentThreadID{$ENDIF};
end; { TPlatform.GetThreadID }

class procedure TPlatform.SetThreadAffinity(const value: string);
begin
  {$IFDEF MSWINDOWS}
  DSiSetThreadAffinity(value);
  {$ENDIF MSWINDOWS}
end; { TPlatform.SetThreadAffinity }

initialization
  GTimeSource := TTimeSource.Create;
end.
