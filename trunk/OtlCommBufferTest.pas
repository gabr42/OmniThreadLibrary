///<summary>Communication ring buffer unit test. Part of the OmniThreadLibrary project.</summary>
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2008, Primoz Gabrijelcic
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
///   Contributors      : GJ, Lee_Nover
///
///   Creation date     : 2008-07-12
///   Last modification : 2010-07-01
///   Version           : 1.01
///</para><para>
///   History:
///     1.01: 2010-07-01
///       - Includes OTLOptions.inc.
///     1.0: 2008-08-26
///       - First official release.
///</para></remarks>

unit OtlCommBufferTest;

{$I OtlOptions.inc}

interface

uses
  OtlComm;

implementation

uses
  SysUtils,
  OtlCommon;

procedure RingBufferTest;
var
  i  : integer;
  msg: TOmniMessage;
  rb : TOmniMessageQueue;
begin
  rb := TOmniMessageQueue.Create(100);
  try
    if not rb.IsEmpty then
      raise Exception.Create('Buffer is not empty when created');
    for i := 1 to 100 do begin
      msg.MsgID := i;
      msg.MsgData := -i;
      if not rb.Enqueue(msg) then
        raise Exception.CreateFmt('Enqueue failed on element %d', [msg.MsgID]);
    end;
    for i := 1 to 100 do begin
      if rb.IsEmpty then
        raise Exception.CreateFmt('Buffer is empty on element %d', [i]);
      msg := rb.Dequeue;
      if (msg.MsgID <> i) or (msg.MsgData.AsInteger <> -i) then
        raise Exception.CreateFmt('Retrieved (%d, %d), expected (%d, %d)',
          [msg.MsgID, msg.MsgData.AsInteger, i, -i]);
    end;
    if not rb.IsEmpty then
      raise Exception.Create('Buffer is not empty at the end');
   finally FreeAndNil(rb); end;
end; { RingBufferTest }

initialization
  RingBufferTest;
end.
