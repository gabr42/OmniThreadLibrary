(*:DCP-based encryption for GpStreams streams.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2020, Primoz Gabrijelcic
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
- Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
- Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
- The name of the Primoz Gabrijelcic may not be used to endorse or promote
  products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   Author            : Primoz Gabrijelcic
   Creation date     : 2021-02-26
   Last modification : 2021-02-26
   Version           : 0.1
</pre>*)(*
   History:
     0.1: 2021-02-26
       - Created
*)

unit GpStreams.DCP;

interface

uses
  GpStreams;

type
  TGpTwofishEncryptedStream = class(TGpBaseEncryptedStream)
  protected
    procedure DoneCypher; override;
    procedure InitCypher(const key; keySize: integer; nonce: uint64); override;
  end; { TGpTwofishEncryptedStream }

implementation

{ TGpTwofishEncryptedStream }

procedure TGpTwofishEncryptedStream.DoneCypher;
begin
  inherited;

end; { TGpTwofishEncryptedStream.DoneCypher }

procedure TGpTwofishEncryptedStream.InitCypher(const key; keySize: integer;
  nonce: uint64);
begin
  inherited;

end; { TGpTwofishEncryptedStream.InitCypher }

end.
