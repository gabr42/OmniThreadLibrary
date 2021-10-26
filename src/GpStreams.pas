(*:TStream descendants, TStream compatible classes and TStream helpers.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2021, Primoz Gabrijelcic
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
   Creation date     : 2006-09-21
   Last modification : 2021-02-26
   Version           : 2.0
</pre>*)(*
   History:
     2.0: 2021-02-26
       - Reparented all stream-wrapping streams on the new TGpStreamWrapper class.
     1.57: 2020-11-30
       - TGpStreamWindow checks validity of configuration parameters.
     1.56a: 2020-11-16
       - Fixed: WriteToFile did not close internal THandleStream.
     1.56: 2020-11-12
       - Changed ReadFromFile, WriteToFile, and AppendToFile implementation to
         not raise internal exceptions.
     1.55a: 2020-09-30
       - Fixed TGpFixedMemoryStream for 64-bit.
     1.55: 2020-08-21
       - Added methods BE_Read48bits, BE_Write48bits, LE_Read48bits, and LE_Write48bits
         to the stream enhancer.
     1.54: 2020-07-24
       - Added TGpStreamEnhancer.AtStart and .IsEmpty.
     1.53: 2019-12-03
       - Added overloaded SafeCreateGpFileStream which returns error code
         instead of error message.
     1.52: 2019-12-02
       - Implemented TGpStreamEnhancer.ReadTag and .WriteTag with an IGpBuffer parameter.
     1.51: 2019-11-15
       - IsEqual resets stream positions to 0 before comparing streams and
         restores them at the end.
     1.50b: 2018-03-14
       - TGpJoinedStream.Read and TGpJoinedStream.Write would crash when
         stream was larger than MaxInt.
     1.50a: 2017-06-19
       - SafeCreateGpFileStream correctly handles `fmCreate` mode.
     1.50: 2017-05-18
       - Implemented TGpStreamEnhancer.Advance.
     1.49: 2017-03-22
       - Implemented TRawByteStringStream.
       - Implemented TGpStreamEnhancer.WriteRawStr.
     1.48: 2016-02-03
       - AppendToFile opens file with fmShareDenyWrite to allow simultaneous reading.
     1.47a: 2015-11-09
       - Removed 'inline' frm BE_ReadDWord as it caused internal error in D10.
     1.47: 2015-07-22
       - Implemented TGpStreamEnhancer.RemoveFirst and .KeepLast.
     1.46: 2015-01-19
       - Implemented IsEqual(string,string).
     1.45: 2015-01-16
       - Added parameter startAt to the WriteTag(tag, TStream) function. If set to >= 0,
         stream position will be reset to startAt prior to writing.
     1.44: 2015-01-13
       - New methods in TGpStreamEnhancer: GetTag, overloaded PeekTag, GoToEnd, GoToStart.
     1.43a: 2014-03-03
       - Fixed ReadFromFile(string, IGpBuffer).
     1.43: 2014-01-15
       - TGpFixedMemoryStream.Create accepts IGpBuffer.
       - Added AppendToFile overload accepting IGpBuffer.
       - Added WriteToFile overloads accepting AnsiString and IGpBuffer.
       - Added ReadFromFile functions returning TStream, AnsiString and IGpBuffer.
     1.42: 2013-09-24
       - Implemented TAnsiStringStream class.
     1.41: 2013-01-07
       - Added function CloneStream.
     1.40: 2012-11-08
       - Added two WriteToFile functions.
     1.39: 2012-02-06
       - Added function CopyStreamEx that accepts a TStreamProgressEvent.
     1.38: 2011-12-02
       - Added function TGpStreamEnhancer.CheckTag.
     1.37: 2011-03-17
       - TGpJoinedStream can take ownership of substreams.
       - TGpJoinedStream.StreamCount and .Stream[] made public.
       - Added function TGpJoinedStream.RemoveStream and .RemoveProcessedStreams.
     1.36a: 2011-02-11
       - Fixed bug introduced in 1.36.
     1.36: 2011-02-04
       - Fixed TStream helper to not cause internal compiler errors in Delphi XE.
     1.35: 2011-02-02
       - TGpJoinedStream uses Int64 for offsets
     1.34: 2011-01-19
       - Added methods WriteWideStr and WritelnWide to the TGpStreamEnhancer.
     1.33: 2010-12-25
       - ReadTag functions always uses strings with explicit "wideness".
       - Added method WritelnAnsi.
     1.32: 2010-10-11
       - Implemented stream wrapper CreateJoinedStream.
     1.31: 2010-09-15
       - KeepStreamPosition implements Restore function.
     1.30: 2010-04-12
       - Implemented TGpFileStream class and two SafeCreateGpFileStream functions.
     1.29b: 2010-04-09
       - Unicode fixes.
     1.29a: 2010-03-29
       - Disable inlining for Delphi 2007 because of compiler bugs.
     1.29: 2010-03-08
       - Added function BytesLeft to the TStream class helper.
     1.28: 2009-12-11
       - Implemented TGpFixedMemoryStream.CreateA and fixed TGpFixedMemoryStream.Create.
     1.27: 2009-12-09
       - Added function AtEnd to the TStream class helper.
     1.26: 2009-12-09
       - Added AsAnsiString property and WriteAnsiStr method.
     1.25b: 2009-09-14
       - Added setter for TGpFixedMemoryStream.Position so that invalid positions raise
         exception.
     1.25a: 2009-06-30
       - Safer TGpFixedMemoryStream.Read.
     1.25: 2008-09-24
       - Added TGpJoinedStream class.
     1.24: 2008-09-03
       - Span-storing class can now be modified via TGpScatteredStream.SpanClass.
       - TGpScatteredStream's AddSpan and AddSpanOS now return span offset in the span list.
     1.23: 2008-04-30
       - Added bunch of BE_ overloads to the TGpStreamEnhancer class.
     1.22a: 2008-03-31
       - Small optimization in KeepStreamPositionWrapper destructor.
     1.22: 2008-02-21
       - Added AppendToFile helper functions (two overloads).
     1.21: 2007-12-07
       - Added ReadTag and WriteTag support for int64 and WideString data.
     1.20: 2007-11-09
       - Added two overloaded SafeCreateFileStream versions returning exception message.
     1.19: 2007-10-17
       - Implemented Append stream helper.
       - Made 'count' parameter to CopyStream optional, the same way as TStream.CopyFrom
         is implemented. 
     1.18: 2007-10-08
       - Added TGpBufferedStream class. At the moment, only reading is buffered while
         writing is implemented as a pass-through operation.
     1.17: 2007-09-27
       - Fixed TGpScatteredStream on-demand support.
       - Check for < 0 position in TGpStreamWindow.Seek.
       - Added property CumulativeSize to the TGpScatteredStream.
     1.16a: 2007-09-24
       - Fixed reading/writing of zero bytes in TGpStreamWindow.
     1.16: 2007-09-18
       - Added on-demand data provider support (OnNeedMoreData event and option in the
         constructor) to the TGpScatteredStream class.
         When delayed data provider is enabled, Seek to the end of stream won't work.
       - Fixed incompatibility between TGpScatteredStream.AddSpan and new TGpInt64List.
       - Added method AddSpanOS to the TGpScatteredStream class.
       - Added bunch of 'inline' directives.
     1.15: 2007-06-18
       - Implemented TGpScatteredStream class.
     1.14: 2007-06-04
       - Added AutoDestroyWrappedStream property to the TGpStreamWindow class.
     1.13: 2007-03-30
       - Renamed WrapStream -> AutoDestroyStream, KeepPosition -> KeepStreamPosition.
     1.12: 2007-03-21
       - Added WriteStr and Writeln methods to the stream helper.
     1.11a: 2007-03-01
       - Bug fixed: TGpStreamWindow caused exception when empty stream was passed to the
         single-parameter constructor.
     1.11: 2007-01-08
       - Added FirstPos/LastPos properties to the TGpStreamWindow class.
     1.10: 2006-11-08
       - Added overloaded version of SafeCreateFileStream.
       - Added global method DestroyFileStreamAndDeleteFile.
     1.09: 2006-11-07
       - Added global method SafeCreateFileStream.
     1.08: 2006-10-06
       - Added read/write AsString property to the TStream helper.
       - Added auto-destructor-interface-based TStream position keeper KeepPosition.
     1.07: 2006-10-05
       - Added TGpFixedMemoryStream constructor that takes a long string as an argument.
     1.06: 2006-10-03
       - Added readonly AsHexString property to the TStream helper.
     1.05: 2006-09-26
       - All TStream helpers merged together into one class due to Delphi 2006 limitations.
     1.04: 2006-09-26
       - TGpFixedMemoryStream imported from the GpMemStr unit.
       - TGpBigEndianStream TStream helper renamed to TGpStreamEnhancer and extended
         with 'little endian' readers and writers.
       - IGpStreamWrapper renamed to IGpStreamWrapper.
       - WrapStream renamed to WrapStream.
     1.03: 2006-09-25
       - Added auto-destructor-interface-based TStream wrapper WrapStream.
     1.02: 2006-09-25
       - Implemented endianess inverting TStream helper.
     1.01: 2006-09-22
       - Implemented tagging TStream helper.
     1.0: 2006-09-21
       - Created.
*)

unit GpStreams;
                                 
interface

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion > 21} //D2007-D2010 compilers have big internal problems with inlines in this unit
     {$DEFINE GpStreams_Inline}
  {$IFEND}
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF (CompilerVersion >= 17)}
    {$DEFINE USE_STRICT}
  {$IFEND}
{$ENDIF}

{$IF CompilerVersion >= 18.5}
  {$DEFINE GpStreams_ClassHelpers}
{$IFEND}

uses
  Windows,
  SysUtils,
  Classes,
  Contnrs,
  DSiWin32,
  GpStuff,
  GpLists;

type
  ///<summary>Base class for all stream wrappers.</summary>
  TGpStreamWrapper = class(TStream)
  private
    FAutoDestroy: boolean;
    FBaseStream : TStream;
  public
    constructor Create(baseStream: TStream; autoDestroyWrappedStream: boolean = false);
    destructor  Destroy; override;
    property AutoDestroyWrappedStream: boolean read FAutoDestroy write FAutoDestroy;
    property WrappedStream: TStream read FBaseStream;
  end; { TGpStreamWrapper }

  {:A stream-compatible class that can limit read/writes to a window in another stream.
    @since   2006-04-14
  }
  TGpStreamWindow = class(TGpStreamWrapper)
  private
    swFirstPos: int64;
    swLastPos : int64;
  protected
    function  GetSize: int64; override;
  public
    constructor Create(baseStream: TStream); overload;
    constructor Create(baseStream: TStream; firstPos, lastPos: int64;
      autoDestroyWrappedStream: boolean = false); overload;
    function  Read(var buffer; count: integer): integer; override;
    function  Write(const buffer; count: integer): integer; override;
    function  Seek(const offset: int64; origin: TSeekOrigin): int64; overload; override;
    procedure SetWindow(firstPos, lastPos: int64);
    property FirstPos: int64 read swFirstPos;
    property LastPos: int64 read swLastPos;
  end; { TGpStreamWindow }

  {:Provides streamed access to a fixed memory buffer.
    @since   2006-09-26
  }
  TGpFixedMemoryStream = class(TStream)
  private
    fmsBuffer  : pointer;
    fmsPosition: integer;
    fmsSize    : integer;
  protected
    procedure SetPosition(const value: integer);
  public
    constructor Create; overload;
    constructor Create(const data; size: integer); overload;
    constructor Create(const data: string); overload;
    constructor Create(const data: IGpBuffer); overload;
    constructor CreateA(const data: AnsiString); overload;
    function  Read(var data; size: integer): integer; override;
    function  Seek(offset: longint; origin: word): longint; override;
    procedure SetBuffer(const data; size: integer);
    function  Write(const data; size: integer): integer; override;
    property  Position: integer read fmsPosition write SetPosition;
    property  Memory: pointer read fmsBuffer;
  end; { TGpFixedMemoryStream }

  ///<summary>Metadata for one span in a scattered stream.</summary>
  ///<since>2007-06-18</since>
  TGpScatteredStreamSpan = class
  private
    sssCumulativeOffs: int64;
    sssCumulativeSize: int64;
    sssFirstPos      : int64;
    sssLastPos       : int64;
  protected
    property CumulativeOffset: int64 read sssCumulativeOffs write sssCumulativeOffs;
    property CumulativeSize: int64 read sssCumulativeSize write sssCumulativeSize;
  public
    constructor Create(firstPos, lastPos: int64);
    function Size: int64; {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    property FirstPos: int64 read sssFirstPos;
    property LastPos: int64 read sssLastPos;
  end; { TGpScatteredStreamSpan }

  TGpScatteredStreamSpanClass = class of TGpScatteredStreamSpan;

  TGpScatteredStreamOption = (ssoDataOnDemand);
  TGpScatteredStreamOptions = set of TGpScatteredStreamOption;

  ///<summary>Event called when scattered stream needs more data to complete read/write request.</summary><para>
  ///If event handler has more data, it should call TGpScatteredStream(Sender).AddSpan.</para>
  ///<since>2007-09-17</since>
  TGpScatteredStreamNeedMoreData = procedure(Sender: TObject) of object;

  ///<summary>Provides a streamed access to scattered data from another stream.</summary>
  ///<since>2007-06-18</since>
  TGpScatteredStream = class(TGpStreamWrapper)
  private
    ssCurrentPos    : int64;
    ssOnNeedMoreData: TGpScatteredStreamNeedMoreData;
    ssOptions       : TGpScatteredStreamOptions;
    ssSpanClass     : TGpScatteredStreamSpanClass;
    ssSpanIdx       : integer;
    ssSpanList      : TGpInt64ObjectList;
    ssSpanOffset    : int64;
  protected
    function  GetSize: int64; override;
    function  GetSpan(idxSpan: integer): TGpScatteredStreamSpan; {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  InternalSeek(offset: int64): int64; virtual;
    procedure RecalcCumulative(fromIndex: integer); virtual;
  public
    constructor Create(baseStream: TStream; options: TGpScatteredStreamOptions = [];
      autoDestroyWrappedStream: boolean = false); overload;
    destructor  Destroy; override;
    function  AddSpan(firstPos, lastPos: int64): integer;
    function  AddSpanOS(offset, size: int64): integer; {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  CountSpans: integer; {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  CumulativeSize: int64;
    function  LocateCumulativeOffset(offset: int64): integer;
    function  Read(var buffer; count: integer): integer; override;
    function  Seek(const offset: int64; origin: TSeekOrigin): int64; overload; override;
    function  Write(const buffer; count: integer): integer; override;
    property Options: TGpScatteredStreamOptions read ssOptions write ssOptions;
    property Span[idxSpan: integer]: TGpScatteredStreamSpan read GetSpan;
    property SpanClass: TGpScatteredStreamSpanClass read ssSpanClass write ssSpanClass;
    property OnNeedMoreData: TGpScatteredStreamNeedMoreData read ssOnNeedMoreData write
      ssOnNeedMoreData;
  end; { TGpScatteredStream }

  TGpInterceptorReadProc = function (position: int64; var buffer; count: integer): integer of object;
  TGpInterceptorWriteProc = function (position: int64; const buffer; count: integer): integer of object;
  TGpInterceptorSeekProc = function (const offset: int64; origin: TSeekOrigin): int64 of object;
  TGpInterceptorSetSizeProc = procedure (const newSize: int64) of object;

  ///<summary>Interceptor stream-alike which reroutes read/write/seek calls to custom wrappers.</summary>
  TGpInterceptorStream = class(TStream)
  private
    FCurrentPos : int64;
    FReadProc   : TGpInterceptorReadProc;
    FSeekProc   : TGpInterceptorSeekProc;
    FSetSizeProc: TGpInterceptorSetSizeProc;
    FWriteProc  : TGpInterceptorWriteProc;
  protected
    procedure SetPosition(const value: int64);
    procedure SetSize(const newSize: int64); override;
  public
    constructor Create(readProc: TGpInterceptorReadProc; writeProc: TGpInterceptorWriteProc;
      seekProc: TGpInterceptorSeekProc; setSizeProc: TGpInterceptorSetSizeProc);
    function  Read(var buffer; count: integer): integer; override;
    function  Write(const buffer; count: integer): integer; override;
    function  Seek(const offset: int64; origin: TSeekOrigin): int64; overload; override;
    property Position: int64 read FCurrentPos write SetPosition;
  end; { TGpInterceptorStream }

  ///<summary>Provides a buffered access to another stream.</summary>
  ///<since>2007-10-04</since>
  TGpBufferedStream = class(TGpStreamWrapper)
  {$IFDEF USE_STRICT} strict {$ENDIF}  private
    bsBasePosition: int64;
    bsBaseSize    : int64;
    bsBuffer      : PAnsiChar;
    bsBufferData  : integer;
    bsBufferOffset: int64;
    bsBufferPtr   : PAnsiChar;
    bsBufferSize  : integer;
  protected
    function  CurrentPosition: int64; {$IFDEF GpStreams_Inline} inline; {$ENDIF}
    function  GetSize: int64; override;
    function  InternalSeek(offset: int64): int64;
  public
    constructor Create(baseStream: TStream; bufferSize: integer; autoDestroyWrappedStream:
      boolean = false); overload;
    destructor  Destroy; override;
    function  Read(var buffer; count: integer): integer; override;
    function  Seek(const offset: int64; origin: TSeekOrigin): int64; overload; override;
    function  Write(const buffer; count: integer): integer; override;
  end; { TGpBufferedStream }

  // https://en.wikipedia.org/wiki/Block_cipher_mode_of_operation#Counter_(CTR)
  // GpStreams.DCP
  TGpBaseEncryptedStream = class(TGpStreamWrapper)
  private
  const
    CBufferSize = 1024;
  var
    FBufferer   : TGpBufferedStream;
    FInterceptor: TGpInterceptorStream;
  protected
    procedure DoneCypher; virtual; abstract;
    function  GetSize: int64; override;
    procedure InitCypher(const key; keySize: integer; nonce: uint64); virtual; abstract;
    function  InternalSeek(offset: int64): int64;
  public
    constructor Create(const key; keySize: integer; nonce: uint64;
      baseStream: TStream; autoDestroyWrappedStream: boolean = false);
    destructor  Destroy; override;
    function  Seek(const offset: int64; origin: TSeekOrigin): int64; overload; override;
  end; { TGpBaseEncryptedStream }

  ///<summary>Provides a streamed access to collection of streams.</summary>
  ///<since>2008-09-23</since>
  TGpJoinedStream = class(TStream)
  private
    jsButLastSize : int64;
    jsCurrentPos  : int64;
    jsStartOffsets: TGpInt64List;
    jsStreamIdx   : integer;
    jsStreamList  : TObjectList;
    jsStreamOffset: int64;
  protected
    function  CumulativeSize(idxStream: integer): int64;
    function  GetSize: int64; override;
    function  GetStream(idxStream: integer): TStream;
    function  InternalSeek(offset: int64): int64; virtual;
  public
    constructor Create(ownsStreams: boolean = false); overload;
    constructor Create(streams: array of TStream; ownsStreams: boolean = false); overload;
    destructor  Destroy; override;
    procedure AddStream(aStream: TStream);
    procedure RemoveStream(aStream: TStream; failIfContainsPosition: boolean = true); overload;
    procedure RemoveStream(streamIdx: integer; failIfContainsPosition: boolean = true);
      overload;
    procedure RemoveProcessedStreams;
    function  LocateCumulativeOffset(offset: int64): integer;
    function  Read(var buffer; count: integer): integer; override;
    function  Seek(const offset: int64; origin: TSeekOrigin): int64; overload; override;
    function  StreamCount: integer;
    function  Write(const buffer; count: integer): integer; override;
    property Stream[idxStream: integer]: TStream read GetStream;
  end; { TGpJoinedStream }

  {:AnsiString-based implementation of TStringStream, used for AnsiString processing.
  }
  TAnsiStringStream = class(TStream)
  private
    FDataString: AnsiString;
    FPosition  : Integer;
  protected
    procedure SetSize(newSize: longint); override;
  public
    constructor Create(const value: AnsiString);
    function  Read(var buffer; count: longint): longint; override;
    function  ReadString(count: longint): AnsiString;
    function  Seek(offset: longint; origin: word): longint; override;
    function  Write(const buffer; count: longint): longint; override;
    procedure WriteString(const value: AnsiString);
    property DataString: AnsiString read FDataString;
  end; { TAnsiStringStream }

  {:RawByteString-based implementation of TStringStream, used for UTF-8 processing.
  }
  TRawByteStringStream = class(TStream)
  private
    FDataString: RawByteString;
    FPosition  : Integer;
  protected
    procedure SetSize(newSize: longint); override;
  public
    constructor Create(const value: RawByteString);
    function  Read(var buffer; count: longint): longint; override;
    function  ReadString(count: longint): RawByteString;
    function  Seek(offset: longint; origin: word): longint; override;
    function  Write(const buffer; count: longint): longint; override;
    procedure WriteString(const value: RawByteString);
    property DataString: RawByteString read FDataString;
  end; { TRawByteStringStream }

  IGpStreamWrapper = interface['{12735720-9247-42D4-A911-D23AD8D2B03D}']
    function  GetStream: TStream;
    procedure Restore;
    property Stream: TStream read GetStream;
  end; { IGpStreamWrapper }

  {:Small enhancements to the TStream class and descendants.
    @since   2006-09-21
  }
  {$IFDEF GpStreams_ClassHelpers}
  TGpStreamEnhancer = class helper for TStream
  public
    // Big-Endian (Motorola) readers/writers
    function  BE_Read24bits: DWORD; overload;                   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_Read24bits(var w24: DWORD): boolean; overload; {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_Read48bits: int64; overload;                   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_Read48bits(var h: int64): boolean; overload;   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_ReadByte: byte; overload;                      {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_ReadByte(var b: byte): boolean; overload;      {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_ReadDWord: DWORD; overload;                    {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_ReadDWord(var dw: DWORD): boolean; overload;   //inline causes "F2084 Internal Error: C6908" in D10
    function  BE_ReadGUID: TGUID; overload;                     {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_ReadGUID(var guid: TGUID): boolean; overload;  {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_ReadHuge: int64;  overload;                    {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_ReadHuge(var h: int64): boolean;  overload;    {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_ReadWord: word; overload;                      {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BE_ReadWord(var w: word): boolean; overload;      //inline causes "F2084 Internal Error: C5849" in XE2
    procedure BE_Write24bits(const dw: DWORD);                  {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure BE_Write48bits(const h: int64);                   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure BE_WriteByte(const b: byte);                      {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure BE_WriteDWord(const dw: DWORD);                   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure BE_WriteGUID(const g: TGUID);                     {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure BE_WriteHuge(const h: Int64);                     {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure BE_WriteWord(const w: word);                      {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    // Little-Endian (Intel) readers/writers
    function  LE_Read24bits: DWORD;               {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  LE_Read48bits: int64;               {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  LE_ReadByte: byte;                  {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  LE_ReadDWord: DWORD;                {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  LE_ReadGUID: TGUID;                 {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  LE_ReadHuge: int64;                 {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  LE_ReadWord: word;                  {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure LE_Write24bits(const dw: DWORD);    {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure LE_Write48bits(const h: int64);     {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure LE_WriteByte(const b: byte);        {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure LE_WriteDWord(const dw: DWORD);     {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure LE_WriteGUID(const g: TGUID);       {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure LE_WriteHuge(const h: Int64);       {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure LE_WriteWord(const w: word);        {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    // Tagged readers/writers
    function  CheckTag(tag: integer): boolean;
    function  GetTag(var tag: integer; var stream: IGpStreamWrapper): boolean;
    function  PeekTag(var tag: integer): boolean; overload;                  {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  PeekTag(var tag: integer; var dataPos: int64; var dataSize: integer): boolean; overload;
    function  ReadTag(var tag: integer): boolean; overload;                  {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  ReadTag(tag: integer; var data: boolean): boolean; overload;   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  ReadTag(tag: integer; var data: integer): boolean; overload;   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  ReadTag(tag: integer; size: integer; var buf): boolean; overload;
    function  ReadTag(tag: integer; var data: AnsiString): boolean; overload;
    function  ReadTag(tag: integer; var data: WideString): boolean; overload;
    function  ReadTag(tag: integer; var data: TDateTime): boolean; overload; {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  ReadTag(tag: integer; data: TStream): boolean; overload;
    function  ReadTag(tag: integer; var data: IGpBuffer): boolean; overload; {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  ReadTag64(tag: integer; var data: int64): boolean; overload;   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WriteTag(tag: integer); overload;                  {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WriteTag(tag: integer; data: boolean); overload;   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WriteTag(tag: integer; data: integer); overload;   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WriteTag(tag: integer; size: integer; const buf); overload;
    procedure WriteTag(tag: integer; data: AnsiString); overload;
    procedure WriteTag(tag: integer; data: WideString); overload;
    procedure WriteTag(tag: integer; data: TDateTime); overload; {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WriteTag(tag: integer; data: TStream; startAt: int64 = -1); overload;   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WriteTag(tag: integer; const data: IGpBuffer); overload;       {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WriteTag64(tag: integer; data: int64);             {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure SkipTag;
    // Text file emulator
    procedure WriteAnsiStr(const s: AnsiString);  {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WriteRawStr(const s: RawByteString);{$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WriteStr(const s: string);          {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WriteWideStr(const s: WideString);  {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure Writeln(const s: string = '');      {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WritelnAnsi(const s: AnsiString = '');      {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure WritelnWide(const s: WideString = '');      {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    // TMemoryStream data movers
    procedure RemoveFirst(numBytes: integer);
    procedure KeepLast(numBytes: integer);        {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    // Other helpers
    procedure Advance(deltaPosition: int64);      {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure Append(source: TStream);            {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  AtEnd: boolean;                     {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  AtStart: boolean;                   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  BytesLeft: int64;
    procedure Clear;                              {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  GetAsHexString: string;
    function  GetAsAnsiString: AnsiString;
    function  GetAsString: string;
    procedure GoToStart;                          {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure GoToEnd;                            {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    function  IsEmpty: boolean;                   {$IFDEF GpStreams_Inline}inline;{$ENDIF}
    procedure LoadFromFile(const fileName: string);
    procedure SaveToFile(const fileName: string);
    procedure SetAsAnsiString(const value: AnsiString);
    procedure SetAsString(const value: string);
    property AsHexString: string read GetAsHexString;
    property AsString: string read GetAsString write SetAsString;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
  end; { TGpStreamEnhancer }
  {$ENDIF}

  TGpFileStream = class(THandleStream)
  {$IFDEF USE_STRICT} strict {$ENDIF}  private
    gfsFileName  : string;
  public
    constructor Create(const fileName: string; fileHandle: THandle);
    destructor  Destroy; override;
    property FileName: string read gfsFileName;
  end; { TGpFileStream }

  TStreamProgressEvent = procedure(current, total: int64; var abort: boolean) of object;

  {:Creates stream wrapper that automatically destroys the stream when interface leaves
    the scope. Use only if you know what you're doing.
    @since   2006-09-25
  }
  function AutoDestroyStream(stream: TStream): IGpStreamWrapper;

  ///<summary>Either creates a joined stream or returns one source stream directly
  ///    if all other source streams are nil or contain no data. The destructor either
  ///    destroys the joined stream or does nothing if a source stream was returned
  ///    directly.</summary>
  ///<since>2010-10-11</since>
  function CreateJoinedStream(var joinedStream: TStream; streams: array of TStream): IGpStreamWrapper;

  {:Creates a wrapper that automatically restores TStream position when interface leaves
    the scope. Use only if you know what you're doing.
    @since   2006-10-06
  }
  function KeepStreamPosition(stream: TStream; newPosition: int64 = -1): IGpStreamWrapper;

  {:Creates a TGpFileStream object. Catches EFOpenError/EFCreateError exceptions and
    converts them to a False result. Stores exception text in an output variable.
    Waits up to the specified time on file sharing errors.
    @since   2010-04-12
  }
  function SafeCreateGpFileStream(const fileName: string; mode: word;
    waitUpTo_ms: integer; var fileStream: TGpFileStream;
    var errorMessage: string): boolean; overload;

  {:Creates a TGpFileStream object. Catches EFOpenError/EFCreateError exceptions and
    converts them to a False result. Stores error code in an output variable.
    Waits up to the specified time on file sharing errors.
    @since   2010-04-12
  }
  function SafeCreateGpFileStream(const fileName: string; mode: word;
    waitUpTo_ms: integer; var fileStream: TGpFileStream;
    var error: DWORD): boolean; overload;

  {:Creates a TGpFileStream object. Catches EFOpenError/EFCreateError exceptions and
    converts them to a False result. Stores exception text in an output variable.
    Waits up to the specified time on file sharing errors.
    @since   2010-04-12
  }
  function SafeCreateGpFileStream(const fileName: string; mode: word;
    waitUpTo_ms: integer; var fileStream: TGpFileStream): boolean; overload;

  {:Creates a TFileStream object. Catches EFOpenError/EFCreateError exceptions and
    converts them to a False result. Stores exception text in an output variable.
    @since   2007-11-08
  }
  function SafeCreateFileStream(const fileName: string; mode: word;
    var fileStream: TFileStream; var errorMessage: string): boolean; overload;

  {:Creates a TFileStream object. Catches EFOpenError/EFCreateError exceptions and
    converts them to a False result.
    @since   2006-11-07
  }
  function SafeCreateFileStream(const fileName: string; mode: word;
    var fileStream: TFileStream): boolean; overload;

  {:Creates a TFileStream object. Catches EFOpenError/EFCreateError exceptions and
    converts them to a Nil result. Stores exception text in an output variable.
    @since   2006-11-07
  }
  function SafeCreateFileStream(const fileName: string; mode: word;
    var errorMessage: string): TFileStream; overload;

  {:Creates a TFileStream object. Catches EFOpenError/EFCreateError exceptions and
    converts them to a Nil result.
    @since   2006-11-07
  }
  function SafeCreateFileStream(const fileName: string; mode: word): TFileStream; overload;

  {:Destroys file stream object and sets object reference to nil, then deletes the file
    used by the file stream object.
    @returns result of the DeleteFile function.
    @since   2006-11-08
  }
  {$IFDEF GpStreams_ClassHelpers}
  function DestroyFileStreamAndDeleteFile(var fileStream: TFileStream): boolean;
  {$ENDIF}

  ///<summary>Copies stream of unknown size. A companion to the TGpScatteredStream in
  ///    on-demand mode.</summary>
  ///<returns>Number of bytes copied.</returns>
  ///<since>2007-09-18</since>
  function CopyStream(source, destination: TStream; count: int64 = 0): int64;
  function CopyStreamEx(source, destination: TStream; count: int64; progressEvent: TStreamProgressEvent): int64;

  ///<summary>
  ///    Copies 'source' stream into 'destination' stream and returns 'destination' stream.
  ///</summary>
  function CloneStream(destination, source: TStream): TStream;

  function IsEqual(stream1, stream2: TStream): boolean;

  function AppendToFile(const fileName: string; data: TStream): boolean; overload;
  function AppendToFile(const fileName: string; var data; dataSize: integer): boolean; overload;
  function AppendToFile(const fileName: string; const data: AnsiString): boolean; overload;
  function AppendToFile(const fileName: string; const buffer: IGpBuffer): boolean; overload;

  function WriteToFile(const fileName: string; data: TStream): boolean; overload;
  function WriteToFile(const fileName: string; var data; dataSize: integer): boolean; overload;
  function WriteToFile(const fileName: string; const data: AnsiString): boolean; overload;
  function WriteToFile(const fileName: string; const buffer: IGpBuffer): boolean; overload;

  function ReadFromFile(const fileName: string; data: TStream): boolean; overload;
  function ReadFromFile(const fileName: string; var data: AnsiString): boolean; overload;
  function ReadFromFile(const fileName: string; var buffer: IGpBuffer): boolean; overload;

implementation

type
  TGpDoNothingStreamWrapper = class(TInterfacedObject, IGpStreamWrapper)
  private
    FStream: TStream;
  protected
    procedure SetStream(stream: TStream);
  public
    constructor Create(stream: TStream);
    destructor  Destroy; override;
    function  GetStream: TStream;
    procedure Restore; virtual;
    property Stream: TStream read GetStream write SetStream;
  end; { TGpDoNothingStreamWrapper }

  TGpAutoDestroyStreamWrapper = class(TGpDoNothingStreamWrapper)
  public
    procedure Restore; override;
  end; { TGpAutoDestroyStreamWrapper }

  TGpKeepStreamPositionWrapper = class(TGpDoNothingStreamWrapper)
  private
    kspwOriginalPosition: int64;
  public
    constructor Create(managedStream: TStream);
    procedure Restore; override;
  end; { TGpKeepStreamPositionWrapper }

{ publics }

function AppendToFile(const fileName: string; data: TStream): boolean;
var
  fileHandle: THandle;
  fs        : TStream;
begin
  Result := false;

  if not FileExists(fileName) then
    fileHandle := FileCreate(fileName, fmCreate, 0)
  else
    fileHandle := FileOpen(fileName, fmOpenWrite + fmShareDenyWrite);
  if fileHandle = INVALID_HANDLE_VALUE then
    Exit;

  try
    fs := THandleStream.Create(fileHandle);
    try
      fs.Position := fs.Size;
      Result := fs.CopyFrom(data, 0) = data.Size;
    finally FreeAndNil(fs); end;
  finally FileClose(fileHandle); end;
end; { AppendToFile }

function AppendToFile(const fileName: string; var data; dataSize: integer): boolean;
begin
  Result := AppendToFile(fileName,
    AutoDestroyStream(TGpFixedMemoryStream.Create(data, dataSize)).Stream);
end; { AppendToFile }

function AppendToFile(const fileName: string; const data: AnsiString): boolean;
begin
  Result := AppendToFile(fileName,
    AutoDestroyStream(TGpFixedMemoryStream.CreateA(data)).Stream);
end; { AppendToFile }

function AppendToFile(const fileName: string; const buffer: IGpBuffer): boolean; overload;
begin
  Result := AppendToFile(fileName,
    AutoDestroyStream(TGpFixedMemoryStream.Create(buffer)).Stream);
end; { AppendToFile }

function WriteToFile(const fileName: string; data: TStream): boolean;
var
  fileHandle: THandle;
  fs        : TStream;
begin
  Result := false;
  fileHandle := FileCreate(fileName, fmCreate, 0);
  if fileHandle = INVALID_HANDLE_VALUE then
    Exit;

  try
    fs := THandleStream.Create(fileHandle);
    try
      Result := fs.CopyFrom(data, 0) = data.Size;
    finally FreeAndNil(fs); end;
  finally FileClose(fileHandle); end;
end; { WriteToFile }

function WriteToFile(const fileName: string; var data; dataSize: integer): boolean;
begin
  Result := WriteToFile(fileName,
    AutoDestroyStream(TGpFixedMemoryStream.Create(data, dataSize)).Stream);
end; { WriteToFile }

function WriteToFile(const fileName: string; const data: AnsiString): boolean; overload;
begin
  Result := WriteToFile(fileName,
    AutoDestroyStream(TGpFixedMemoryStream.CreateA(data)).Stream);
end; { WriteToFile }

function WriteToFile(const fileName: string; const buffer: IGpBuffer): boolean; overload;
begin
  Result := WriteToFile(fileName,
    AutoDestroyStream(TGpFixedMemoryStream.Create(buffer)).Stream);
end; { WriteToFile }

function ReadFromFile(const fileName: string; data: TStream): boolean;
var
  fileHandle: THandle;
  fs        : TStream;
begin
  fileHandle := FileOpen(fileName, fmOpenRead);
  if fileHandle = INVALID_HANDLE_VALUE then
    Exit(false);

  try
    fs := THandleStream.Create(fileHandle);
    try
      Result := data.CopyFrom(fs, 0) = fs.Size;
    finally FreeAndNil(fs); end;
  finally FileClose(fileHandle); end;
end; { ReadFromFile }

function ReadFromFile(const fileName: string; var data: AnsiString): boolean;
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    Result := ReadFromFile(fileName, stream);
    if Result then begin
      SetLength(data, stream.Size);
      if stream.Size > 0 then
        Move(stream.Memory^, data[1], stream.Size);
    end;
  finally FreeAndNil(stream); end;
end; { ReadFromFile }

function ReadFromFile(const fileName: string; var buffer: IGpBuffer): boolean; overload;
var
  stream: TMemoryStream;
begin
  buffer := TGpBuffer.Create;
  stream := TMemoryStream.Create;
  try
    Result := ReadFromFile(fileName, stream);
    if Result then
      buffer.Assign(stream.Memory, stream.Size);
  finally FreeAndNil(stream); end;
end; { ReadFromFile }

function AutoDestroyStream(stream: TStream): IGpStreamWrapper;
begin
  Result := TGpAutoDestroyStreamWrapper.Create(stream);
end; { AutoDestroyStream }

function CreateJoinedStream(var joinedStream: TStream; streams: array of TStream):
  IGpStreamWrapper;
var
  dataStream   : TStream;
  numDataStream: integer;
  stream       : TStream;
{$IFDEF VER150}
var
  i            : integer;
{$ENDIF VER150}
begin
  joinedStream := TGpJoinedStream.Create;
  dataStream := nil;
  numDataStream := 0;
  {$IFDEF VER150}
  // TP : Untested D7 enabled loop
  for i := Low(streams) to High(streams) do begin
    stream := streams[I];
  {$ELSE}
  for stream in streams do begin
  {$ENDIF ~VER150}
    if assigned(stream) and (stream.Size > 0) then begin
      dataStream := stream;
      Inc(numDataStream);
      TGpJoinedStream(joinedStream).AddStream(stream);
    end;
  end;
  if numDataStream = 1 then begin
    FreeAndNil(joinedStream);
    joinedStream := dataStream;
    Result := TGpDoNothingStreamWrapper.Create(joinedStream);
  end
  else begin
    if numDataStream = 0 then begin
      FreeAndNil(joinedStream);
      joinedStream := TMemoryStream.Create;
    end;
    Result := AutoDestroyStream(joinedStream);
  end;
end; { CreateJoinedStream }

function KeepStreamPosition(stream: TStream; newPosition: int64): IGpStreamWrapper;
begin
  Result := TGpKeepStreamPositionWrapper.Create(stream);
  if newPosition >= 0 then
    stream.Position := newPosition;
end; { KeepStreamPosition }

function SafeCreateGpFileStream(const fileName: string; mode: word; waitUpTo_ms: integer;
  var fileStream: TGpFileStream; var errorMessage: string): boolean;
var
  handle   : THandle;
  startTime: int64;
begin
  startTime := DSiTimeGetTime64;
  repeat
    if mode = fmCreate then
      handle := THandle(FileCreate(fileName))
    else
      handle := THandle(FileOpen(fileName, mode));
    if (handle <> INVALID_HANDLE_VALUE) or (GetLastError <> ERROR_SHARING_VIOLATION) or
        DSiHasElapsed(startTime, waitUpTo_ms)
    then
      break; //repeat
    Sleep(50);
  until false;
  if handle = INVALID_HANDLE_VALUE then begin
    fileStream := nil;
    errorMessage := SysErrorMessage(GetLastError);
    Result := false;
  end
  else begin
    fileStream := TGpFileStream.Create(fileName, handle);
    errorMessage := '';
    Result := true;
  end;
end; { SafeCreateGpFileStream }

function SafeCreateGpFileStream(const fileName: string; mode: word; waitUpTo_ms: integer;
  var fileStream: TGpFileStream; var error: DWORD): boolean;
var
  handle   : THandle;
  startTime: int64;
begin
  error := 0;
  startTime := DSiTimeGetTime64;
  repeat
    if mode = fmCreate then
      handle := THandle(FileCreate(fileName))
    else
      handle := THandle(FileOpen(fileName, mode));
    if (handle <> INVALID_HANDLE_VALUE) or (GetLastError <> ERROR_SHARING_VIOLATION) or
        DSiHasElapsed(startTime, waitUpTo_ms)
    then
      break; //repeat
    Sleep(50);
  until false;

  if handle = INVALID_HANDLE_VALUE then begin
    fileStream := nil;
    error := GetLastError;
    Result := false;
  end
  else begin
    fileStream := TGpFileStream.Create(fileName, handle);
    error := NO_ERROR;
    Result := true;
  end;
end; { SafeCreateGpFileStream }

function SafeCreateGpFileStream(const fileName: string; mode: word; waitUpTo_ms: integer;
  var fileStream: TGpFileStream): boolean;
var
  errMsg: string;
begin
  Result := SafeCreateGpFileStream(fileName, mode, waitUpTo_ms, fileStream, errMsg);
end; { SafeCreateGpFileStream }

function SafeCreateFileStream(const fileName: string; mode: word; var fileStream:
  TFileStream; var errorMessage: string): boolean; overload;
begin
  Result := false;
  errorMessage := '';
  try
    fileStream := TFileStream.Create(fileName, mode);
    Result := true;
  except
    on E: EFCreateError do
      errorMessage := E.Message;
    on E: EFOpenError do
      errorMessage := E.Message;
  end;
end; { SafeCreateFileStream }

function SafeCreateFileStream(const fileName: string; mode: word; var fileStream:
  TFileStream): boolean;
var
  errorMessage: string;
begin
  Result := SafeCreateFileStream(fileName, mode, fileStream, errorMessage);
end; { SafeCreateFileStream }

function SafeCreateFileStream(const fileName: string; mode: word;
  var errorMessage: string): TFileStream;
begin
  if not SafeCreateFileStream(fileName, mode, Result, errorMessage) then
    Result := nil;
end; { SafeCreateFileStream }

function SafeCreateFileStream(const fileName: string; mode: word): TFileStream;
begin
  if not SafeCreateFileStream(fileName, mode, Result) then
    Result := nil;
end; { SafeCreateFileStream }

{$IFDEF GpStreams_ClassHelpers}
function DestroyFileStreamAndDeleteFile(var fileStream: TFileStream): boolean;
var
  sFileName: string;
begin
  sFileName := fileStream.FileName;
  FreeAndNil(fileStream);
  Result := DeleteFile(sFileName);
end; { DestroyFileStreamAndDeleteFile }
{$ENDIF}

function CopyStream(source, destination: TStream; count: int64): int64;
const
  MaxBufSize = $F000;
var
  buffer     : PAnsiChar;
  bufSize    : integer;
  bytesRead  : integer;
  bytesToRead: integer;
begin
  Result := 0;
  if count = 0 then begin
    Source.Position := 0;
    count := -1;
  end;
  if (count < 0) or (count > MaxBufSize) then
    bufSize := MaxBufSize
  else
    bufSize := count;
  GetMem(buffer, bufSize);
  try
    while count <> 0 do begin
      if (count > bufSize) or (count < 0) then
        bytesToRead := bufSize
      else
        bytesToRead := count;
      bytesRead := source.Read(buffer^, bytesToRead);
      destination.WriteBuffer(buffer^, bytesRead);
      if count > 0 then
        Dec(count, bytesRead);
      Inc(Result, bytesRead);
      if bytesRead < bytesToRead then
        break; //while
    end; //while
  finally FreeMem(buffer); end;
end; { CopyStream }

function CopyStreamEx(source, destination: TStream; count: int64; progressEvent: TStreamProgressEvent): int64;
const
  MaxBufSize = $F000;
var
  buffer     : PAnsiChar;
  bufSize    : integer;
  bytesRead  : integer;
  bytesToRead: integer;
  abort      : boolean;
  total      : int64;
begin
  Result := 0;
  abort := false;
  if count = 0 then begin
    Source.Position := 0;
    count := -1;
  end;
  if (count < 0) or (count > MaxBufSize) then
    bufSize := MaxBufSize
  else
    bufSize := count;
  total := count;
  GetMem(buffer, bufSize);
  try
    while count <> 0 do begin
      if (count > bufSize) or (count < 0) then
        bytesToRead := bufSize
      else
        bytesToRead := count;
      bytesRead := source.Read(buffer^, bytesToRead);
      destination.WriteBuffer(buffer^, bytesRead);
      progressEvent(result, total, abort);
      if abort then
        exit;
      if count > 0 then
        Dec(count, bytesRead);
      Inc(Result, bytesRead);
      if bytesRead < bytesToRead then
        break; //while
    end; //while
  finally FreeMem(buffer); end;
end; { CopyStreamEx }

function CloneStream(destination, source: TStream): TStream;
begin
  destination.Position := 0;
  CopyStream(source, destination);
  destination.Size := destination.Position;
  Result := destination;
end; { CloneStream }

function IsEqual(stream1, stream2: TStream): boolean;
const
  CBlockSize = 65536;
var
  buf1  : array [1 .. CBlockSize] of byte;
  buf2  : array [1 .. CBlockSize] of byte;
  bufLen: integer;
begin
  Result := true;
  if stream1.Size <> stream2.Size then
    Exit(false);

  with KeepStreamPosition(stream1, 0),
       KeepStreamPosition(stream2, 0)
  do
  while not stream1.AtEnd do begin
    bufLen := stream1.Read(buf1, CBlockSize);
    stream2.Read(buf2, bufLen);
    if not CompareMem(@buf1, @buf2, bufLen) then
      Exit(false);
  end;
end; { IsEqual }

{ TGpStreamWrapper }

constructor TGpStreamWrapper.Create(baseStream: TStream;
  autoDestroyWrappedStream: boolean);
begin
  inherited Create;
  FBaseStream := baseStream;
  FAutoDestroy := autoDestroyWrappedStream;
end; { TGpStreamWrapper.Create }

destructor TGpStreamWrapper.Destroy;
begin
  if FAutoDestroy then
    FreeAndNil(FBaseStream);
  inherited;
end; { TGpStreamWrapper.Destroy }

{ TGpStreamWindow }

constructor TGpStreamWindow.Create(baseStream: TStream; firstPos, lastPos: int64;
  autoDestroyWrappedStream: boolean = false);
begin
  inherited Create(baseStream, autoDestroyWrappedStream);
  SetWindow(firstPos, lastPos);
end; { TGpStreamWindow.Create }

constructor TGpStreamWindow.Create(baseStream: TStream);
begin
  Create(baseStream, 0, IFF64(baseStream.Size = 0, 0, baseStream.Size-1));
end; { TGpStreamWindow.Create }

function TGpStreamWindow.GetSize: int64;
begin
  Result := swLastPos - swFirstPos + 1;
end; { TGpStreamWindow.GetSize }

function TGpStreamWindow.Read(var buffer; count: integer): integer;
begin
  Result := count;
  if (WrappedStream.Position + count - 1) > swLastPos then
    Result := swLastPos - WrappedStream.Position + 1;
  if Result > 0 then
    Result := WrappedStream.Read(buffer, Result);
end; { TGpStreamWindow.Read }

function TGpStreamWindow.Seek(const offset: int64; origin: TSeekOrigin): int64;
begin
  case origin of
    soBeginning:
      Result := WrappedStream.Seek(swFirstPos + offset, soBeginning) - swFirstPos;
    soCurrent:
      Result := WrappedStream.Seek(offset, soCurrent) - swFirstPos;
    soEnd:
      Result := WrappedStream.Seek(swLastPos + offset, soBeginning) - swFirstPos;
    else
      Result := 0; // to keep Delphi happy
  end;
  if Result < 0 then
    raise Exception.Create('TGpStreamWindow.Seek: Seek out of range');
end; { TGpStreamWindow.Seek }

procedure TGpStreamWindow.SetWindow(firstPos, lastPos: int64);
begin
  if firstPos < 0 then
    raise EStreamError.CreateFmt('Invalid first position: %d', [firstPos]);
  if lastPos >= WrappedStream.Size then
    raise EStreamError.CreateFmt('Invalid last position: %d (base stream size: %d)', [lastPos, WrappedStream.Size]);

  swFirstPos := firstPos;
  swLastPos := lastPos;
  if WrappedStream.Position < swFirstPos then
    WrappedStream.Position := swFirstPos
  else if WrappedStream.Position > swLastPos then
    WrappedStream.Position := swLastPos;
end; { TGpStreamWindow.SetWindow }

function TGpStreamWindow.Write(const buffer; count: integer): integer;
begin
  Result := count;
  if (WrappedStream.Position + count - 1) > swLastPos then
    Result := swLastPos - WrappedStream.Position + 1;
  if Result > 0 then
    Result := WrappedStream.Write(buffer, Result);
end; { TGpStreamWindow.Write }

{ TGpFixedMemoryStream }

constructor TGpFixedMemoryStream.Create;
begin
  inherited Create;
end; { TGpFixedMemoryStream.Create }

constructor TGpFixedMemoryStream.Create(const data; size: integer);
begin
  inherited Create;
  SetBuffer(data, size);
end; { TGpFixedMemoryStream.Create }

constructor TGpFixedMemoryStream.Create(const data: string);
begin
  inherited Create;
  if data = '' then
    SetBuffer(self, 0)
  else
    SetBuffer(data[1], Length(data)*SizeOf(char));
end; { TGpFixedMemoryStream.Create }

constructor TGpFixedMemoryStream.Create(const data: IGpBuffer);
begin
  inherited Create;
  if data.Size = 0 then
    SetBuffer(self, 0)
  else
    SetBuffer(data.Value^, data.Size);
end; { TGpFixedMemoryStream.Create }

constructor TGpFixedMemoryStream.CreateA(const data: AnsiString);
begin
  inherited Create;
  if data = '' then
    SetBuffer(self, 0)
  else
    SetBuffer(data[1], Length(data)*SizeOf(AnsiChar));
end; { TGpFixedMemoryStream.Create }

function TGpFixedMemoryStream.Read(var data; size: integer): integer;
begin
  Result := size;
  if (fmsPosition + Result) > fmsSize then
    Result := fmsSize - fmsPosition;
  if Result < 0 then
    Result := 0
  else if Result > 0 then begin
    Move(pointer(NativeUInt(fmsBuffer)+NativeUInt(fmsPosition))^, data, Result);
    fmsPosition := fmsPosition + Result
  end;
end; { TGpFixedMemoryStream.Read }

function TGpFixedMemoryStream.Seek(offset: longint; origin: word): longint;
begin
  if origin = soFromBeginning then
    fmsPosition := offset
  else if origin = soFromCurrent then
    fmsPosition := fmsPosition + offset
  else
    fmsPosition := fmsSize - offset;
  Result := fmsPosition;
end; { TGpFixedMemoryStream.Seek }

procedure TGpFixedMemoryStream.SetBuffer(const data; size: integer);
begin
  fmsBuffer  := @data;
  fmsSize    := size;
  fmsPosition:= 0;
end; { TGpFixedMemoryStream.SetBuffer }

procedure TGpFixedMemoryStream.SetPosition(const value: integer);
begin
  if (value < 0) or (value > fmsSize) then
    raise Exception.CreateFmt(
      'TGpFixedMemoryStream.SetPosition: Invalid position %d, should lie in range [0, %d].',
      [value, fmsSize-1]);
  fmsPosition := value;
end; { TGpFixedMemoryStream.SetPosition }

function TGpFixedMemoryStream.Write(const data; size: integer): integer;
begin
  if (fmsPosition+size) > fmsSize then size := fmsSize-fmsPosition;
  Move(data,pointer(NativeUInt(fmsBuffer)+NativeUInt(fmsPosition))^,size);
  fmsPosition := fmsPosition + size;
  Write := size;
end; { TGpFixedMemoryStream.Write }

{ TGpScatteredStreamSpan }

constructor TGpScatteredStreamSpan.Create(firstPos, lastPos: int64);
begin
  inherited Create;
  sssFirstPos := firstPos;
  sssLastPos := lastPos;
end; { TGpScatteredStreamSpan.Create }

function TGpScatteredStreamSpan.Size: int64;
begin
  Result := LastPos - FirstPos + 1;
end; { TGpScatteredStreamSpan.Size }

{ TGpScatteredStream }

constructor TGpScatteredStream.Create(baseStream: TStream; options:
  TGpScatteredStreamOptions; autoDestroyWrappedStream: boolean);
begin
  inherited Create(baseStream, autoDestroyWrappedStream);
  ssSpanClass := TGpScatteredStreamSpan;
  ssSpanList := TGpInt64ObjectList.Create;
  ssSpanList.Sorted := true;
  ssSpanList.Duplicates := dupError;
  ssOptions := options;
end; { TGpScatteredStream.Create }

destructor TGpScatteredStream.Destroy;
begin
  FreeAndNil(ssSpanList);
  inherited;
end; { TGpScatteredStream.Destroy }

///<summary>Declares a segment of data in the wrapped stream spanning from firstPos to lastPos.</summary>
///<since>2007-06-18</since>
function TGpScatteredStream.AddSpan(firstPos, lastPos: int64): integer;
var
  idxSpan: integer;
  newSpan: TGpScatteredStreamSpan;
begin
  if firstPos > lastPos then
    raise Exception.Create('TGpScatteredStream.AddSpan: firstPos > lastPos');
  ssSpanList.Find(firstPos, idxSpan);
  if idxSpan > 0 then
    if firstPos <= Span[idxSpan-1].LastPos then
      raise EStreamError.CreateFmt(
        'TGpScatteredStream.AddSpan: Span (%d,%d) overlaps with span (%d,%d)',
        [firstPos, lastPos, Span[idxSpan-1].FirstPos, Span[idxSpan-1].LastPos]);
  if idxSpan < (CountSpans - 1) then
    if lastPos >= Span[idxSpan+1].FirstPos then
      raise EStreamError.CreateFmt(
        'TGpScatteredStream.AddSpan: Span (%d,%d) overlaps with span (%d,%d)',
        [firstPos, lastPos, Span[idxSpan+1].FirstPos, Span[idxSpan+1].LastPos]);
  newSpan := ssSpanClass.Create(firstPos, lastPos);
  Result := ssSpanList.AddObject(firstPos, newSpan);
  RecalcCumulative(idxSpan);
end; { TGpScatteredStream.AddSpan }

function TGpScatteredStream.AddSpanOS(offset, size: int64): integer;
begin
  Result := AddSpan(offset, offset + size - 1);
end; { TGpScatteredStream.AddSpanOS }

function TGpScatteredStream.CountSpans: integer;
begin
  Result := ssSpanList.Count;
end; { TGpScatteredStream.CountSpans }

function TGpScatteredStream.CumulativeSize: int64;
begin
  Result := 0;
  if CountSpans > 0 then
    Result := Span[CountSpans - 1].CumulativeSize;
end; { TGpScatteredStream.CumulativeSize }

function TGpScatteredStream.GetSize: int64;
begin
  if ssoDataOnDemand in Options then
    raise EStreamError.Create('TGpScatteredStream.GetSize: Cannot calculate size for on-demand scattered stream')
  else if CountSpans <= 0 then
    Result := 0
  else
    Result := Span[CountSpans-1].CumulativeSize;
end; { TGpScatteredStream.GetSize }

function TGpScatteredStream.GetSpan(idxSpan: integer): TGpScatteredStreamSpan;
begin
  Result := TGpScatteredStreamSpan(ssSpanList.Objects[idxSpan]);
end; { TGpScatteredStream.GetSpan }

function TGpScatteredStream.InternalSeek(offset: int64): int64;
begin
  if offset = 0 then begin
    ssSpanIdx := 0;
    ssSpanOffset := 0;
  end
  else begin
    ssSpanIdx := LocateCumulativeOffset(offset);
    if ssSpanIdx < 0 then
      raise EStreamError.CreateFmt('TGpScatteredStream.InternalSeek: Seek offset %d out of range', [offset]);
    ssSpanOffset := offset - Span[ssSpanIdx].CumulativeOffset;
  end;
  ssCurrentPos := offset;
  Result := offset;
end; { TGpScatteredStream.InternalSeek }

function TGpScatteredStream.LocateCumulativeOffset(offset: int64): integer;
var
  L, H, I: integer;
begin
  if (offset < 0) or (CountSpans = 0) or (offset > Span[CountSpans-1].CumulativeSize) then
    Result := -1
  else begin
    L := 0;
    H := CountSpans - 1;
    while L <= H do begin
      I := (L + H) shr 1;
      if Span[I].CumulativeSize < offset then
        L := I + 1
      else begin
        H := I - 1;
        if offset >= Span[I].CumulativeOffset then
          L := I;
      end;
    end;
    Result := L;
  end;
end; { TGpScatteredStream.LocateCumulativeOffset }

function TGpScatteredStream.Read(var buffer; count: integer): integer;
var
  dataRead: integer;
  pBuf    : PAnsiChar;
begin
  Result := 0;
  pBuf := @buffer;
  while count > 0 do begin
    if (ssSpanIdx >= CountSpans) and (ssoDataOnDemand in Options) and assigned(OnNeedMoreData) then 
      OnNeedMoreData(Self);
    if ssSpanIdx >= CountSpans then
      break; //while
    dataRead := Span[ssSpanIdx].CumulativeSize - ssCurrentPos;
    if dataRead > 0 then begin
      if dataRead > count then
        dataRead := count;
      WrappedStream.Position := Span[ssSpanIdx].FirstPos + ssSpanOffset;
      dataRead := WrappedStream.Read(pBuf^, dataRead);
      Inc(ssSpanOffset, dataRead);
      Inc(ssCurrentPos, dataRead);
      Inc(Result, dataRead);
      Dec(count, dataRead);
      Inc(pBuf, dataRead);
    end;
    if count = 0 then
      break; //while
    if (ssSpanIdx = (CountSpans - 1)) and (ssoDataOnDemand in Options) and assigned(OnNeedMoreData) then 
      OnNeedMoreData(Self);
    if ssSpanIdx = (CountSpans - 1) then
      break; //while
    Inc(ssSpanIdx);
    ssSpanOffset := 0;
  end; //while
end; { TGpScatteredStream.Read }

procedure TGpScatteredStream.RecalcCumulative(fromIndex: integer);
var
  spanIndex: integer;
begin
  for spanIndex := fromIndex to CountSpans - 1 do begin
    if spanIndex = 0 then
      Span[spanIndex].CumulativeOffset := 0
    else
      Span[spanIndex].CumulativeOffset := Span[spanIndex-1].CumulativeSize;
    Span[spanIndex].CumulativeSize := Span[spanIndex].CumulativeOffset + Span[spanIndex].Size;
  end;
end; { TGpScatteredStream.RecalcCumulative }

function TGpScatteredStream.Seek(const offset: int64; origin: TSeekOrigin): int64;
begin
  case origin of
    soBeginning:
      Result := InternalSeek(offset);
    soEnd:
      Result := InternalSeek(Size + offset);
    else
      Result := InternalSeek(ssCurrentPos + offset);
  end;
end; { TGpScatteredStream.Seek }

function TGpScatteredStream.Write(const buffer; count: integer): integer;
var
  dataWritten: integer;
  pBuf       : PAnsiChar;
begin
  Result := 0;
  pBuf := @buffer;
  while count > 0 do begin
    if (ssSpanIdx >= CountSpans) and (ssoDataOnDemand in Options) and assigned(OnNeedMoreData) then
      OnNeedMoreData(Self);
    if ssSpanIdx >= CountSpans then
      break; //while
    dataWritten := Span[ssSpanIdx].CumulativeSize - ssCurrentPos;
    if dataWritten > 0 then begin
      if dataWritten > count then
        dataWritten := count;
      WrappedStream.Position := Span[ssSpanIdx].FirstPos + ssSpanOffset;
      dataWritten := WrappedStream.Write(pBuf^, dataWritten);
      Inc(ssSpanOffset, dataWritten);
      Inc(ssCurrentPos, dataWritten);
      Inc(Result, dataWritten);
      Dec(count, dataWritten);
      Inc(pBuf, dataWritten);
    end;
    if count = 0 then
      break; //while
    if (ssSpanIdx = (CountSpans - 1)) and (ssoDataOnDemand in Options) and assigned(OnNeedMoreData) then
      OnNeedMoreData(Self);
    if ssSpanIdx = (CountSpans - 1) then
      break; //while
    Inc(ssSpanIdx);
    ssSpanOffset := 0;
  end; //while
end; { TGpScatteredStream.Write }

{ TGpInterceptorStream }

constructor TGpInterceptorStream.Create(readProc: TGpInterceptorReadProc;
  writeProc: TGpInterceptorWriteProc; seekProc: TGpInterceptorSeekProc;
  setSizeProc: TGpInterceptorSetSizeProc);
begin
  inherited Create;
  FReadProc := readProc;
  FWriteProc := writeProc;
  FSeekProc := seekProc;
  FSetSizeProc := setSizeProc;
end; { TGpInterceptorStream.Create }

function TGpInterceptorStream.Read(var buffer; count: integer): integer;
begin
  Result := FReadProc(FCurrentPos, buffer, count);
  Inc(FCurrentPos, Result);
end; { TGpInterceptorStream.Read }

function TGpInterceptorStream.Seek(const offset: int64; origin: TSeekOrigin): int64;
begin
  FCurrentPos := FSeekProc(offset, origin);
  Result := FCurrentPos;
end; { TGpInterceptorStream.Seek }

procedure TGpInterceptorStream.SetPosition(const value: int64);
begin
  FCurrentPos := FSeekProc(value, TSeekOrigin.soBeginning);
end; { TGpInterceptorStream.SetPosition }

procedure TGpInterceptorStream.SetSize(const newSize: int64);
begin
  FSetSizeProc(newSize);
  FCurrentPos := Seek(0, TSeekOrigin.soCurrent);
end; { TGpInterceptorStream.SetSize }

function TGpInterceptorStream.Write(const buffer; count: integer): integer;
begin
  Result := FWriteProc(FCurrentPos, buffer, count);
  Inc(FCurrentPos, Result);
end; { TGpInterceptorStream.Write }

{ TGpBufferedStream }

constructor TGpBufferedStream.Create(baseStream: TStream; bufferSize: integer;
  autoDestroyWrappedStream: boolean = false);
begin
  inherited Create(baseStream, autoDestroyWrappedStream);
  bsBaseSize := baseStream.Size;
  bsBufferSize := bufferSize;
  GetMem(bsBuffer, bsBufferSize);
  bsBufferPtr := bsBuffer;
  bsBufferData := 0;
  bsBasePosition := baseStream.Position;
  bsBufferOffset := bsBasePosition;
end; { TGpBufferedStream.Create }

destructor TGpBufferedStream.Destroy;
begin
  FreeMem(bsBuffer);
  inherited;
end; { TGpBufferedStream.Destroy }

function TGpBufferedStream.CurrentPosition: int64;
begin
  Result := (bsBufferPtr - bsBuffer) + bsBufferOffset;
end; { TGpBufferedStream.CurrentPosition }

function TGpBufferedStream.GetSize: int64;
begin
  // TODO 5 -oPrimoz Gabrijelcic : this will have to be updated when Write is implemented
  Result := bsBaseSize;
end; { TGpBufferedStream.GetSize }

function TGpBufferedStream.InternalSeek(offset: int64): int64;
var
  bufferEnd: int64;
begin
  bufferEnd := CurrentPosition + bsBufferData; //last offset in buffer
  if (offset < bsBufferOffset) or (offset > bufferEnd) then begin
    bsBufferData := 0;
    WrappedStream.Position := offset;
    bsBasePosition := offset;
    bsBufferPtr := bsBuffer;
    bsBufferOffset := offset;
  end
  else begin
    bsBufferPtr := bsBuffer + (offset - bsBufferOffset);
    bsBufferData := bufferEnd - CurrentPosition;
  end;
  Result := CurrentPosition;
end; { TGpBufferedStream.InternalSeek }

function TGpBufferedStream.Read(var buffer; count: integer): integer;
var
  canMove : integer;
  dataRead: integer;
  pOut    : PAnsiChar;
begin
  Result := 0;
  pOut := @buffer;
  while count > 0 do begin
    canMove := count;
    if bsBufferData < canMove then
      canMove := bsBufferData;
    if canMove > 0 then begin
      Move(bsBufferPtr^, pOut^, canMove);
      Inc(bsBufferPtr, canMove);
      Inc(pOut, canMove);
      Inc(Result, canMove);
      Dec(bsBufferData, canMove);
      Dec(count, canMove);
    end;
    if count >= bsBufferSize then begin
      bsBufferOffset := bsBasePosition;
      dataRead := WrappedStream.Read(pOut^, count);
      Inc(bsBasePosition, dataRead);
      Inc(Result, dataRead);
      break; //while
    end
    else if count > 0 then begin
      bsBufferOffset := bsBasePosition;
      bsBufferData := WrappedStream.Read(bsBuffer^, bsBufferSize);
      Inc(bsBasePosition, bsBufferData);
      bsBufferPtr := bsBuffer;
      if bsBufferData = 0 then begin
        break; //while
      end;
    end;
  end;
end; { TGpBufferedStream.Read }

function TGpBufferedStream.Seek(const offset: int64; origin: TSeekOrigin): int64;
begin
  case origin of
    soBeginning:
      Result := InternalSeek(offset);
    soEnd:
      Result := InternalSeek(WrappedStream.Size + offset);
    else
      Result := InternalSeek(CurrentPosition + offset);
  end;
end; { TGpBufferedStream.Seek }

function TGpBufferedStream.Write(const buffer; count: integer): integer;
var
  oldPos: int64;
begin
  oldPos := Position;
  WrappedStream.Position := oldPos;
  Result := WrappedStream.Write(buffer, count);
  bsBufferData := 0;
  bsBasePosition := oldPos + Result;
  bsBufferPtr := bsBuffer;
  bsBufferOffset := oldPos + Result;
end; { TGpBufferedStream.Write }

{ TGpJoinedStream }

constructor TGpJoinedStream.Create(ownsStreams: boolean = false);
begin
  inherited Create;
  jsStreamList := TObjectList.Create(ownsStreams);
  jsStartOffsets := TGpInt64List.Create;
end; { TGpJoinedStream.Create }

constructor TGpJoinedStream.Create(streams: array of TStream; ownsStreams: boolean =
  false);
var
  iStream: integer;
begin
  Create(ownsStreams);
  for iStream := Low(streams) to High(streams) do
    AddStream(streams[iStream]);
end; { TGpJoinedStream.Create }

destructor TGpJoinedStream.Destroy;
begin
  FreeAndNil(jsStartOffsets);
  FreeAndNil(jsStreamList);
  inherited;
end; { TGpJoinedStream.Destroy }

procedure TGpJoinedStream.AddStream(aStream: TStream);
begin
  jsStreamList.Add(aStream);
  if jsStreamList.Count = 1 then
    jsStartOffsets.Add(0)
  else begin
    jsButLastSize := jsStartOffsets[StreamCount-2] + Stream[StreamCount-2].Size;
    jsStartOffsets.Add(jsButLastSize);
  end;
end; { TGpJoinedStream.AddStream }

function TGpJoinedStream.CumulativeSize(idxStream: integer): int64;
begin
  Result := jsStartOffsets[idxStream] + Stream[idxStream].Size;
end; { TGpJoinedStream.CumulativeSize }

function TGpJoinedStream.GetSize: int64;
begin
  if StreamCount = 0 then
    Result := 0
  else
    Result := jsButLastSize + Stream[StreamCount - 1].Size;
end; { TGpJoinedStream.GetSize }

function TGpJoinedStream.GetStream(idxStream: integer): TStream;
begin
  Result := TStream(jsStreamList[idxStream]);
end; { TGpJoinedStream.GetStream }

function TGpJoinedStream.InternalSeek(offset: int64): int64;
begin
  if offset = 0 then begin
    jsStreamIdx := 0;
    jsStreamOffset := 0;
  end
  else begin
    jsStreamIdx := LocateCumulativeOffset(offset);
    if jsStreamIdx < 0 then
      raise Exception.CreateFmt('TGpJoinedStream.InternalSeek: Seek offset %d out of range', [offset]);
    jsStreamOffset := offset - jsStartOffsets[jsStreamIdx];
  end;
  jsCurrentPos := offset;
  Result := offset;
end; { TGpJoinedStream.InternalSeek }

function TGpJoinedStream.LocateCumulativeOffset(offset: int64): integer;
var
  L, H, I: integer;
begin
  if (offset < 0) or (StreamCount = 0) or (offset > CumulativeSize(StreamCount-1)) then
    Result := -1
  else begin
    L := 0;
    H := StreamCount - 1;
    while L <= H do begin
      I := (L + H) shr 1;
      if CumulativeSize(I) < offset then
        L := I + 1
      else begin
        H := I - 1;
        if offset >= CumulativeSize(I) then
          L := I;
      end;
    end;
    Result := L;
  end;
end; { TGpJoinedStream.LocateCumulativeOffset }

function TGpJoinedStream.Read(var buffer; count: integer): integer;
var
  dataRead: int64;
  pBuf    : PAnsiChar;
begin
  Result := 0;
  pBuf := @buffer;
  while count > 0 do begin
    if jsStreamIdx >= StreamCount then
      break; //while
    dataRead := CumulativeSize(jsStreamIdx) - jsCurrentPos;
    if dataRead > 0 then begin
      if dataRead > count then
        dataRead := count;
      Stream[jsStreamIdx].Position := jsStreamOffset;
      dataRead := Stream[jsStreamIdx].Read(pBuf^, dataRead);
      Inc(jsStreamOffset, dataRead);
      Inc(jsCurrentPos, dataRead);
      Inc(Result, dataRead);
      Dec(count, dataRead);
      Inc(pBuf, dataRead);
    end;
    if count = 0 then
      break; //while
    if jsStreamIdx = (StreamCount - 1) then
      break; //while
    Inc(jsStreamIdx);
    jsStreamOffset := 0;
  end; //while
end; { TGpJoinedStream.Read }

procedure TGpJoinedStream.RemoveProcessedStreams;
begin
  while (StreamCount > 0) and (jsCurrentPos >= Stream[0].Size) do
    RemoveStream(0);
end; { TGpJoinedStream.RemoveProcessedStreams }

procedure TGpJoinedStream.RemoveStream(aStream: TStream; failIfContainsPosition: boolean);
begin
  RemoveStream(jsStreamList.IndexOf(aStream));
end; { TGpJoinedStream.RemoveStream }

procedure TGpJoinedStream.RemoveStream(streamIdx: integer; failIfContainsPosition: boolean);
var
  iStream: integer;
begin
  if (jsCurrentPos >= jsStartOffsets[streamIdx]) and
     (jsCurrentPos < (jsStartOffsets[streamIdx] + Stream[streamIdx].Size)) then
  begin
    if failIfContainsPosition then
      raise Exception.Create('TGpJoinedStream.RemoveStream: Cannot remove active stream')
    else if streamIdx < (StreamCount - 1) then // move position to the start of the next stream
      Position := jsStartOffsets[streamIdx+1]
    else if StreamCount = 1 then // list will be empty
      Position := 0
    else // last stream, move after the end of the previous stream
      Position := jsStartOffsets[streamIdx];
  end;
  with KeepStreamPosition(Self) do begin
    jsStreamList.Delete(streamIdx);
    jsStartOffsets.Delete(streamIdx);
  end;
  jsButLastSize := 0;
  for iStream := 0 to StreamCount - 2 do
    Inc(jsButLastSize, Stream[iStream].Size);
end; { TGpJoinedStream.RemoveStream }

function TGpJoinedStream.Seek(const offset: int64; origin: TSeekOrigin): int64;
begin
  case origin of
    soBeginning:
      Result := InternalSeek(offset);
    soEnd:
      Result := InternalSeek(Size + offset);
    else
      Result := InternalSeek(jsCurrentPos + offset);
  end;
end; { TGpJoinedStream.Seek }

function TGpJoinedStream.StreamCount: integer;
begin
  Result := jsStreamList.Count;
end; { TGpJoinedStream.StreamCount }

function TGpJoinedStream.Write(const buffer; count: integer): integer;
var
  dataWritten: int64;
  pBuf       : PAnsiChar;
begin
  Result := 0;
  pBuf := @buffer;
  while count > 0 do begin
    if jsStreamIdx >= StreamCount then
      break; //while
    dataWritten := CumulativeSize(jsStreamIdx) - jsCurrentPos;
    if dataWritten > 0 then begin
      if dataWritten > count then
        dataWritten := count;
      Stream[jsStreamIdx].Position := jsStreamOffset;
      dataWritten := Stream[jsStreamIdx].Write(pBuf^, dataWritten);
      Inc(jsStreamOffset, dataWritten);
      Inc(jsCurrentPos, dataWritten);
      Inc(Result, dataWritten);
      Dec(count, dataWritten);
      Inc(pBuf, dataWritten);
    end;
    if count = 0 then
      break; //while
    if jsStreamIdx = (StreamCount - 1) then
      break; //while
    Inc(jsStreamIdx);
    jsStreamOffset := 0;
  end; //while
end; { TGpJoinedStream.Write }

{$IFDEF GpStreams_ClassHelpers}
{ TGpStreamEnhancer }

procedure TGpStreamEnhancer.Advance(deltaPosition: int64);
begin
  Position := Position + deltaPosition;
end; { TGpStreamEnhancer.Advance }

///<summary>Appends full contents of the source stream to the end of Self.
///   <para>Uses CopyStream instead of CopyFrom to support TGpScatteredStream.</para></summary>
///<since>2007-10-17</since>
procedure TGpStreamEnhancer.Append(source: TStream);
begin
  Position := Size;
  CopyStream(source, Self, 0);
end; { TGpStreamEnhancer.Append }

function TGpStreamEnhancer.BE_Read24bits(var w24: DWORD): boolean;
var
  hi: byte;
  lo: word;
begin
  Result := BE_ReadByte(hi);
  if Result then
    Result := BE_ReadWord(lo);
  if Result then begin
    LongRec(w24).Hi := hi;
    LongRec(w24).Lo := lo;
  end;
end; { TGpStreamEnhancer.BE_Read24bits }

function TGpStreamEnhancer.BE_Read24bits: DWORD;
begin
  LongRec(Result).Hi := BE_ReadByte;
  LongRec(Result).Lo := BE_ReadWord;
end; { TGpStreamEnhancer.BE_Read24bits }

function TGpStreamEnhancer.BE_Read48bits: int64;
begin
  Int64Rec(Result).Hi := BE_ReadWord;
  Int64Rec(Result).Lo := BE_ReadDWord;
end; { TGpStreamEnhancer.BE_Read48bits }

function TGpStreamEnhancer.BE_Read48bits(var h: int64): boolean;
var
  hi: word;
  lo: DWORD;
begin
  Result := BE_ReadWord(hi);
  if Result then
    Result := BE_ReadDWord(lo);
  if Result then begin
    Int64Rec(h).Hi := hi;
    Int64Rec(h).Lo := lo;
  end;
end; { TGpStreamEnhancer.BE_Read48bits }

function TGpStreamEnhancer.BE_ReadByte(var b: byte): boolean;
begin
  Result := (Read(b, 1) = 1);
end; { TGpStreamEnhancer.BE_ReadByte }

function TGpStreamEnhancer.BE_ReadByte: byte;
begin
  ReadBuffer(Result, 1);
end; { TGpStreamEnhancer.BE_ReadByte }

function TGpStreamEnhancer.BE_ReadDWord(var dw: DWORD): boolean;
var
  hi: word;
  lo: word;
begin
  Result := BE_ReadWord(hi);
  if Result then
    Result := BE_ReadWord(lo);
  if Result then begin
    LongRec(dw).Hi := hi;
    LongRec(dw).Lo := lo;
  end;
end; { TGpStreamEnhancer.BE_ReadDWord }

function TGpStreamEnhancer.BE_ReadDWord: DWORD;
begin
  LongRec(Result).Hi := BE_ReadWord;
  LongRec(Result).Lo := BE_ReadWord;
end; { TGpStreamEnhancer.BE_ReadDWord }

function TGpStreamEnhancer.BE_ReadGUID(var guid: TGUID): boolean;
var
  b : byte;
  d1: DWORD;
  d2: word;
  d3: word;
  i : integer;
begin
  Result := BE_ReadDWord(d1);
  if Result then
    Result := BE_ReadWord(d2);
  if Result then
    Result := BE_ReadWord(d3);
  if Result then begin
    guid.D1 := d1;
    guid.D2 := d2;
    guid.D3 := d3;
    for i := 0 to 7 do begin
      if not BE_ReadByte(b) then
        Exit;
      guid.D4[i] := b;
    end;
  end;
end; { TGpStreamEnhancer.BE_ReadGUID }

function TGpStreamEnhancer.BE_ReadGUID: TGUID;
var
  i: integer;
begin
  Result.D1 := BE_ReadDWord;
  Result.D2 := BE_ReadWord;
  Result.D3 := BE_ReadWord;
  for i := 0 to 7 do
    Result.D4[i] := BE_ReadByte;
end; { TGpStreamEnhancer.BE_ReadGUID }

function TGpStreamEnhancer.BE_ReadHuge(var h: int64): boolean;
var
  hi: DWORD;
  lo: DWORD;
begin
  Result := BE_ReadDWord(hi);
  if Result then
    Result := BE_ReadDWord(lo);
  if Result then begin
    Int64Rec(h).Hi := hi;
    Int64Rec(h).Lo := lo;
  end;
end; { TGpStreamEnhancer.BE_ReadHuge }

function TGpStreamEnhancer.BE_ReadHuge: int64;
begin
  Int64Rec(Result).Hi := BE_ReadDWord;
  Int64Rec(Result).Lo := BE_ReadDWord;
end; { TGpStreamEnhancer.BE_ReadHuge }

function TGpStreamEnhancer.BE_ReadWord(var w: word): boolean;
var
  lo: byte;
  hi: byte;
begin
  Result := BE_ReadByte(hi);
  if Result then
    Result := BE_ReadByte(lo);
  if Result then begin
    WordRec(w).Hi := hi;
    WordRec(w).Lo := lo;
  end;
end; { TGpStreamEnhancer.BE_ReadWord }

function TGpStreamEnhancer.BE_ReadWord: word;
begin
  WordRec(Result).Hi := BE_ReadByte;
  WordRec(Result).Lo := BE_ReadByte;
end; { TGpStreamEnhancer.BE_ReadWord }

procedure TGpStreamEnhancer.BE_Write24bits(const dw: DWORD);
begin
  BE_WriteByte(LongRec(dw).Hi);
  BE_WriteWord(LongRec(dw).Lo);
end; { TGpStreamEnhancer.BE_Write24bits }

procedure TGpStreamEnhancer.BE_Write48bits(const h: int64);
begin
  BE_WriteWord(Int64Rec(h).Hi);
  BE_WriteDWord(Int64Rec(h).Lo);
end; { TGpStreamEnhancer.BE_Write48bits }

procedure TGpStreamEnhancer.BE_WriteByte(const b: byte);
begin
  WriteBuffer(b, 1);
end; { TGpStreamEnhancer.BE_WriteByte }

procedure TGpStreamEnhancer.BE_WriteDWord(const dw: DWORD);
begin
  BE_WriteWord(LongRec(dw).Hi);
  BE_WriteWord(LongRec(dw).Lo);
end; { TGpStreamEnhancer.BE_WriteDWord }

procedure TGpStreamEnhancer.BE_WriteGUID(const g: TGUID);
var
  i: integer;
begin
  BE_WriteDWord(g.D1);
  BE_WriteWord(g.D2);
  BE_WriteWord(g.D3);
  for i := 0 to 7 do
    BE_WriteByte(g.D4[i]);
end; { TGpStreamEnhancer.BE_WriteGUID }

procedure TGpStreamEnhancer.BE_WriteHuge(const h: Int64);
var
  tmp: int64;
begin
  tmp := h;
  BE_WriteDWord(Int64Rec(tmp).Hi);
  BE_WriteDWord(Int64Rec(tmp).Lo);
end; { TGpStreamEnhancer.BE_WriteHuge }

procedure TGpStreamEnhancer.BE_WriteWord(const w: word);
begin
  BE_WriteByte(WordRec(w).Hi);
  BE_WriteByte(WordRec(w).Lo);
end; { TGpStreamEnhancer.BE_WriteWord }

procedure TGpStreamEnhancer.Clear;
begin
  Size := 0;
end; { TGpStreamEnhancer.Clear }

function TGpStreamEnhancer.AtEnd: boolean;
begin
  Result := (BytesLeft = 0);
end; { TGpStreamEnhancer.AtEnd }

function TGpStreamEnhancer.AtStart: boolean;
begin
  Result := (Position = 0);
end; { TGpStreamEnhancer.AtStart }

function TGpStreamEnhancer.BytesLeft: int64;
begin
  Result := (Size - Position);
end; { TGpStreamEnhancer.BytesLeft }

function TGpStreamEnhancer.CheckTag(tag: integer): boolean;
var
  strTag: integer;
begin
  Result := ReadTag(strTag) and (strTag = tag);
end; { TGpStreamEnhancer.CheckTag }

function TGpStreamEnhancer.GetAsAnsiString: AnsiString;
begin
  SetLength(Result, Size);
  if Length(Result) > 0 then
    with KeepStreamPosition(Self, 0) do
      Read(Result[1], Length(Result));
end; { TGpStreamEnhancer.GetAsAnsiString }

function TGpStreamEnhancer.GetAsHexString: string;
var
  b   : byte;
  i   : integer;
  pRes: PChar;
const
  CHexChar: string = '0123456789ABCDEF';
begin
  SetLength(Result, Size*2);
  if Size > 0 then
    with KeepStreamPosition(Self, 0) do begin
      pRes := @Result[1];
      for i := 1 to Size do begin
        ReadBuffer(b, 1);
        pRes^ := CHexChar[(b SHR 4) + 1];
        Inc(pRes);
        pRes^ := CHexChar[(b AND $0F) + 1];
        Inc(pRes);
      end;
    end;
end; { TGpStreamEnhancer.GetAsHexString }

function TGpStreamEnhancer.GetAsString: string;
begin
  SetLength(Result, Size div SizeOf(char));
  if Length(Result) > 0 then
    with KeepStreamPosition(Self, 0) do
      Read(Result[1], Length(Result)*SizeOf(char));
end; { TGpStreamEnhancer.GetAsString }

procedure TGpStreamEnhancer.GoToEnd;
begin
  Position := Size;
end; { TGpStreamEnhancer.GoToEnd }

function TGpStreamEnhancer.IsEmpty: boolean;
begin
  Result := (Size = 0);
end; { TGpStreamEnhancer.IsEmpty }

function TGpStreamEnhancer.GetTag(var tag: integer; var stream: IGpStreamWrapper): boolean;
var
  dataPos : int64;
  dataSize: integer;
begin
  stream := nil;
  Result := PeekTag(tag, dataPos, dataSize);
  if Result then begin
    SkipTag;
    stream := AutoDestroyStream(TGpStreamWindow.Create(Self, dataPos, dataPos + dataSize - 1));
    stream.Stream.GoToStart;
  end;
end; { TGpStreamEnhancer.GetTag }

procedure TGpStreamEnhancer.GoToStart;
begin
  Position := 0;
end; { TGpStreamEnhancer.GoToStart }

procedure TGpStreamEnhancer.KeepLast(numBytes: integer);
begin
  RemoveFirst(Size - numBytes);
end; { TGpStreamEnhancer.KeepLast }

function TGpStreamEnhancer.LE_Read24bits: DWORD;
begin
  Result := 0;
  ReadBuffer(Result, 3);
end; { TGpStreamEnhancer.LE_Read24bits }

function TGpStreamEnhancer.LE_Read48bits: int64;
begin
  Result := 0;
  ReadBuffer(Result, 6);
end; { TGpStreamEnhancer.LE_Read48bits }

function TGpStreamEnhancer.LE_ReadByte: byte;
begin
  ReadBuffer(Result, 1);
end; { TGpStreamEnhancer.LE_ReadByte }

function TGpStreamEnhancer.LE_ReadDWord: DWORD;
begin
  ReadBuffer(Result, SizeOf(DWORD));
end; { TGpStreamEnhancer.LE_ReadDWord }

function TGpStreamEnhancer.LE_ReadGUID: TGUID;
var
  i: integer;
begin
  Result.D1 := LE_ReadDWord;
  Result.D2 := LE_ReadWord;
  Result.D3 := LE_ReadWord;
  for i := 0 to 7 do
    Result.D4[i] := LE_ReadByte;
end; { TGpStreamEnhancer.LE_ReadGUID }

function TGpStreamEnhancer.LE_ReadHuge: int64;
begin
  ReadBuffer(Result, SizeOf(int64));
end; { TGpStreamEnhancer.LE_ReadHuge }

function TGpStreamEnhancer.LE_ReadWord: word;
begin
  ReadBuffer(Result, SizeOf(word));
end; { TGpStreamEnhancer.LE_ReadWord }

procedure TGpStreamEnhancer.LE_Write24bits(const dw: DWORD);
begin
  WriteBuffer(dw, 3);
end; { TGpStreamEnhancer.LE_Write24bits }

procedure TGpStreamEnhancer.LE_Write48bits(const h: int64);
begin
  WriteBuffer(h, 6);
end; { TGpStreamEnhancer.LE_Write48bits }

procedure TGpStreamEnhancer.LE_WriteByte(const b: byte);
begin
  WriteBuffer(b, 1);
end; { TGpStreamEnhancer.LE_WriteByte }

procedure TGpStreamEnhancer.LE_WriteDWord(const dw: DWORD);
begin
  WriteBuffer(dw, SizeOf(DWORD));
end; { TGpStreamEnhancer.LE_WriteDWord }

procedure TGpStreamEnhancer.LE_WriteGUID(const g: TGUID);
var
  i: integer;
begin
  LE_WriteDWord(g.D1);
  LE_WriteWord(g.D2);
  LE_WriteWord(g.D3);
  for i := 0 to 7 do
    LE_WriteByte(g.D4[i]);
end; { TGpStreamEnhancer.LE_WriteGUID }

procedure TGpStreamEnhancer.LE_WriteHuge(const h: Int64);
var
  tmp: int64;
begin
  tmp := h;
  WriteBuffer(tmp, SizeOf(int64));
end; { TGpStreamEnhancer.LE_WriteHuge }

procedure TGpStreamEnhancer.LE_WriteWord(const w: word);
begin
  WriteBuffer(w, SizeOf(word));
end; { TGpStreamEnhancer.LE_WriteWord }

procedure TGpStreamEnhancer.LoadFromFile(const fileName: string);
var
  strFile: TFileStream;
begin
  strFile := TFileStream.Create(fileName, fmOpenRead);
  try
    Position := 0;
    CopyFrom(strFile, 0);
  finally FreeAndNil(strFile); end;
end; { TGpStreamEnhancer.LoadFromFile }

{:Reads a tag from the stream and returns it. Does not change stream position.
  Returns False if there is not enough stream left to contain tag ID, tag
  length, and tag data.
}
function TGpStreamEnhancer.PeekTag(var tag: integer): boolean;
var
  dataPos : int64;
  dataSize: integer;
begin
  Result := PeekTag(tag, dataPos, dataSize);
end; { TGpStreamEnhancer.PeekTag }

function TGpStreamEnhancer.PeekTag(var tag: integer; var dataPos: int64; var dataSize:
  integer): boolean;
begin
  Result := false;
  with KeepStreamPosition(Self) do begin
    if (Read(tag, SizeOf(tag)) <> SizeOf(tag)) or
       (Read(dataSize, SizeOf(dataSize)) <> SizeOf(dataSize))
    then
      Exit;
    dataPos := Position;
    Result := ((Position + dataSize) <= Self.Size);
  end;
end; { TGpStreamEnhancer.PeekTag }

{:Read the tag. Size must be zero or error will be returned.
  @since   2006-09-22
}
function TGpStreamEnhancer.ReadTag(var tag: integer): boolean;
begin
  Result := PeekTag(tag);
  if Result then
    Result := ReadTag(tag, 0, tag);
end; { TGpStreamEnhancer.ReadTag }

{:Reads boolean data from a stream. Returns false if stream ends prematurely or
  if tag in the stream is invalid. Keeps position if tag is invalid, positions
  after the read data otherwise.
}
function TGpStreamEnhancer.ReadTag(tag: integer; var data: boolean): boolean;
begin
  Result := ReadTag(tag, SizeOf(data), data);
end; { TGpStreamEnhancer.ReadTag }

{:Reads integer data from a stream. Returns false if stream ends prematurely or
  if tag in the stream is invalid. Keeps position if tag is invalid, positions
  after the read data otherwise.
  @returns True only if tag is correct and full tagged data was present in the
           stream.
}
function TGpStreamEnhancer.ReadTag(tag: integer; var data: integer): boolean;
begin
  Result := ReadTag(tag, SizeOf(data), data);
end; { TGpStreamEnhancer.ReadTag }

{:Reads untyped data from a stream. Returns false if stream ends
  prematurely or if tag in the stream is invalid. Keeps position if tag is
  invalid, positions after the read data otherwise.
}
function TGpStreamEnhancer.ReadTag(tag, size: integer; var buf): boolean;
var
  oldPos: int64;
  stSize: integer;
  stTag : integer;
begin
  Result := false;
  oldPos := Position;
  if Read(stTag, SizeOf(stTag)) = SizeOf(stTag) then begin
    if stTag <> tag then
      Position := oldPos
    else if Read(stSize, SizeOf(stSize)) = SizeOf(stSize) then begin
      if stSize = size then begin
        if size > 0 then
          Result := (Read(buf, size) = size)
        else
          Result := true;
      end
      else
        Position := Position + stSize;
    end;
  end;
end; { TGpStreamEnhancer.ReadTag }

{:Reads string data from a stream. Returns false if stream ends prematurely or
  if tag in the stream is invalid. Keeps position if tag is invalid, positions
  after the read data otherwise.
}
function TGpStreamEnhancer.ReadTag(tag: integer; var data: AnsiString): boolean;
var
  oldPos: int64;
  stSize: integer;
  stTag : integer;
begin
  Result := false;
  oldPos := Position;
  if Read(stTag, SizeOf(stTag)) = SizeOf(stTag) then begin
    if stTag <> tag then
      Position := oldPos
    else if Read(stSize, SizeOf(stSize)) = SizeOf(stSize) then begin
      SetLength(data, stSize);
      if stSize > 0 then
        Result := (Read(data[1], stSize) = stSize)
      else
        Result := true;
    end;
  end;
end; { TGpStreamEnhancer.ReadTag }

function TGpStreamEnhancer.ReadTag(tag: integer; var data: WideString): boolean;
var
  oldPos: int64;
  stSize: integer;
  stTag : integer;
begin
  Result := false;
  oldPos := Position;
  if Read(stTag, SizeOf(stTag)) = SizeOf(stTag) then begin
    if stTag <> tag then
      Position := oldPos
    else if Read(stSize, SizeOf(stSize)) = SizeOf(stSize) then begin
      SetLength(data, stSize div SizeOf(WideChar));
      if stSize > 0 then
        Result := (Read(data[1], stSize) = stSize)
      else
        Result := true;
    end;
  end;
end; { TGpStreamEnhancer.ReadTag }

{:Reads date-time data from a stream. Returns false if stream ends prematurely
  or if tag in the stream is invalid. Keeps position if tag is invalid,
  positions after the read data otherwise.
}
function TGpStreamEnhancer.ReadTag(tag: integer; var data: TDateTime): boolean;
begin
  Result := ReadTag(tag, SizeOf(data), data);
end; { TGpStreamEnhancer.ReadTag }

{:Reads untyped data from a stream. Returns false if stream ends
  prematurely or if tag in the stream is invalid. Keeps position if tag is
  invalid, positions after the read data otherwise.
}
function TGpStreamEnhancer.ReadTag(tag: integer; data: TStream): boolean;
var
  oldPos: int64;
  stSize: integer;
  stTag : integer;
begin
  Result := false;
  oldPos := Position;
  if Read(stTag, SizeOf(stTag)) = SizeOf(stTag) then begin
    if stTag <> tag then
      Position := oldPos
    else if Read(stSize, SizeOf(stSize)) = SizeOf(stSize) then begin
      if stSize > 0 then
        Result := (data.CopyFrom(Self, stSize) = stSize)
      else
        Result := true;
    end;
  end;
end; { TGpStreamEnhancer.ReadTag }

{:Reads untyped data from a stream. Returns false if stream ends
  prematurely or if tag in the stream is invalid. Keeps position if tag is
  invalid, positions after the read data otherwise.
}
function TGpStreamEnhancer.ReadTag(tag: integer; var data: IGpBuffer): boolean;
begin
  data := TGpBuffer.Create;
  Result := ReadTag(tag, data.AsStream);
  if not Result then
    data := nil;
end; { TGpStreamEnhancer.ReadTag }

function TGpStreamEnhancer.ReadTag64(tag: integer; var data: int64): boolean;
begin
  Result := ReadTag(tag, SizeOf(data), data);
end; { TGpStreamEnhancer.ReadTag64 }

procedure TGpStreamEnhancer.RemoveFirst(numBytes: integer);
begin
  Assert(Self is TMemoryStream);
  if (numBytes < 0) or (numBytes > Size) then
    raise Exception.CreateFmt(
            'TGpStreamEnhancer.RemoveFirst: Cannot remove %d bytes, size = %d',
            [numBytes, Size]);
  if numBytes = 0 then
    Exit;
  if numBytes = Size then
    Clear
  else begin
    Move(OffsetPtr(TMemoryStream(Self).Memory, numBytes)^,
         TMemoryStream(Self).Memory^, Size - numBytes);
    Size := Size - numBytes;
  end;
end; { TGpStreamEnhancer.RemoveFirst }

procedure TGpStreamEnhancer.SaveToFile(const fileName: string);
var
  strFile: TFileStream;
begin
  strFile := TFileStream.Create(fileName, fmCreate);
  try
    strFile.CopyFrom(Self, 0);
  finally FreeAndNil(strFile); end;
end; { TGpStreamEnhancer.SaveToFile }

procedure TGpStreamEnhancer.SetAsAnsiString(const value: AnsiString);
begin
  Size := Length(value);
  if Size > 0 then begin
    Position := 0;
    Write(value[1], Size);
    Position := 0;
  end;
end; { TGpStreamEnhancer.SetAsAnsiString }

procedure TGpStreamEnhancer.SetAsString(const value: string);
begin
  Size := Length(value) * SizeOf(Char);
  if Size > 0 then begin
    Position := 0;
    Write(value[1], Size);
    Position := 0;
  end;
end; { TGpStreamEnhancer.SetAsString }

{:Skips over the next tagged data.
}
procedure TGpStreamEnhancer.SkipTag;
var
  size: integer;
  tag : integer;
begin
  if (Read(tag, SizeOf(tag)) = SizeOf(tag)) and
     (Read(size, SizeOf(size)) = SizeOf(size))
  then
    Position := Position + size;
end; { TGpStreamEnhancer.SkipTag }

procedure TGpStreamEnhancer.WriteAnsiStr(const s: AnsiString);
begin
  if s <> '' then
    Write(s[1], Length(s));
end; { TGpStreamEnhancer.WriteAnsiStr }

procedure TGpStreamEnhancer.WriteStr(const s: string);
begin
  if s <> '' then
    Write(s[1], Length(s) * SizeOf(Char));
end; { TGpStreamEnhancer.WriteStr }

procedure TGpStreamEnhancer.WriteTag(tag: integer);
begin
  WriteTag(tag, 0, tag);
end; { TGpStreamEnhancer.WriteTag }

{:Writes tagged boolean.
}
procedure TGpStreamEnhancer.WriteTag(tag: integer; data: boolean);
begin
  WriteTag(tag, SizeOf(data), data);
end; { TGpStreamEnhancer.WriteTag }

{:Writes tagged integer.
}
procedure TGpStreamEnhancer.WriteTag(tag: integer; data: integer);
begin
  WriteTag(tag, SizeOf(data), data);
end; { TGpStreamEnhancer.WriteTag }

{:Writes tagged untyped buffer.
}
procedure TGpStreamEnhancer.WriteTag(tag, size: integer; const buf);
begin
  Write(tag, SizeOf(tag));
  Write(size, SizeOf(size));
  if size > 0 then
    Write(buf, size);
end; { TGpStreamEnhancer.WriteTag }

{:Writes tagged string.
}
procedure TGpStreamEnhancer.WriteTag(tag: integer; data: AnsiString);
var
  size: integer;
begin
  Write(tag, SizeOf(tag));
  size := Length(data);
  Write(size, SizeOf(size));
  if size > 0 then
    Write(data[1], size);
end; { TGpStreamEnhancer.WriteTag }

{:Writes tagged date-time.
}
procedure TGpStreamEnhancer.WriteTag(tag: integer; data: TDateTime);
var
  tmp: TDateTime;
begin
  tmp := data;
  WriteTag(tag, SizeOf(tmp), tmp);
end; { TGpStreamEnhancer.WriteTag }

procedure TGpStreamEnhancer.WriteTag(tag: integer; data: WideString);
var
  size: integer;
begin
  Write(tag, SizeOf(tag));
  size := Length(data) * SizeOf(WideChar);
  Write(size, SizeOf(size));
  if size > 0 then
    Write(data[1], size);
end; { TGpStreamEnhancer.WriteTag }

procedure TGpStreamEnhancer.Writeln(const s: string);
begin
  if s <> '' then
    WriteStr(s);
  WriteStr(#13#10);
end; { TGpStreamEnhancer.Writeln }

procedure TGpStreamEnhancer.WritelnAnsi(const s: AnsiString);
begin
  if s <> '' then
    WriteAnsiStr(s);
  WriteAnsiStr(#13#10);
end; { TGpStreamEnhancer.WritelnAnsi }

procedure TGpStreamEnhancer.WritelnWide(const s: WideString);
begin
  if s <> '' then
    WriteWideStr(s);
  WriteWideStr(#13#10);
end; { TGpStreamEnhancer.WritelnWide }

procedure TGpStreamEnhancer.WriteRawStr(const s: RawByteString);
begin
  if s <> '' then
    Write(s[1], Length(s));
end; { TGpStreamEnhancer.WriteRawStr }

{:Writes tagged stream.
}
procedure TGpStreamEnhancer.WriteTag(tag: integer; data: TStream; startAt: int64);
var
  size: integer;
begin
  Write(tag, SizeOf(tag));
  if startAt >= 0 then
    data.Position := startAt;
  size := data.Size - data.Position;
  Write(size, SizeOf(size));
  if size > 0 then
    CopyFrom(data, size);
end; { TGpStreamEnhancer.WriteTag }

{:Writes tagged buffer.
}
procedure TGpStreamEnhancer.WriteTag(tag: integer; const data: IGpBuffer);
begin
  WriteTag(tag, data.AsStream, 0);
end; { TGpStreamEnhancer.WriteTag }

procedure TGpStreamEnhancer.WriteTag64(tag: integer; data: int64);
var
  tmp: int64;
begin
  tmp := data;
  WriteTag(tag, SizeOf(tmp), tmp);
end; { TGpStreamEnhancer.WriteTag64 }

procedure TGpStreamEnhancer.WriteWideStr(const s: WideString);
begin
  if s <> '' then
    Write(s[1], Length(s)*SizeOf(WideChar));
end; { TGpStreamEnhancer.WriteWideStr }
{$ENDIF GpStreams_ClassHelpers}

{ TGpDoNothingStreamWrapper }

constructor TGpDoNothingStreamWrapper.Create(stream: TStream);
begin
  inherited Create;
  FStream := stream;
end; { TGpDoNothingStreamWrapper.Create }

destructor TGpDoNothingStreamWrapper.Destroy;
begin
  Restore;
  inherited;
end; { TGpDoNothingStreamWrapper.Destroy }

function TGpDoNothingStreamWrapper.GetStream: TStream;
begin
  Result := FStream;
end; { TGpDoNothingStreamWrapper.GetStream }

procedure TGpDoNothingStreamWrapper.Restore;
begin
  // do nothing
end; { TGpDoNothingStreamWrapper.Restore }

procedure TGpDoNothingStreamWrapper.SetStream(stream: TStream);
begin
  FStream := stream;
end; { TGpDoNothingStreamWrapper.SetStream }

{ TGpAutoDestroyStreamWrapper }

procedure TGpAutoDestroyStreamWrapper.Restore;
begin
  FreeAndNil(FStream);
end; { TGpAutoDestroyStreamWrapper.Restore }

{ TGpKeepStreamPositionWrapper }

constructor TGpKeepStreamPositionWrapper.Create(managedStream: TStream);
begin
  inherited Create(managedStream);
  kspwOriginalPosition := managedStream.Position;
end; { TGpKeepStreamPositionWrapper.Create }

procedure TGpKeepStreamPositionWrapper.Restore;
begin
  if not assigned(Stream) then
    Exit;
  Stream.Position := kspwOriginalPosition;
  Stream := nil;
end; { TGpKeepStreamPositionWrapper.Restore }

{ TGpFileStream }

constructor TGpFileStream.Create(const fileName: string; fileHandle: THandle);
begin
  gfsFileName := fileName;
  inherited Create(fileHandle);
end; { TGpFileStream.Create }

destructor TGpFileStream.Destroy;
begin
  if Handle <> 0 then
    CloseHandle(Handle);
  inherited;
end; { TGpFileStream.Destroy }

{ TAnsiStringStream }

constructor TAnsiStringStream.Create(const value: AnsiString);
begin
  inherited Create;
  FDataString := value;
end; { TAnsiStringStream.Create }

function TAnsiStringStream.Read(var buffer; count: longint): longint;
begin
  Result := Length(FDataString) - FPosition;
  if Result > count then
    Result := count;
  if Result > 0 then
    Move(FDataString[FPosition+1], buffer, Result);
  Inc(FPosition, Result);
end; { TAnsiStringStream.Read }

function TAnsiStringStream.Write(const buffer; count: longint): longint;
begin
  Result := count;
  SetLength(FDataString, (FPosition + Result));
  if Result > 0 then
    Move(buffer, FDataString[FPosition+1], Result);
  Inc(FPosition, Result);
end; { TAnsiStringStream.Write }

function TAnsiStringStream.Seek(offset: longint; origin: word): longint;
begin
  case origin of
    soFromBeginning: FPosition := offset;
    soFromCurrent: FPosition := FPosition + offset;
    soFromEnd: FPosition := Length(FDataString) - offset;
  end;
  if FPosition > Length(FDataString) then
    FPosition := Length(FDataString)
  else if FPosition < 0 then
    FPosition := 0;
  Result := FPosition;
end; { TAnsiStringStream.Seek }

function TAnsiStringStream.ReadString(count: longint): AnsiString;
var
  len: integer;
begin
  len := Length(FDataString) - FPosition;
  if len > count then
    len := count;
  SetLength(Result, len);
  if len > 0 then
    Read(Result[1], len);
end; { TAnsiStringStream.ReadString }

procedure TAnsiStringStream.WriteString(const value: AnsiString);
begin
  Write(PAnsiChar(value)^, Length(value));
end; { TAnsiStringStream.WriteString }

procedure TAnsiStringStream.SetSize(newSize: longint);
begin
  SetLength(FDataString, newSize);
  if FPosition > newSize then
    FPosition := newSize;
end; { TAnsiStringStream.SetSize }

{ TRawByteStringStream }

constructor TRawByteStringStream.Create(const value: RawByteString);
begin
  inherited Create;
  FDataString := value;
end; { TRawByteStringStream.Create }

function TRawByteStringStream.Read(var buffer; count: longint): longint;
begin
  Result := Length(FDataString) - FPosition;
  if Result > count then
    Result := count;
  if Result > 0 then
    Move(FDataString[FPosition+1], buffer, Result);
  Inc(FPosition, Result);
end; { TRawByteStringStream.Read }

function TRawByteStringStream.Write(const buffer; count: longint): longint;
begin
  Result := count;
  SetLength(FDataString, (FPosition + Result));
  if Result > 0 then
    Move(buffer, FDataString[FPosition+1], Result);
  Inc(FPosition, Result);
end; { TRawByteStringStream.Write }

function TRawByteStringStream.Seek(offset: longint; origin: word): longint;
begin
  case origin of
    soFromBeginning: FPosition := offset;
    soFromCurrent: FPosition := FPosition + offset;
    soFromEnd: FPosition := Length(FDataString) - offset;
  end;
  if FPosition > Length(FDataString) then
    FPosition := Length(FDataString)
  else if FPosition < 0 then
    FPosition := 0;
  Result := FPosition;
end; { TRawByteStringStream.Seek }

function TRawByteStringStream.ReadString(count: longint): RawByteString;
var
  len: integer;
begin
  len := Length(FDataString) - FPosition;
  if len > count then
    len := count;
  SetLength(Result, len);
  if len > 0 then
    Read(Result[1], len);
end; { TRawByteStringStream.ReadString }

procedure TRawByteStringStream.WriteString(const value: RawByteString);
begin
  Write(PAnsiChar(value)^, Length(value));
end; { TRawByteStringStream.WriteString }

procedure TRawByteStringStream.SetSize(newSize: longint);
begin
  SetLength(FDataString, newSize);
  if FPosition > newSize then
    FPosition := newSize;
end; { TRawByteStringStream.SetSize }

{ TGpBaseEncryptedStream }

constructor TGpBaseEncryptedStream.Create(const key; keySize: integer; nonce: uint64;
  baseStream: TStream; autoDestroyWrappedStream: boolean);
begin
  inherited Create(baseStream, autoDestroyWrappedStream);
  FBufferer := TGpBufferedStream.Create(baseStream, CBufferSize, false);
  FInterceptor := TGpInterceptorStream.Create(InterceptRead, InterceptWrite, InterceptSeek, InterceptSetSize);
  InitCypher(key, keySize, nonce);
end; { TGpBaseEncryptedStream.Create }

destructor TGpBaseEncryptedStream.Destroy;
begin
  DoneCypher;
  FreeAndNil(FBufferer);
  FreeAndNil(FInterceptor);
  inherited;
end; { TGpBaseEncryptedStream.Destroy }

function TGpBaseEncryptedStream.GetSize: int64;
begin
  Result := FBaseStream.Size;
  // TODO 1 -oPrimoz Gabrijelcic : plus size of unwritten buffer
end; { TGpBaseEncryptedStream.GetSize }

function TGpBaseEncryptedStream.InternalSeek(offset: int64): int64;
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TGpBaseEncryptedStream.InternalSeek
end;

function TGpBaseEncryptedStream.Seek(const offset: int64; origin: TSeekOrigin): int64;
begin
  case origin of
    soBeginning:
      Result := InternalSeek(offset);
    soEnd:
      Result := InternalSeek(WrappedStream.Size + offset);
//    else
//      Result := InternalSeek(CurrentPosition + offset);
// TODO 1 -oPrimoz Gabrijelcic : implement: TGpBaseEncryptedStream.Seek
  end;
end; { TGpBaseEncryptedStream.Seek }

end.
