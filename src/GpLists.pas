(*:Various TList descendants, TList-compatible, and TList-similar classes.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2018, Primoz Gabrijelcic
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
   Creation date     : 2002-07-04
   Last modification : 2018-01-22
   Version           : 1.77
</pre>*)(*
   History:
     1.77: 2018-01-22
       - TGpFifoBuffer.Truncate was not taking active block into account.
     1.76: 2017-12-13
       - Successful TGpCache<K,V>.TryGetValue pushes retrieved key to the front of the
         MRU list.
     1.75: 2017-11-15
       - Key comparer can be set for TGpCache<K,V>.
     1.74: 2017-11-02
       - Implemented O(1) size-constrained cache TGpCache<K,V>.
     1.73: 2017-10-04
       - All lists that implemented Contains(element) got also Contains(element, var index).
         (Except the skip lists where elements are not indexed.)
     1.72c: 2017-09-05
       - TGpObjectMap's internal storage change from TGpIntegerObjectList to
         TGpInt64ObjectList so that it can correctly store pointers in 64-bit code.
     1.72b: 2017-04-06
       - Removed (useless, duplicate) GUID from IGpRingBuffer<T>.
     1.72a: 2016-08-30
       - TGpInt64ObjectList<T> methods had 'item' incorrectly defined as 'integer'
         instead of 'int64'.
     1.72: 2016-05-03
       - Implemented IGpInt64ObjectList<T> and TGpInt64ObjectList<T>.
     1.71: 2016-03-05
       - [bero] Added 'const' to various 'string' parameters.
       - [bero] Fixed invalid condition in GUIDCompare.
     1.70: 2016-03-04
       - TGpRingBuffer<T>.Dequeue: T returns Default(T) if queue is empty.
     1.69: 2016-02-24
       - Implemented TGpFifoBuffer.Clear.
     1.68: 2016-02-15
       - Implemented TGpObjectRingBuffer.BufferSizse.
     1.67: 2016-02-12
       - TGpFifoBuffer can maintain cached list of TFifoBlock objects.
     1.66: 2014-10-13
       - Implemented TGpDoublyLinkedList.EnumerateAs<T>.
     1.65: 2013-03-11
       - Implemented TGpSkipList<T>.Remove.
     1.64: 2012-12-07
       - TGpSkipList implements Head, Tail, Next and various overloads working with
         TGpSkipListEl<T>.
     1.63: 2012-12-05
       - Added missing TGpSkipList<T>.Locate.
       - Implemented TGpSkipObjectList<T>.
     1.62: 2012-11-21
       - Implemented TGpSkipList<T> and TGpSkipList<T,K>.
     1.61: 2012-07-04
       - Implemented IGpRingBuffer<T> and TGpRingBuffer<T>.
     1.60: 2012-05-07
       - Implemented TGpObjectRingBuffer.Remove.
     1.59: 2012-01-23
       - Added class TGpWideString.
     1.58: 2012-01-12
       - FifoBuffer supports external memory manager hooks (OnGetMem, OnFreeMem).
     1.57: 2012-01-11
       - (I|T)FifoBuffer renamed to (I|T)GpFifoBuffer.
       - Removed function CreateFifoBuffer, use TGpFifoBuffer.CreateInterface.
       - Speed optimizations in TGpDoublyLinkedList.
       - Inlining added to TGpFifoBuffer.
     1.56a: 2012-01-10
       - Fixed error in TGpFifoBuffer.Read (more data could be returned than requested).
       - Fixed error in TGpFifoBuffer.Write (DataSize/FifoPlace would return wrong value
         after failed write when the fifo was full).
     1.56: 2011-12-02
       - TGpIntegerObjectList<T> and TGpInterfaceList<T> are disabled in D2009
         due to the limitations of the compiler.
       - Fixed TGpIntegerObjectList<T>.Ensure(integer).
       - TGpInterfaceList<T> compiles with D2010.
     1.55: 2011-11-29
       - Each list implements class function CreateInterface which creates corresponding
         interface-implementing object.
     1.54: 2011-11-25
       - TGpIntegerObjectList implements IGpIntegerObjectList.
       - TGpIntegerObjectList<T> implements IGpIntegerObjectList<T>.
       - TGpCountedIntegerList implements IGpCountedIntegerList.
       - TGpInt64ObjectList implements IGpInt64ObjectList.
       - TGpCountedInt64List implements IGpCountedInt64List.
       - TGpGUIDList implements IGpGUIDList.
       - TGpTMethodList implements IGpTMethodList.
       - TGpClassList implements IGpClassList.
       - TGpObjectRingBuffer implements IGpObjectRingBuffer.
       - TGpObjectMap implements IGpObjectMap.
       - TGpObjectObjectMap implements IGpObjectObjectMap.
       - TGpDoublyLinkedList implements IGpDoublyLinkedList.
     1.53: 2011-11-24
       - TGpIntegerList implements IGpIntegerList.
       - TGpInt64List implements IGpInt64List.
     1.52: 2011-11-23
       - Added TGpIntegerObjectList<T>, a thin wrapper around TGpIntegerObjectList (D2009+).
       - Added TGpInterfaceList<T>, a thin wrapper aroung TList<T>, which implements
         IGpInterfaceList<T> (D2009+).
     1.51a: 2011-11-02
       - Find functions check (in Assert) if the list is sorted.
     1.51: 2011-06-21
       - Implemented limited size FIFO buffer.
     1.50: 2011-03-01
       - Added method FreeObjects to TGpIntegerObjectList and TGpInt64ObjectList.
     1.49: 2011-01-02
       - Implemented TGpGUIDList, a TGUID list. 
     1.48b: 2011-01-01
       - [Erik Berry] TGpStringValue is only compiled if TStrings helper class is compiled. 
     1.48a: 2010-10-21
       - Fixed TGpStringsHelper.WalkKV.
     1.48: 2010-10-19
       - Added method RemoveObject and enumerator WalkKV to the TStrings helper.
     1.47: 2010-10-13
       - Fixed TGp[Integer|Int64]List sorting (broken since 1.44 release).
     1.46a: 2010-07-28
       - [Jens] Capacity was not set to the ideal value in TGp[Integer|Int64]List.Append.
     1.46: 2010-07-13
       - [Istvan] Reintroduced Insert methods for Counted Integer and Int64 lists that
         accept a count parameter
     1.45: 2010-07-05
       - Added overloaded version of EnsureObject.
     1.44: 2010-05-13
       - TStringList helper split into TStrings and TStringList helpers.
     1.43: 2009-07-01
       - Added parameter 'step' to various Slice(), Walk() and WalkKV() enumerators.
     1.42: 2008-11-10
       - Added method FreeObjects to the TStringList helper.
     1.41: 2008-06-03
       - Unicode-ready (hope, hope).
     1.40: 2008-05-11
       - Delphi 7 compatibility restored.
       - Added helper method FetchObject to TGpIntegerObjectList,
         TGpInt64ObjectList, and TGpStringListHelper.
     1.39: 2008-04-22
       - Implemented TGpStringListHelper.Contains.
     1.38a: 2008-03-27
       - Fixed broken TGpInt64List.Move.
     1.38: 2008-03-25
       - Implemented method Sort in TGpIntegerList, TGpInt64List, and TGpStringListHelper.
     1.37: 2008-03-20
       - Added WalkKV enumerator to the TGpIntegerObjectList and TGpInt64ObjectList.
     1.36a: 2008-03-17
       - Use CUpperListBound instead of -1 as a Slice and Walk default value for the upper
         index.
     1.36: 2008-03-13
       - Added Walk enumerator to the TGpIntegerList and TGpInt64List. This enumerator
         allows list modifications (Add, Insert, Delete) from the enumeration consumer.
         IOW, you can do this:
         for idx in list.Walk do
           if SomeCondition(list[idx]) then
             list.Delete(idx);
     1.35: 2008-03-11
       - Modified TGpCountedInt64List to store int64 counters.
       - Added property ItemCounter[] to TGpCountedIntegerList and TGpCountedInt64List.
     1.34: 2008-03-03
       - Added Slice(from, to) enumerators to TGpIntegerList and TGpInt64List.
     1.33: 2007-11-27
       - Add TGpObjectRingBuffer and TGpDoublyLinkedList enumerators. Both lock access to
         the list during the enumeration process if multithreaded mode is enabled.
     1.32a: 2007-11-21
       - When TGpIntegerObjectList or TGpInt64ObjectList was Sorted and with Duplicates
         set to dupIgnore, calling AddObject with item that was already in the list caused
         internal exception. Test case:
           iol := TGpIntegerObjectList.Create(false);
           iol.Sorted := true;
           iol.Duplicates := dupIgnore;
           iol.AddObject(1, nil);
           iol.AddObject(1, nil);
     1.32: 2007-11-15
       - Added method Contains to TGpIntegerList, TGpInt64List, TGpCountedStringList,
         TGpTMethodList.
     1.31: 2007-10-26
       - Implemented TGpClassList, a TStringList-based list of classes. Class names must
         be unique. Useful when you need to implement a class registry to generate objects
         from class names.
       - Un-virtual-ized bunch of methods in TGpTMethodList so that they can be inlined.
       - Inlined some more getters and setters.
     1.30: 2007-10-18
       - Enumerators changed to records with GetCurrent inlined, as suggested in
         http://hallvards.blogspot.com/2007/10/more-fun-with-enumerators.html.
     1.29: 2007-10-03
       - Use spinlock for locking.
       - TGpObjectRingBuffer can put locks around all operations.
       - TGpObjectRingBuffer can trigger an event when buffer is fuller than the specified
         threshold and another event when buffer is emptier than the (different) threshold.
       - Added missing locks to TGpDoublyLinkedList in multithreaded mode.
     1.28b: 2007-09-13
       - Fixed Add operation on sorted lists (broken in 1.28a).
     1.28a: 2007-09-12
       - Disallow Move and Insert operations on sorted lists.
     1.28: 2007-07-25
       - Added Last method to the TStringList helper.
     1.27: 2007-06-28
       - Added bunch of 'inline' directives.
       - Added TStringList helper.
     1.26: 2007-04-17
       - Added TGpReal class.
     1.25: 2007-03-19
       - Added TGpCountedIntegerList class.
       - Added TGpCountedInt64List class.
       - Added CustomSort method to the TGpIntegerList and TGpInt64List classes.
     1.24: 2007-02-20
       - Added EqualTo method to the TGpIntegerList class.
     1.23a: 2007-01-19
       - Compiles with Delphi 6 again. Big thanks to Tao Lin and Erik Berry for reporting,
         diagnosing and fixing the problem.
     1.23: 2006-12-06
       - Added ValuesIdx to the TGpObjectMap class.
     1.22a: 2006-09-29
       - Fixed nasty bug in TGpIntegerObjectList.AddObject and
         TGpInt64ObjectList.AddObject.
       - Fixed range errors in TGpInt64[Object]List.
     1.22: 2006-09-20
       - Implemented TGpInt64List and TGpInt64ObjectList.
     1.21: 2006-05-15
       - Implemented list of TMethod records - TGpTMethodList.
     1.20: 2006-04-24
       - Added method TGpIntegerObjectList.ExtractObject.
     1.19: 2005-11-18
       - Added D2005-style TGpIntegerList enumerator.
     1.18: 2005-10-27
       - Added TGpString class.
     1.17: 2005-06-02
       - Added methods FreeAll and UnlinkAll to the TGpDoublyLinkedList class.
     1.16: 2004-11-22
       - Added Dump/Restore mechanism to the TGpInteger[Object]List classes.
     1.15: 2004-09-09
       - Added method Remove to the TGpIntegerList class.
     1.14: 2004-02-17
       - Added 'delimiter' parameter to the TGpIntegerList.AsHexText.
     1.13: 2004-02-12
       - Added iterator access (Count, Items) to the TGpObjectMap class.
     1.12: 2003-12-18
       - Published helper function IntegerCompare.
     1.11: 2003-11-05
       - TGpDoublyLinkedList got new constructor parameter - multithreaded. When
         set to True (default is False), all list-related operations are wrapped
         into a critical section.
     1.10: 2003-10-28
       - Added doubly-linked list class - TGpDoublyLinkedList.
     1.09a: 2003-10-16
       - TGpObjectRingBuffer.Head was off-by-one, causing all sorts of problems.
     1.09: 2003-09-27
       - Added function TGpIntegerList.AsDelimitedText.
     1.08: 2003-09-15
       - Added function TGpIntegerList.Ensure.
       - Added function TGpIntegerObjectList.EnsureObject.
       - Added function TGpCountedStringList.Ensure.
       - Added methods  TGpIntegerObjectList.LoadFromStream, .SaveToStream.
     1.07: 2003-08-02
       - Added class TGpObjectMap.
       - Added class TGpObjectObjectMap.
       - Added class TGpInt64.
     1.06: 2003-07-27
       - Prefixed all classes with 'Gp'.
       - Added class TGpObjectRingBuffer.
     1.05: 2003-07-15
       - Added overloaded constructor TIntegerList.Create(array of integer).
       - Added overloaded method Assign(array of integer).
     1.04: 2003-06-11
       - Added methods TIntegerList.SaveToStream and
         TIntegerList.LoadFromStream.
     1.03: 2003-06-09
       - Added TIntegerObjectList class.
     1.02a: 2003-03-21
       - Fixed TIntegerList.Find, which was completely broken.
     1.02: 2002-10-30
       - Added property TIntegerList.Text;
     1.01: 2002-09-23
       - Added method TIntegerList.IndexOf.
*)

unit GpLists;

interface

{$IFDEF CONDITIONALEXPRESSIONS}
  {$WARN SYMBOL_PLATFORM OFF}
  {$IF (RTLVersion < 15)} // Delphi 6 or older
    {$DEFINE GpLists_RequiresD6CompilerHack}
  {$IFEND}
  {$IF (CompilerVersion >= 16) and (CompilerVersion < 17)} // Delphi 8 IDE Integration compiler
    {$DEFINE GpLists_RequiresD6CompilerHack}
  {$IFEND}
  {$IF (CompilerVersion >= 17)}
    {$DEFINE USE_STRICT}
  {$IFEND}
  {$IF (CompilerVersion >= 18)} // Delphi 2006 or newer (D2005 does not support record methods, and fails to compile this unit with inlining enabled)
    {$DEFINE GpLists_Inline}
    {$DEFINE GpLists_TStringsHelper}
    {$DEFINE GpLists_Enumerators}
    {$DEFINE GpLists_Sorting}
    {$DEFINE GpLists_RegionsSupported} // TP : I am not sure in which version, but D7 don't agree with them
  {$IFEND}
  {$IF CompilerVersion <= 20} //D2009
    {$DEFINE GpLists_LimitedGenerics}
  {$IFEND}
  {$IF CompilerVersion >= 23} //DXE2
    {$DEFINE GpLists_HasSystemTypes}
  {$IFEND}
{$ENDIF}

{$M-}

uses
  SysUtils,
  Classes,
  {$IFDEF GpLists_HasSystemTypes}
  System.Types,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Windows,
  Contnrs,
  DSiWin32,
  {$ENDIF}
  {$IFDEF POSIX}
  Posix.Pthread,
  {$ENDIF}
  {$IFDEF Unicode}
  Generics.Defaults,
  Generics.Collections,
  {$ENDIF}
  SyncObjs,
  Math;

const
  CUpperListBound = MaxInt; //converted to Self.Count-1 inside Slice and Walk

resourcestring
  SGUIDDuplicateItem = 'List does not allow duplicates ($0%x)';

type
  {:Boxed int64. Usable for inserting int64 numbers into the TObjectList or similar classes.
    @since   2003-08-02
  }
  TGpInt64 = class
  private
    i64Value: int64;
  public
    constructor Create(aValue: int64 = 0);
    property Value: int64 read i64Value write i64Value;
  end; { TGpInt64 }

  {:Boxed TDateTime. Usable for inserting TDateTime values into the TObjectList or similar classes.
    @since   2003-08-23
  }
  TGpDateTime = class
  private
    dtValue: TDateTime;
  public
    constructor Create(aValue: TDateTime = 0);
    property Value: TDateTime read dtValue write dtValue;
  end; { TGpDateTime }

  {:Boxed string.
    @since   2005-10-27
  }
  TGpString = class
  private
    sValue: string;
  public
    constructor Create(const aValue: string = '');
    property Value: string read sValue write sValue;
  end; { TGpString }

  {:Boxed wide string.
    @since   2012-01-23
  }
  {$IFDEF MSWINDOWS}
  TGpWideString = class
  private
    sValue: WideString;
  public
    constructor Create(const aValue: WideString = '');
    property Value: WideString read sValue write sValue;
  end; { TGpWideString }
  {$ENDIF}

  {:Boxed real.
    @since   2007-04-17
  }
  TGpReal = class
  private
    rValue: real;
  public
    constructor Create(aValue: real = 0);
    property Value: real read rValue write rValue;
  end; { TGpReal }

  {:Key-value pair as returned form the WalkKV enumerator.
    @since   2008-03-20
  }
  TGpKeyValue = class
  private
    kvKey  : int64;
    kvValue: TObject;
  public
    property Key: int64 read kvKey write kvKey;
    property Value: TObject read kvValue write kvValue;
  end; { TGpKeyValue }

  {$IFDEF GpLists_TStringsHelper} 
  {:Key-value pair as returned form the TStrings helper's WalkKV enumerator.
    @since   2010-10-19
  }
  TGpStringValue = record
  private
    kvKey  : string;
    kvValue: TObject;
  public
    property Key: string read kvKey write kvKey;
    property Value: TObject read kvValue write kvValue;
  end; { TGpStringValue }
  {$ENDIF}

  TGpListOperation = (loInsert, loDelete);
  TGpListNotificationEvent = procedure(list: TObject; idxItem: integer; operation:
    TGpListOperation) of object;

  TGpIntegerList = class;
  TGpTMethodList = class;

  {$IFDEF GpLists_Enumerators}
  TGpIntegerListEnumerator = record
  private
    ileIdxTo: integer;
    ileIndex: integer;
    ileList : TGpIntegerList;
    ileStep : integer;
  public
    constructor Create(aList: TGpIntegerList; idxFrom, idxTo, step: integer);
    function  GetCurrent: integer;                  {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: integer read GetCurrent;
  end; { TGpIntegerListEnumerator }

  ///<summary>Partially stable enumerator that returns list indices instead of elements.
  ///   <para>Can handle deletions and insertions while the enumerator is in operation.</para></summary>
  ///<since>2008-03-13</since>
  TGpIntegerListWalkEnumerator = class
  private
    ilweIdxTo: integer;
    ilweIndex: integer;
    ilweList : TGpIntegerList;
    ilweStep : integer;
  protected
    procedure HandleListChange(list: TObject; idxItem: integer; operation: TGpListOperation);
    property List: TGpIntegerList read ilweList;
  public
    constructor Create(aList: TGpIntegerList; idxFrom, idxTo, step: integer);
    destructor  Destroy; override;
    function  GetCurrent: integer;                  {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: integer read GetCurrent;
  end; { TGpIntegerListWalkEnumerator }

  TGpIntegerListSliceEnumeratorFactory = record
  private
    sefList   : TGpIntegerList;
    sefIdxFrom: integer;
    sefIdxTo  : integer;
    sefStep   : integer;
  public
    constructor Create(list: TGpIntegerList; idxFrom, idxTo, step: integer);
    function  GetEnumerator: TGpIntegerListEnumerator;
  end; { TGpIntegerListSliceEnumeratorFactory }

  TGpIntegerListWalkEnumeratorFactory = record
  private
    wefList   : TGpIntegerList;
    wefIdxFrom: integer;
    wefIdxTo  : integer;
    wefStep   : integer;
  public
    constructor Create(list: TGpIntegerList; idxFrom, idxTo, step: integer);
    function  GetEnumerator: TGpIntegerListWalkEnumerator;
  end; { TGpIntegerListWalkEnumeratorFactory }
  {$ENDIF GpLists_Enumerators}

  TGpIntegerListSortCompare = function(List: TGpIntegerList;
    Index1, Index2: integer): integer;

  IGpIntegerList = interface ['{226CE09C-935C-44A9-BB74-66BFB0CFE130}']
    function  GetCapacity: integer;
    function  GetCount: integer;
    function  GetDuplicates: TDuplicates;
    function  GetItems(idx: integer): integer;
    {$IFDEF GpLists_Sorting}
    function  GetSorted: boolean;
    {$ENDIF}
    function  GetText: string;
    procedure SetCapacity(const value: integer);
    procedure SetCount(const value: integer);
    procedure SetDuplicates(const value: TDuplicates);
    procedure SetItems(idx: integer; const value: integer);
    {$IFDEF GpLists_Sorting}
    procedure SetSorted(const value: boolean);
    {$ENDIF}
    procedure SetText(const value: string);
    //
    function  Add(item: integer): integer;
    procedure Append(const elements: array of integer); overload;
    procedure Append(list: TGpIntegerList); overload;
    function  AsDelimitedText(const delimiter: string): string;
    function  AsHexText(const delimiter: string = ''): string;
    procedure Assign(const elements: array of integer); overload;
    procedure Assign(list: TGpIntegerList); overload;
    procedure Clear;
    function  Contains(item: integer): boolean; overload;
    function  Contains(item: integer; var idx: integer): boolean; overload;
    {$IFDEF GpLists_Sorting}
    procedure CustomSort(sortMethod: TGpIntegerListSortCompare);
    {$ENDIF}
    procedure Delete(idx: integer);
    function  Dump(baseAddr: pointer): pointer;
    function  Ensure(item: integer): integer;
    function  EqualTo(list: TGpIntegerList): boolean;
    procedure Exchange(idx1, idx2: integer);
    function  Find(avalue: integer; var idx: integer): boolean;
    function  First: integer;
    function  IndexOf(item: integer): integer;
    procedure Insert(idx, item: integer);
    function  Last: integer;
    function  LoadFromStream(stream: TStream): boolean;
    procedure Move(curIdx, newIdx: integer);
    procedure RegisterNotification(notificationHandler: TGpListNotificationEvent);
    procedure Remove(item: integer);
    function  Restore(baseAddr: pointer): pointer;
    procedure SaveToStream(stream: TStream);
    {$IFDEF GpLists_Sorting}
    procedure Sort;
    {$ENDIF}
    procedure UnregisterNotification(notificationHandler: TGpListNotificationEvent);
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpIntegerListEnumerator;
    function  Slice(idxFrom: integer; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpIntegerListSliceEnumeratorFactory;
    function  Walk(idxFrom: integer = 0; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpIntegerListWalkEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Items[idx: integer]: integer read GetItems write SetItems; default;
    {$IFDEF GpLists_Sorting}
    property Sorted: boolean read GetSorted write SetSorted;
    {$ENDIF}
    property Text: string read GetText write SetText;
  end; { IGpIntegerList }

  {:List of integers.
    @since   2002-07-04.
  }
  TGpIntegerList = class(TInterfacedObject, IGpIntegerList)
  private
    ilDuplicates          : TDuplicates;
    ilList                : TList;
    ilNotificationHandlers: TGpTMethodList;
    ilSorted              : boolean;
  protected
    function  GetAsDelimitedText(const delimiter: string;
      appendLastDelimiter: boolean): string;
    function  GetCapacity: integer; virtual;
    function  GetCount: integer; virtual;
    function  GetItems(idx: integer): integer; virtual;
    function  GetText: string; virtual;
    procedure InsertItem(idx, item: integer);
    procedure Notify(idxItem: integer; operation: TGpListOperation); {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetDuplicates: TDuplicates; virtual;
    function  GetSorted: boolean; virtual;
    procedure QuickSort(L, R: integer; SCompare: TGpIntegerListSortCompare);
    procedure SetCapacity(const value: integer); virtual;
    procedure SetCount(const value: integer); virtual;
    procedure SetDuplicates(const value: TDuplicates); virtual;
    procedure SetItems(idx: integer; const value: integer); virtual;
    {$IFDEF GpLists_Sorting}
    procedure SetSorted(const value: boolean); virtual;
    {$ENDIF}
    procedure SetText(const value: string); virtual;
  public
    constructor Create; overload;
    constructor Create(const elements: array of integer); overload;
    destructor  Destroy; override;
    class function CreateInterface: IGpIntegerList; overload;
    class function CreateInterface(const elements: array of integer): IGpIntegerList; overload;
    function  Add(item: integer): integer; virtual;
    procedure Append(const elements: array of integer); overload;
    procedure Append(list: TGpIntegerList); overload; virtual;
    procedure Append(list: IGpIntegerList); overload; virtual;
    function  AsDelimitedText(const delimiter: string): string; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  AsHexText(const delimiter: string = ''): string;
    procedure Assign(const elements: array of integer); overload;
    procedure Assign(list: TGpIntegerList); overload; virtual;
    procedure Assign(list: IGpIntegerList); overload; virtual;
    procedure Clear; virtual;
    function  Contains(item: integer): boolean; overload;                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Contains(item: integer; var idx: integer): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$IFDEF GpLists_Sorting}
    procedure CustomSort(sortMethod: TGpIntegerListSortCompare);
    {$ENDIF}
    procedure Delete(idx: integer); virtual;
    function  Dump(baseAddr: pointer): pointer; virtual;
    function  Ensure(item: integer): integer; virtual;
    function  EqualTo(list: TGpIntegerList): boolean;
    procedure Exchange(idx1, idx2: integer); virtual;
    function  Find(avalue: integer; var idx: integer): boolean; virtual;
    function  First: integer; virtual;
    function  IndexOf(item: integer): integer;
    procedure Insert(idx, item: integer); virtual;
    function  Last: integer; virtual;
    function  LoadFromStream(stream: TStream): boolean; virtual;
    procedure Move(curIdx, newIdx: integer); virtual;
    procedure RegisterNotification(notificationHandler: TGpListNotificationEvent);
    procedure Remove(item: integer); virtual;
    function  Restore(baseAddr: pointer): pointer; virtual;
    procedure SaveToStream(stream: TStream); virtual;
    {$IFDEF GpLists_Sorting}
    procedure Sort;                                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$ENDIF}
    procedure UnregisterNotification(notificationHandler: TGpListNotificationEvent);
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpIntegerListEnumerator; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Slice(idxFrom: integer; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpIntegerListSliceEnumeratorFactory;
    function  Walk(idxFrom: integer = 0; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpIntegerListWalkEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Items[idx: integer]: integer read GetItems write SetItems; default;
    {$IFDEF GpLists_Sorting}
    property Sorted: boolean read GetSorted write SetSorted;
    {$ENDIF}
    property Text: string read GetText write SetText;
  end; { TGpIntegerList }

  TGpInt64List = class;

  {$IFDEF GpLists_Enumerators}
  {:TGpInt64List enumerator.
    @since   2005-11-18
  }
  TGpInt64ListEnumerator = record
  private
    ileIdxTo: integer;
    ileIndex: integer;
    ileList : TGpInt64List;
    ileStep : integer;
  public
    constructor Create(aList: TGpInt64List; idxFrom, idxTo, step: integer);
    function  GetCurrent: int64;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: int64 read GetCurrent;
  end; { TGpInt64ListEnumerator }

  ///<summary>Partially stable enumerator that returns list indices instead of elements.
  ///   <para>Can handle deletions and insertions while the enumerator is in operation.</para></summary>
  ///<since>2008-03-13</since>
  TGpInt64ListWalkEnumerator = class
  private
    ilweIdxTo: integer;
    ilweIndex: integer;
    ilweList : TGpInt64List;
    ilweStep : integer;
  protected
    procedure HandleListChange(list: TObject; idxItem: integer; operation: TGpListOperation);
    property List: TGpInt64List read ilweList;
  public
    constructor Create(aList: TGpInt64List; idxFrom, idxTo, step: integer);
    destructor  Destroy; override;
    function  GetCurrent: integer;
    function  MoveNext: boolean;
    property Current: integer read GetCurrent;
  end; { TGpInt64ListWalkEnumerator }

  TGpInt64ListSliceEnumeratorFactory = record
  private
    sefList   : TGpInt64List;
    sefIdxFrom: integer;
    sefIdxTo  : integer;
    sefStep   : integer;
  public
    constructor Create(list: TGpInt64List; idxFrom, idxTo, step: integer);
    function  GetEnumerator: TGpInt64ListEnumerator;
  end; { TGpInt64ListSliceEnumeratorFactory }

  TGpInt64ListWalkEnumeratorFactory = record
  private
    wefList   : TGpInt64List;
    wefIdxFrom: integer;
    wefIdxTo  : integer;
    wefStep   : integer;
  public
    constructor Create(list: TGpInt64List; idxFrom, idxTo, step: integer);
    function  GetEnumerator: TGpInt64ListWalkEnumerator;
  end; { TGpInt64ListWalkEnumeratorFactory }
  {$ENDIF GpLists_Enumerators}

  TGpInt64ListSortCompare = function(List: TGpInt64List; Index1, Index2: integer):
    integer;

  IGpInt64List = interface ['{712C0D85-6B7A-48CC-B25C-4DB9C2736190}']
    function  GetCapacity: integer;
    function  GetCount: integer;
    function  GetDuplicates: TDuplicates;
    function  GetItems(idx: integer): int64;
    function  GetSorted: boolean;
    function  GetText: string;
    procedure SetCapacity(const value: integer);
    procedure SetCount(const value: integer);
    procedure SetDuplicates(const value: TDuplicates);
    procedure SetItems(idx: integer; value: int64);
    procedure SetSorted(const value: boolean);
    procedure SetText(const value: string);
    //
    function  Add(item: int64): integer;
    procedure Append(const elements: array of int64); overload;
    procedure Append(list: TGpInt64List); overload;
    procedure Append(list: TGpIntegerList); overload;
    function  AsDelimitedText(const delimiter: string): string;
    function  AsHexText(const delimiter: string = ''): string;
    procedure Assign(const elements: array of int64); overload;
    procedure Assign(list: TGpInt64List); overload;
    procedure Assign(list: TGpIntegerList); overload;
    procedure Clear;
    function  Contains(item: int64): boolean; overload;
    function  Contains(item: int64; var idx: integer): boolean; overload;
    procedure Delete(idx: integer);
    function  Dump(baseAddr: pointer): pointer;
    function  Ensure(item: int64): integer;
    procedure Exchange(idx1, idx2: integer);
    function  Find(avalue: int64; var idx: integer): boolean;
    function  First: int64;
    function  IndexOf(item: int64): integer;
    procedure Insert(idx: integer; item: int64);
    function  Last: int64;
    function  LoadFromStream(stream: TStream): boolean;
    procedure Move(curIdx, newIdx: integer);
    procedure RegisterNotification(notificationHandler: TGpListNotificationEvent);
    procedure Remove(item: int64);
    function  Restore(baseAddr: pointer): pointer;
    procedure SaveToStream(stream: TStream);
    procedure Sort;
    procedure UnregisterNotification(notificationHandler: TGpListNotificationEvent);
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpInt64ListEnumerator;
    function  Slice(idxFrom: integer; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpInt64ListSliceEnumeratorFactory;
    function  Walk(idxFrom: integer = 0; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpInt64ListWalkEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Items[idx: integer]: int64 read GetItems write SetItems; default;
    property Sorted: boolean read GetSorted write SetSorted;
    property Text: string read GetText write SetText;
  end; { IGpInt64List }

  {:List of 64-bit integers.
    @since   2006-09-20
  }
  TGpInt64List = class(TInterfacedObject, IGpInt64List)
  private
    ilDuplicates          : TDuplicates;
    ilList                : TList;
    ilNotificationHandlers: TGpTMethodList;
    ilSorted              : Boolean;
  protected
    procedure CustomSort(sortMethod: TGpInt64ListSortCompare); virtual;
    function  GetAsDelimitedText(const delimiter: string;
      appendLastDelimiter: boolean): string;
    function  GetCapacity: integer; virtual;
    function  GetCount: integer; virtual;
    function  GetDuplicates: TDuplicates; virtual;
    function  GetItems(idx: integer): int64; virtual;
    function  GetSorted: boolean; virtual;
    function  GetText: string; virtual;
    procedure InsertItem(idx: integer; item: int64);
    procedure Notify(idxItem: integer; operation: TGpListOperation);
    procedure QuickSort(L, R: integer; SCompare: TGpInt64ListSortCompare);
    procedure SetCapacity(const value: integer); virtual;
    procedure SetCount(const value: integer); virtual;
    procedure SetDuplicates(const value: TDuplicates);
    procedure SetItems(idx: integer; value: int64); virtual;
    procedure SetSorted(const value: boolean); virtual;
    procedure SetText(const value: string); virtual;
  public
    constructor Create; overload;
    constructor Create(const elements: array of int64); overload;
    class function CreateInterface: IGpInt64List; overload;
    class function CreateInterface(const elements: array of int64): TGpInt64List; overload;
    destructor  Destroy; override;
    function  Add(item: int64): integer; virtual;
    procedure Append(const elements: array of int64); overload;
    procedure Append(list: TGpIntegerList); overload; virtual;
    procedure Append(list: IGpIntegerList); overload; virtual;
    procedure Append(list: TGpInt64List); overload; virtual;
    procedure Append(list: IGpInt64List); overload; virtual;
    function  AsDelimitedText(const delimiter: string): string; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  AsHexText(const delimiter: string = ''): string;
    procedure Assign(const elements: array of int64); overload;
    procedure Assign(list: TGpInt64List); overload; virtual;
    procedure Assign(list: IGpInt64List); overload; virtual;
    procedure Assign(list: TGpIntegerList); overload; virtual;
    procedure Assign(list: IGpIntegerList); overload; virtual;
    procedure Clear; virtual;
    function  Contains(item: int64): boolean; overload;                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Contains(item: int64; var idx: integer): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Delete(idx: integer); virtual;
    function  Dump(baseAddr: pointer): pointer; virtual;
    function  Ensure(item: int64): integer; virtual;
    procedure Exchange(idx1, idx2: integer); virtual;
    function  Find(avalue: int64; var idx: integer): boolean; virtual;
    function  First: int64; virtual;
    function  IndexOf(item: int64): integer;
    procedure Insert(idx: integer; item: int64); virtual;
    function  Last: int64; virtual;
    function  LoadFromStream(stream: TStream): boolean; virtual;
    procedure Move(curIdx, newIdx: integer); virtual;
    procedure RegisterNotification(notificationHandler: TGpListNotificationEvent);
    procedure Remove(item: int64); virtual;
    function  Restore(baseAddr: pointer): pointer; virtual;
    procedure SaveToStream(stream: TStream); virtual;
    procedure Sort;                                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure UnregisterNotification(notificationHandler: TGpListNotificationEvent);
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpInt64ListEnumerator; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Slice(idxFrom: integer; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpInt64ListSliceEnumeratorFactory;
    function  Walk(idxFrom: integer = 0; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpInt64ListWalkEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Items[idx: integer]: int64 read GetItems write SetItems; default;
    property Sorted: boolean read GetSorted write SetSorted;
    property Text: string read GetText write SetText;
  end; { TGpInt64List }

  {$IFDEF GpLists_Enumerators}
  TGpIntegerObjectList = class;

  TGpIntegerObjectListWalkKVEnumerator = class
  private
    wkeCurrentKV     : TGpKeyValue;
    wkeListEnumerator: TGpIntegerListWalkEnumerator;
  public
    constructor Create(aList: TGpIntegerObjectList; idxFrom, idxTo, step: integer);
    destructor  Destroy; override;
    function  GetCurrent: TGpKeyValue;              {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: TGpKeyValue read GetCurrent;
  end; { TGpIntegerObjectListWalkKVEnumerator }

  TGpIntegerObjectListWalkKVEnumeratorFactory = record
  private
    wkefList   : TGpIntegerObjectList;
    wkefIdxFrom: integer;
    wkefIdxTo  : integer;
    wkefStep   : integer;
  public
    constructor Create(list: TGpIntegerObjectList; idxFrom, idxTo, step: integer);
    function  GetEnumerator: TGpIntegerObjectListWalkKVEnumerator;
  end; { TGpIntegerObjectListWalkKVEnumeratorFactory }
  {$ENDIF GpLists_Enumerators}

  IGpIntegerObjectList = interface(IGpIntegerList) ['{FEA4B0DB-E611-4181-B6A9-88E22AF4021E}']
    function  GetObject(idxObject: integer): TObject;
    procedure SetObject(idxObject: integer; const value: TObject);
    //
    function  Add(item: integer): integer;
    function  AddObject(item: integer; obj: TObject): integer;
    procedure Clear;
    procedure Delete(idx: integer);
    function  Dump(baseAddr: pointer): pointer;
    function  EnsureObject(item: integer; obj: TObject): integer; overload;
    function  EnsureObject(item: integer; objClass: TClass): integer; overload;
    procedure Exchange(idx1, idx2: integer);
    function  ExtractObject(idxObject: integer): TObject;
    function  FetchObject(item: integer): TObject;
    procedure FreeObjects;
    procedure Insert(idx: integer; item: integer);
    procedure InsertObject(idx: integer; item: integer; obj: TObject);
    function  LoadFromStream(stream: TStream): boolean;
    procedure Move(curIdx, newIdx: integer);
    function  Restore(baseAddr: pointer): pointer;
    procedure SaveToStream(stream: TStream);
    {$IFDEF GpLists_Enumerators}
    function  WalkKV(idxFrom: integer = 0; idxTo: integer = CUpperListBound;
      step: integer = 1): TGpIntegerObjectListWalkKVEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Objects[idxObject: integer]: TObject read GetObject write SetObject;
  end; { IGpIntegerObjectList }

  {:Integer list where each integer is accompanied with an object.
    @since   2003-06-09
  }
  {$IFNDEF MSWINDOWS}
  TObjectList = class(TObjectList<TObject>)
  end;
  {$ENDIF}

  TGpIntegerObjectList = class(TGpIntegerList, IGpIntegerObjectList)
  private
    iolObjects: TObjectList;
  protected
    function  GetObject(idxObject: integer): TObject; virtual;
    procedure SetObject(idxObject: integer; const value: TObject); virtual;
  public
    constructor Create(ownsObjects: boolean = true);
    destructor  Destroy; override;
    class function CreateInterface(ownsObjects: boolean = true): IGpIntegerObjectList;
    function  Add(item: integer): integer; override;
    function  AddObject(item: integer; obj: TObject): integer; virtual;
    procedure Clear; override;
    procedure Delete(idx: integer); override;
    function  Dump(baseAddr: pointer): pointer; override;
    function  EnsureObject(item: integer; obj: TObject): integer; overload; virtual;
    function  EnsureObject(item: integer; objClass: TClass): integer; overload; virtual;
    procedure Exchange(idx1, idx2: integer); override;
    function  ExtractObject(idxObject: integer): TObject; virtual;
    function  FetchObject(item: integer): TObject; virtual;
    procedure FreeObjects; virtual;
    procedure Insert(idx: integer; item: integer); override;
    procedure InsertObject(idx: integer; item: integer; obj: TObject); virtual;
    function  LoadFromStream(stream: TStream): boolean; override;
    procedure Move(curIdx, newIdx: integer); override;
    function  Restore(baseAddr: pointer): pointer; override;
    procedure SaveToStream(stream: TStream); override;
    {$IFDEF GpLists_Enumerators}
    function  WalkKV(idxFrom: integer = 0; idxTo: integer = CUpperListBound;
      step: integer = 1): TGpIntegerObjectListWalkKVEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Objects[idxObject: integer]: TObject read GetObject write SetObject;
  end; { TGpIntegerObjectList }

  {$IFDEF Unicode}
  {$IFNDEF GpLists_LimitedGenerics}
  IGpIntegerObjectList<T> = interface
    function  GetObject(idxObject: integer): T;
    procedure SetObject(idxObject: integer; const value: T);
    //
    function  AddObject(item: integer; obj: T): integer;
    function  EnsureObject(item: integer): integer; overload;
    function  EnsureObject(item: integer; obj: T): integer; overload;
    function  ExtractObject(idxObject: integer): T;
    function  FetchObject(item: integer): T;
    procedure InsertObject(idx: integer; item: integer; obj: T);
    property Objects[idxObject: integer]: T read GetObject write SetObject;
  end; { IGpIntegerObjectList<T> }

  {:Integer list where each integer is accompanied with a typed object.
    @since   2003-06-09
  }
  TGpIntegerObjectList<T: class, constructor> = class(TGpIntegerObjectList, IGpIntegerObjectList<T>)
  protected
    function  GetObject(idxObject: integer): T; reintroduce;
    procedure SetObject(idxObject: integer; const value: T); reintroduce;
  public
    class function CreateInterface(ownsObjects: boolean = true): IGpIntegerObjectList<T>;
    function  AddObject(item: integer; obj: T): integer; reintroduce; inline;
    function  EnsureObject(item: integer): integer; overload; inline;
    function  EnsureObject(item: integer; obj: T): integer; reintroduce; overload; inline;
    function  ExtractObject(idxObject: integer): T; reintroduce; inline;
    function  FetchObject(item: integer): T; reintroduce; inline;
    procedure InsertObject(idx: integer; item: integer; obj: T); reintroduce; inline;
    property Objects[idxObject: integer]: T read GetObject write SetObject;
  end; { TGpIntegerObjectList<T> }
  {$ENDIF GpLists_LimitedGenerics}
  {$ENDIF Unicode}

  IGpCountedIntegerList = interface ['{DE728A7B-AE89-4F38-A052-6FEB943A00E0}']
    function  GetCounter(idx: integer): integer;
    function  GetItemCounter(item: integer): integer;
    procedure SetCounter(idx: integer; const value: integer);
    procedure SetItemCounter(item: integer; const value: integer);
    //
    function  Add(item, count: integer): integer;
    function  Ensure(item, count: integer): integer;
    function  Fetch(item: integer): integer;
    procedure Insert(idx: integer; item, count: integer);
    {$IFDEF GpLists_Sorting}
    procedure SortByCounter(descending: boolean = true);
    {$ENDIF}
    property Counter[idx: integer]: integer read GetCounter write SetCounter;
    property ItemCounter[item: integer]: integer read GetItemCounter write SetItemCounter;
  end; { IGpCountedIntegerList }

  {:A thin layer over TGpIntegerObject list where each item has associated counter (stored
    in the Objects property).
  }
  TGpCountedIntegerList = class(TGpIntegerObjectList, IGpCountedIntegerList)
  protected
    function  GetCounter(idx: integer): integer; virtual;
    function  GetItemCounter(item: integer): integer;
    procedure SetCounter(idx: integer; const value: integer); virtual;
    procedure SetItemCounter(item: integer; const value: integer);
  public
    constructor Create; reintroduce;
    class function CreateInterface: IGpCountedIntegerList;
    function  Add(item, count: integer): integer; reintroduce;
    function  Ensure(item, count: integer): integer; reintroduce;
    function  Fetch(item: integer): integer;
    procedure Insert(idx: integer; item, count: integer); reintroduce;
    {$IFDEF GpLists_Sorting}
    procedure SortByCounter(descending: boolean = true);
    {$ENDIF}
    property Counter[idx: integer]: integer read GetCounter write SetCounter;
    property ItemCounter[item: integer]: integer read GetItemCounter write SetItemCounter;
  end; { TGpCountedIntegerList }

  {$IFDEF GpLists_Enumerators}
  TGpInt64ObjectList = class;

  TGpInt64ObjectListWalkKVEnumerator = class
  private
    wkeCurrentKV     : TGpKeyValue;
    wkeListEnumerator: TGpInt64ListWalkEnumerator;
  public
    constructor Create(aList: TGpInt64ObjectList; idxFrom, idxTo, step: integer);
    destructor  Destroy; override;
    function  GetCurrent: TGpKeyValue;              {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: TGpKeyValue read GetCurrent;
  end; { TGpInt64ObjectListWalkKVEnumerator }

  TGpInt64ObjectListWalkKVEnumeratorFactory = record
  private
    wkefList   : TGpInt64ObjectList;
    wkefIdxFrom: integer;
    wkefIdxTo  : integer;
    wkefStep   : integer;
  public
    constructor Create(list: TGpInt64ObjectList; idxFrom, idxTo, step: integer);
    function  GetEnumerator: TGpInt64ObjectListWalkKVEnumerator;
  end; { TGpInt64ObjectListWalkKVEnumeratorFactory }
  {$ENDIF GpLists_Enumerators}

  IGpInt64ObjectList = interface ['{BE86907A-2208-46E0-AD02-C9D6115E7351}']
    function  GetObject(idxObject: integer): TObject;
    procedure SetObject(idxObject: integer; const value: TObject);
    //
    function  Add(item: int64): integer;
    function  AddObject(item: int64; obj: TObject): integer;
    procedure Clear;
    procedure Delete(idx: integer);
    function  Dump(baseAddr: pointer): pointer;
    function  EnsureObject(item: int64; obj: TObject): integer; overload;
    function  EnsureObject(item: int64; objClass: TClass): integer; overload;
    procedure Exchange(idx1, idx2: integer);
    function  ExtractObject(idxObject: integer): TObject;
    function  FetchObject(item: int64): TObject;
    procedure FreeObjects;
    procedure Insert(idx: integer; item: int64);
    procedure InsertObject(idx: integer; item: int64; obj: TObject);
    function  LoadFromStream(stream: TStream): boolean;
    procedure Move(curIdx, newIdx: integer);
    function  Restore(baseAddr: pointer): pointer;
    procedure SaveToStream(stream: TStream);
    {$IFDEF GpLists_Enumerators}
    function  WalkKV(idxFrom: integer = 0; idxTo: integer = CUpperListBound;
      step: integer = 1): TGpInt64ObjectListWalkKVEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Objects[idxObject: integer]: TObject read GetObject write SetObject;
  end; { IGpInt64ObjectList }

  {:Int64 list where each integer is accompanied with an object.
    @since   2006-09-20
  }
  TGpInt64ObjectList = class(TGpInt64List, IGpInt64ObjectList)
  private
    iolObjects: TObjectList;
  protected
    function  GetObject(idxObject: integer): TObject; virtual;
    procedure SetObject(idxObject: integer; const value: TObject); virtual;
  public
    constructor Create(ownsObjects: boolean = true);
    destructor  Destroy; override;
    class function CreateInterface(ownsObjects: boolean = true): IGpInt64ObjectList;
    function  Add(item: int64): integer; override;
    function  AddObject(item: int64; obj: TObject): integer; virtual;
    procedure Clear; override;
    procedure Delete(idx: integer); override;
    function  Dump(baseAddr: pointer): pointer; override;
    function  EnsureObject(item: int64; obj: TObject): integer; overload; virtual;
    function  EnsureObject(item: int64; objClass: TClass): integer; overload; virtual;
    procedure Exchange(idx1, idx2: integer); override;
    function  ExtractObject(idxObject: integer): TObject;
    function  FetchObject(item: int64): TObject;
    procedure FreeObjects;
    procedure Insert(idx: integer; item: int64); override;
    procedure InsertObject(idx: integer; item: int64; obj: TObject); virtual;
    function  LoadFromStream(stream: TStream): boolean; override;
    procedure Move(curIdx, newIdx: integer); override;
    function  Restore(baseAddr: pointer): pointer; override;
    procedure SaveToStream(stream: TStream); override;
    {$IFDEF GpLists_Enumerators}
    function  WalkKV(idxFrom: integer = 0; idxTo: integer = CUpperListBound;
      step: integer = 1): TGpInt64ObjectListWalkKVEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Objects[idxObject: integer]: TObject read GetObject write SetObject;
  end; { TGpInt64ObjectList }

  {$IFDEF Unicode}
  {$IFNDEF GpLists_LimitedGenerics}
  IGpInt64ObjectList<T> = interface
    function  GetObject(idxObject: integer): T;
    procedure SetObject(idxObject: integer; const value: T);
    //
    function  AddObject(item: int64; obj: T): integer;
    function  EnsureObject(item: int64): integer; overload;
    function  EnsureObject(item: int64; obj: T): integer; overload;
    function  ExtractObject(idxObject: integer): T;
    function  FetchObject(item: int64): T;
    procedure InsertObject(idx: integer; item: int64; obj: T);
    property Objects[idxObject: integer]: T read GetObject write SetObject;
  end; { IGpInt64ObjectList<T> }

  {:Integer list where each integer is accompanied with a typed object.
    @since   2003-06-09
  }
  TGpInt64ObjectList<T: class, constructor> = class(TGpInt64ObjectList, IGpInt64ObjectList<T>)
  protected
    function  GetObject(idxObject: integer): T; reintroduce;
    procedure SetObject(idxObject: integer; const value: T); reintroduce;
  public
    class function CreateInterface(ownsObjects: boolean = true): IGpInt64ObjectList<T>;
    function  AddObject(item: int64; obj: T): integer; reintroduce; inline;
    function  EnsureObject(item: int64): integer; overload; inline;
    function  EnsureObject(item: int64; obj: T): integer; reintroduce; overload; inline;
    function  ExtractObject(idxObject: integer): T; reintroduce; inline;
    function  FetchObject(item: int64): T; reintroduce; inline;
    procedure InsertObject(idx: integer; item: int64; obj: T); reintroduce; inline;
    property Objects[idxObject: integer]: T read GetObject write SetObject;
  end; { TGpInt64ObjectList<T> }
  {$ENDIF GpLists_LimitedGenerics}
  {$ENDIF Unicode}

  IGpCountedInt64List = interface ['{74326A7E-F5FC-42CA-8322-43866D7C2437}']
    function  GetCounter(idx: integer): int64;
    function  GetItemCounter(item: int64): int64;
    procedure SetCounter(idx: integer; const value: int64);
    procedure SetItemCounter(item: int64; const value: int64);
    //
    function  Add(item: int64; count: int64): integer;
    function  Ensure(item: int64; count: int64): integer;
    function  Fetch(item: integer): int64;
    procedure Insert(idx: integer; item, count: int64);
    {$IFDEF GpLists_Sorting}
    procedure SortByCounter(descending: boolean = true);
    {$ENDIF}
    property Counter[idx: integer]: int64 read GetCounter write SetCounter;
    property ItemCounter[item: int64]: int64 read GetItemCounter write SetItemCounter;
  end; { IGpCountedInt64List }

  ///	<summary>A thin layer over TGpInt64Object list where each item has 64-bit
  ///	associated counter (stored in the Objects property).</summary>
  TGpCountedInt64List = class(TGpInt64ObjectList, IGpCountedInt64List)
  protected
    function  GetCounter(idx: integer): int64; virtual;
    function  GetItemCounter(item: int64): int64;
    procedure SetCounter(idx: integer; const value: int64); virtual;
    procedure SetItemCounter(item: int64; const value: int64);
  public
    class function CreateInterface(ownsObjects: boolean = true): IGpCountedInt64List;
    function  Add(item: int64; count: int64): integer; reintroduce;
    function  Ensure(item: int64; count: int64): integer; reintroduce;
    function  Fetch(item: integer): int64;
    procedure Insert(idx: integer; item, count: int64); reintroduce;
    procedure SortByCounter(descending: boolean = true);
    property Counter[idx: integer]: int64 read GetCounter write SetCounter;
    property ItemCounter[item: int64]: int64 read GetItemCounter write SetItemCounter;
  end; { TGpCountedInt64List }

  TGpGUIDRec = packed record
    case integer of
      0: (Lo, Hi: int64);
      1: (Guid: TGUID);
  end; { TGpGUIDRec }

  TGpGUIDList = class;

  TGpGUIDListSortCompare = function(List: TGpGUIDList; Index1, Index2: integer): integer;

  {$IFDEF GpLists_Enumerators}
  ///	<summary>TGpGUID list enumerator.</summary>
  TGpGUIDListEnumerator = class
  private
    gleIdxTo: integer;
    gleIndex: integer;
    gleList : TGpGUIDList;
    gleStep : integer;
  public
    constructor Create(aList: TGpGUIDList; idxFrom, idxTo, step: integer);
    function  GetCurrent: TGUID;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: TGUID read GetCurrent;
  end; { TGpGUIDListEnumerator }

  ///<summary>Partially stable enumerator that returns list indices instead of elements.
  ///   <para>Can handle deletions and insertions while the enumerator is in operation.</para></summary>
  TGpGUIDListWalkEnumerator = class
  private
    glweIdxTo: integer;
    glweIndex: integer;
    glweList : TGpGUIDList;
    glweStep : integer;
  protected
    procedure HandleListChange(list: TObject; idxItem: integer; operation: TGpListOperation);
    property List: TGpGUIDList read glweList;
  public
    constructor Create(aList: TGpGUIDList; idxFrom, idxTo, step: integer);
    destructor  Destroy; override;
    function  GetCurrent: integer;
    function  MoveNext: boolean;
    property Current: integer read GetCurrent;
  end; { TGpGUIDListWalkEnumerator }

  TGpGUIDListSliceEnumeratorFactory = class
  private
    sefList   : TGpGUIDList;
    sefIdxFrom: integer;
    sefIdxTo  : integer;
    sefStep   : integer;
  public
    constructor Create(list: TGpGUIDList; idxFrom, idxTo, step: integer);
    function  GetEnumerator: TGpGUIDListEnumerator;
  end; { TGpGUIDListSliceEnumeratorFactory }

  TGpGUIDListWalkEnumeratorFactory = class
  private
    wefList   : TGpGUIDList;
    wefIdxFrom: integer;
    wefIdxTo  : integer;
    wefStep   : integer;
  public
    constructor Create(list: TGpGUIDList; idxFrom, idxTo, step: integer);
    function  GetEnumerator: TGpGUIDListWalkEnumerator;
  end; { TGpGUIDListWalkEnumeratorFactory }
  {$ENDIF GpLists_Enumerators}

  IGpGUIDList = interface ['{7D9FE27F-D465-4EC6-BE88-37ACE22834ED}']
    function  GetCapacity: integer;
    function  GetCount: integer;
    function  GetDuplicates: TDuplicates;
    function  GetItems(idx: integer): TGUID;
    {$IFDEF GpLists_Sorting}
    function  GetSorted: boolean;
    {$ENDIF}
    function  GetText: string;
    procedure SetCapacity(const value: integer);
    procedure SetCount(const value: integer);
    procedure SetDuplicates(const value: TDuplicates);
    procedure SetItems(idx: integer; const value: TGUID);
    {$IFDEF GpLists_Sorting}
    procedure SetSorted(const value: boolean);
    {$ENDIF}
    procedure SetText(const value: string);
    //
    function  Add(item: TGUID): integer;
    procedure Append(const elements: array of TGUID); overload;
    procedure Append(list: TGpGUIDList); overload;
    function  AsDelimitedText(const delimiter: string): string;
    procedure Assign(const elements: array of TGUID); overload;
    procedure Assign(list: TGpGUIDList); overload;
    procedure Clear;
    function  Contains(const item: TGUID): boolean; overload;
    function  Contains(const item: TGUID; var idx: integer): boolean; overload;
    {$IFDEF GpLists_Sorting}
    procedure CustomSort(sortMethod: TGpGUIDListSortCompare);
    {$ENDIF}
    procedure Delete(idx: integer);
    function  Ensure(const item: TGUID): integer;
    function  EqualTo(list: TGpGUIDList): boolean;
    procedure Exchange(idx1, idx2: integer);
    function  Find(const avalue: TGUID; var idx: integer): boolean;
    function  First: TGUID;
    function  IndexOf(const item: TGUID): integer;
    procedure Insert(idx: integer; const item: TGUID);
    function  Last: TGUID;
    function  LoadFromStream(stream: TStream): boolean;
    procedure Move(curIdx, newIdx: integer);
    procedure RegisterNotification(notificationHandler: TGpListNotificationEvent);
    procedure Remove(const item: TGUID);
    procedure SaveToStream(stream: TStream);
    {$IFDEF GpLists_Sorting}
    procedure Sort;
    {$ENDIF}
    procedure UnregisterNotification(notificationHandler: TGpListNotificationEvent);
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpGUIDListEnumerator;
    function  Slice(idxFrom: integer; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpGUIDListSliceEnumeratorFactory;
    function  Walk(idxFrom: integer = 0; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpGUIDListWalkEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Items[idx: integer]: TGUID read GetItems write SetItems; default;
    {$IFDEF GpLists_Sorting}
    property Sorted: boolean read GetSorted write SetSorted;
    {$ENDIF}
    property Text: string read GetText write SetText;
  end; { IGpGUIDList }

  ///	<summary>A thin layer over TGpCountedInt64List; reusing 'item' for one half of the
  ///	GUID and 'count' for the other half.</summary>
  TGpGUIDList = class(TInterfacedObject, IGpGUIDList)
  private
    glDuplicates          : TDuplicates;
    glList                : TGpCountedInt64List;
    glNotificationHandlers: TGpTMethodList;
    glSorted              : boolean;
  protected
    function  GetAsDelimitedText(const delimiter: string;
      appendLastDelimiter: boolean): string;
    function  GetCapacity: integer; virtual;
    function  GetCount: integer; virtual;
    function  GetDuplicates: TDuplicates; virtual;
    function  GetItems(idx: integer): TGUID; virtual;
    {$IFDEF GpLists_Sorting}
    function  GetSorted: boolean; virtual;
    {$ENDIF}
    function  GetText: string; virtual;
    procedure InsertItem(idx: integer; const item: TGUID);
    procedure Notify(idxItem: integer; operation: TGpListOperation); {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure QuickSort(L, R: integer; SCompare: TGpGUIDListSortCompare);
    procedure SetCapacity(const value: integer); virtual;
    procedure SetCount(const value: integer); virtual;
    procedure SetDuplicates(const value: TDuplicates); virtual;
    procedure SetItems(idx: integer; const value: TGUID); virtual;
    {$IFDEF GpLists_Sorting}
    procedure SetSorted(const value: boolean); virtual;
    {$ENDIF}
    procedure SetText(const value: string); virtual;
  public
    constructor Create; overload;
    destructor  Destroy; override;
    class function CreateInterface: IGpGUIDList;
    function  Add(item: TGUID): integer; virtual;
    procedure Append(const elements: array of TGUID); overload;
    procedure Append(list: TGpGUIDList); overload; virtual;
    procedure Append(list: IGpGUIDList); overload; virtual;
    function  AsDelimitedText(const delimiter: string): string; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Assign(const elements: array of TGUID); overload;
    procedure Assign(list: TGpGUIDList); overload; virtual;
    procedure Assign(list: IGpGUIDList); overload; virtual;
    procedure Clear; virtual;
    function  Contains(const item: TGUID): boolean; overload;                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Contains(const item: TGUID; var idx: integer): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$IFDEF GpLists_Sorting}
    procedure CustomSort(sortMethod: TGpGUIDListSortCompare); virtual;
    {$ENDIF}
    procedure Delete(idx: integer); virtual;
    function  Ensure(const item: TGUID): integer; virtual;
    function  EqualTo(list: TGpGUIDList): boolean;
    procedure Exchange(idx1, idx2: integer); virtual;
    function  Find(const avalue: TGUID; var idx: integer): boolean; virtual;
    function  First: TGUID; virtual;
    function  IndexOf(const item: TGUID): integer;
    procedure Insert(idx: integer; const item: TGUID); virtual;
    function  Last: TGUID; virtual;
    function  LoadFromStream(stream: TStream): boolean; virtual;
    procedure Move(curIdx, newIdx: integer); virtual;
    procedure RegisterNotification(notificationHandler: TGpListNotificationEvent);
    procedure Remove(const item: TGUID); virtual;
    procedure SaveToStream(stream: TStream); virtual;
    {$IFDEF GpLists_Sorting}
    procedure Sort;                                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$ENDIF}
    procedure UnregisterNotification(notificationHandler: TGpListNotificationEvent);
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpGUIDListEnumerator; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Slice(idxFrom: integer; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpGUIDListSliceEnumeratorFactory;
    function  Walk(idxFrom: integer = 0; idxTo: integer = CUpperListBound; step: integer = 1):
      TGpGUIDListWalkEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Items[idx: integer]: TGUID read GetItems write SetItems; default;
    {$IFDEF GpLists_Sorting}
    property Sorted: boolean read GetSorted write SetSorted;
    {$ENDIF}
    property Text: string read GetText write SetText;
  end; { TGpGUIDList }

  {$IFDEF Unicode}
  {$IFNDEF GpLists_LimitedGenerics}
  IGpInterfaceListEnumerator<T: IInterface> = interface
    function  GetCurrent: T;
    function  MoveNext: boolean;
    property Current: T read GetCurrent;
  end; { MoveNext }

  IGpInterfaceList<T: IInterface> = interface
    function  GetCapacity: integer;
    function  GetCount: integer;
    function  GetItem(idx: integer): T;
    procedure SetCapacity(const value: integer);
    procedure SetCount(const value: integer);
    procedure SetItem(idx: integer; const value: T);
  //
    function  Add(const item: T): integer;
    procedure Clear;
    function  Contains(const item: T): boolean; overload;
    function  Contains(const item: T; var idx: integer): boolean; overload;
    procedure Delete(idx: integer);
    function  Ensure(const item: T): integer;
    procedure Exchange(idx1, idx2: integer);
    function  First: T;
    function  GetEnumerator: TList<T>.TEnumerator;
    function  IndexOf(const item: T): integer;
    procedure Insert(idx: integer; const item: T);
    function  Last: T;
    procedure Move(curIdx, newIdx: integer);
    procedure Remove(const item: T);
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Items[idx: integer]: T read GetItem write SetItem; default;
  end; { IGpInterfaceList<T> }

  TGpInterfaceList<T: IInterface> = class(TInterfacedObject, IGpInterfaceList<T>)
  strict private
    ilList: TList<T>;
  protected
    function  GetCapacity: integer;                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetCount: integer;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetItem(idx: integer): T;             {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure SetCapacity(const value: integer);    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure SetCount(const value: integer);       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure SetItem(idx: integer; const value: T);{$IFDEF GpLists_Inline}inline;{$ENDIF}
  public
    constructor Create;
    destructor  Destroy; override;
    class function CreateInterface: IGpInterfaceList<T>;
    function  Add(const item: T): integer;          {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Clear;                                {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Contains(const item: T): boolean; overload;                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Contains(const item: T; var idx: integer): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Delete(idx: integer);                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Ensure(const item: T): integer;       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Exchange(idx1, idx2: integer);        {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  First: T;                             {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetEnumerator: TList<T>.TEnumerator;  {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  IndexOf(const item: T): integer;      {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Insert(idx: integer; const item: T);  {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Last: T;                              {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Move(curIdx, newIdx: integer);        {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Remove(const item: T);                {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Items[idx: integer]: T read GetItem write SetItem; default;
  end; { TGpInterfaceList<T> }
  {$ENDIF GpLists_LimitedGenerics}
  {$ENDIF Unicode}

  {$IFDEF GpLists_TStringsHelper}
  TGpStringsHelperWalkKVEnumerator = record
  private
    sheIdxTo  : integer;
    sheIndex  : integer;
    sheStep   : integer;
    sheStrings: TStrings;
  public
    constructor Create(sl: TStrings; idxFrom, idxTo, step: integer);
    function  GetEnumerator: TGpStringsHelperWalkKVEnumerator;
    function  GetCurrent: TGpStringValue;
    function  MoveNext: boolean;
    property Current: TGpStringValue read GetCurrent;
  end; { TGpStringsHelperWalkKVEnumerator }

  ///<summary>Implements helpers for the TStrings.</summary>
  ///<since>2007-06-28</since>
  TGpStringsHelper = class helper for TStrings
  public
    function  Contains(const s: string): boolean; overload;                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Contains(const s: string; var idx: integer): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  FetchObject(const s: string): TObject;
    procedure FreeObjects;
    function  Last: string;                                                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Remove(const s: string);
    procedure RemoveObject(const s: string);
    {$IFDEF GpLists_Enumerators}
    function WalkKV(idxFrom: integer = 0; idxTo: integer = CUpperListBound; step: integer =
      1): TGpStringsHelperWalkKVEnumerator;
    {$ENDIF GpLists_Enumerators}
  end; { TGpStringsHelper }

  TGpStringListHelper = class helper for TStringList
  public
    procedure Sort;
  end; { TGpStringListHelper }
  {$ENDIF GpLists_TStringsHelper}

  {:String list where each item has associated counter (stored in the Objects property).
  }
  TGpCountedStringList = class(TStringList)
  protected
    function  GetItemCount(idx: integer): integer; virtual;
    procedure SetItemCount(idx: integer; const value: integer); virtual;
  public
    function  Add(const s: string; count: integer): integer; reintroduce;
    function  Contains(const s: string): boolean; overload;                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Contains(const s: string; var idx: integer): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Ensure(const s: string; count: integer): integer;
    procedure SortByCounter(descending: boolean = true);
    property  Counter[idx: integer]: integer read GetItemCount write SetItemCount;
  end; { TGpCountedStringList }

  {:AnsiString stream implementation, written by Remy Lebeau. Posted in the
    embarcadero.public.delphi.rtl newsgroup in 2011-05-18.
  }
  {$IFDEF MSWINDOWS}
  TAnsiStringStream = class(TStream)
  private
    FDataString: AnsiString;
    FPosition: integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AString: AnsiString);
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(Count: Longint): AnsiString;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const AString: AnsiString);
    property DataString: AnsiString read FDataString;
  end;
  {$ENDIF}

  {$IFDEF GpLists_Enumerators}
  {:TGpTMethodList enumerator.
    @since   2006-05-15
  }
  TGpTMethodListEnumerator = record
  private
    mleIndex: integer;
    mleList : TGpTMethodList;
  public
    constructor Create(aList: TGpTMethodList);
    function  GetCurrent: TMethod; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: TMethod read GetCurrent;
  end; { TGpTMethodListEnumerator }
  {$ENDIF GpLists_Enumerators}

  IGpTMethodList = interface ['{A6F17BE2-B20E-49C7-B992-DE845E82191A}']
    function  GetCapacity: integer;
    function  GetCount: integer;
    function  GetItems(idx: integer): TMethod;
    procedure SetCapacity(const value: integer);
    procedure SetCount(const value: integer);
    procedure SetItems(idx: integer; const value: TMethod);
    //
    function  Add(item: TMethod): integer;
    procedure Assign(list: TGpTMethodList);
    procedure Clear;
    function  Contains(item: TMethod): boolean; overload;
    function  Contains(item: TMethod; var idx: integer): boolean; overload;
    procedure Delete(idx: integer);
    function  Ensure(item: TMethod): integer;
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpTMethodListEnumerator;
    {$ENDIF GpLists_Enumerators}
    function  IndexOf(item: TMethod): integer;
    procedure Insert(idx: integer; item: TMethod);
    procedure Remove(item: TMethod);
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Items[idx: integer]: TMethod read GetItems write SetItems; default;
  end; { IGpTMethodList }

  {:List of TMethod records.
    @since   2006-05-15
  }
  TGpTMethodList = class(TInterfacedObject, IGpTMethodList)
  private
    mlCode: TList;
    mlData: TList;
  protected
    function  GetCapacity: integer;                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetCount: integer;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetItems(idx: integer): TMethod;      {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure SetCapacity(const value: integer);    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure SetCount(const value: integer);       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure SetItems(idx: integer; const value: TMethod); {$IFDEF GpLists_Inline}inline;{$ENDIF}
  public
    constructor Create;
    destructor  Destroy; override;
    class function CreateInterface: IGpTMethodList;
    function  Add(item: TMethod): integer;                                  {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Assign(list: TGpTMethodList); overload;                       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Clear;                                                        {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Contains(item: TMethod): boolean; overload;                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Contains(item: TMethod; var idx: integer): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Delete(idx: integer);                                         {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Ensure(item: TMethod): integer;                               {$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpTMethodListEnumerator;                      {$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$ENDIF GpLists_Enumerators}
    function  IndexOf(item: TMethod): integer;
    procedure Insert(idx: integer; item: TMethod);                          {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Remove(item: TMethod);                                        {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Items[idx: integer]: TMethod read GetItems write SetItems; default;
  end; { TGpTMethodList }

  {$IFDEF GpLists_Enumerators}
  TGpClassList = class;

  {:TGpClassList enumerator.
    @since   2007-10-26
  }
  TGpClassListEnumerator = record
  private
    cleIndex: integer;
    cleList : TGpClassList;
  public
    constructor Create(aList: TGpClassList);
    function  GetCurrent: TClass; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: TClass read GetCurrent;
  end; { TGpClassListEnumerator }
  {$ENDIF GpLists_Enumerators}

  IGpClassList = interface ['{9B7B590F-307B-4616-8CD5-BE0E713AC0CD}']
    function  GetCapacity: integer;
    function  GetCount: integer;
    function  GetItems(idx: integer): TClass;
    procedure SetCapacity(const value: integer);
    //
    function  Add(aClass: TClass): integer;
    procedure Clear;
    procedure Delete(idx: integer);
    function  CreateObject(const sClass: string): TObject;
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpClassListEnumerator;
    {$ENDIF GpLists_Enumerators}
    function  IndexOf(aClass: TClass): integer; overload;
    function  IndexOf(const sClass: string): integer; overload;
    procedure Remove(aClass: TClass); overload;
    procedure Remove(const sClass: string); overload;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount;
    property Items[idx: integer]: TClass read GetItems; default;
  end; { IGpClassList }

  {:TStringList-based list of classes. Class names must be unique. Useful when you need to
    implement a class registry to generate objects from class names.
    @since   2007-10-25
  }
  TGpClassList = class(TInterfacedObject, IGpClassList)
  private
    clClasses: TStringList;
  protected
    function  GetCapacity: integer;                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetCount: integer;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetItems(idx: integer): TClass;       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure SetCapacity(const value: integer);    {$IFDEF GpLists_Inline}inline;{$ENDIF}
  public
    constructor Create;
    destructor  Destroy; override;
    class function CreateInterface: IGpClassList;
    function  Add(aClass: TClass): integer;
    procedure Clear;                                {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Delete(idx: integer);                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  CreateObject(const sClass: string): TObject;
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpClassListEnumerator;{$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$ENDIF GpLists_Enumerators}
    function  IndexOf(aClass: TClass): integer; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  IndexOf(const sClass: string): integer; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Remove(aClass: TClass); overload;     {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Remove(const sClass: string); overload;     {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount;
    property Items[idx: integer]: TClass read GetItems; default;
  end; { TGpClassList }

  {$IFDEF MSWINDOWS}
  {$IFDEF GpLists_Enumerators}
  TGpObjectRingBuffer = class;

  TGpObjectRingBufferEnumerator = class
  private
    rbeIndex     : integer;
    rbeRingBuffer: TGpObjectRingBuffer;
  public
    constructor Create(ringBuffer: TGpObjectRingBuffer);
    destructor  Destroy; override;
    function  GetCurrent: TObject;                  {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: TObject read GetCurrent;
  end; { TGpObjectRingBufferEnumerator }
  {$ENDIF GpLists_Enumerators}
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  IGpObjectRingBuffer = interface ['{6DBFE065-87AF-47E5-AF03-92B07D5ACAC1}']
    function  GetBufferAlmostEmptyEvent: THandle;
    function  GetBufferAlmostEmptyThreshold: integer;
    function  GetBufferAlmostFullEvent: THandle;
    function  GetBufferAlmostFullThreshold: integer;
    function  GetItem(iObject: integer): TObject;
    function  GetOwnsObjects: boolean;
    procedure SetBufferAlmostEmptyEvent(const Value: THandle);
    procedure SetBufferAlmostEmptyThreshold(const Value: integer);
    procedure SetBufferAlmostFullEvent(const Value: THandle);
    procedure SetBufferAlmostFullThreshold(const Value: integer);
    procedure SetItem(iObject: integer; const value: TObject);
    //
    procedure Clear;
    function  Count: integer;
    function  Dequeue: TObject;
    function  Enqueue(obj: TObject): boolean;
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpObjectRingBufferEnumerator;
    {$ENDIF GpLists_Enumerators}
    function  Head: TObject;
    function  IsEmpty: boolean;
    function  IsFull: boolean;
    procedure Lock;
    function  Remove(obj: TObject): TObject;
    function  Tail: TObject;
    procedure Unlock;
    property BufferAlmostEmptyEvent: THandle read GetBufferAlmostEmptyEvent write
      SetBufferAlmostEmptyEvent;
    property BufferAlmostEmptyThreshold: integer read GetBufferAlmostEmptyThreshold write
      SetBufferAlmostEmptyThreshold;
    property BufferAlmostFullEvent: THandle read GetBufferAlmostFullEvent write
      SetBufferAlmostFullEvent;
    property BufferAlmostFullThreshold: integer read GetBufferAlmostFullThreshold write
      SetBufferAlmostFullThreshold;
    property Items[iObject: integer]: TObject read GetItem write SetItem; default;
    property OwnsObjects: boolean read GetOwnsObjects;
  end; { IGpObjectRingBuffer }
{$ENDIF}

  {:Fixed-size ring buffer of TObject references. Optionally thread-safe.
    @since   2003-07-25
  }
{$IFDEF MSWINDOWS}
  TGpObjectRingBuffer = class(TInterfacedObject, IGpObjectRingBuffer)
  private
    orbBuffer                    : array of TObject;
    orbBufferAlmostEmptyEvent    : THandle;
    orbBufferAlmostEmptyThreshold: integer;
    orbBufferAlmostFullEvent     : THandle;
    orbBufferAlmostFullThreshold : integer;
    orbBufferSize                : integer;
    orbCount                     : integer;
    orbHead                      : integer;
    orbLock                      : TCriticalSection;
    orbOwnsObjects               : boolean;
    orbTail                      : integer;
  protected
    function  GetBufferAlmostEmptyEvent: THandle; virtual;
    function  GetBufferAlmostEmptyThreshold: integer; virtual;
    function  GetBufferAlmostFullEvent: THandle; virtual;
    function  GetBufferAlmostFullThreshold: integer; virtual;
    function  GetItem(iObject: integer): TObject; virtual;
    function  GetOwnsObjects: boolean; virtual;
    function  IncPointer(const ptr: integer; increment: integer = 1): integer;
    function  InternalIsFull: boolean;              {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  InternalDequeue: TObject;
    procedure SetBufferAlmostEmptyEvent(const value: THandle); virtual;
    procedure SetBufferAlmostEmptyThreshold(const value: integer); virtual;
    procedure SetBufferAlmostFullEvent(const value: THandle); virtual;
    procedure SetBufferAlmostFullThreshold(const value: integer); virtual;
    procedure SetItem(iObject: integer; const value: TObject); virtual;
  public
    constructor Create(bufferSize: integer; ownsObjects: boolean = true;
      multithreaded: boolean = false);
    destructor  Destroy; override;
    class function CreateInterface(bufferSize: integer; ownsObjects: boolean = true;
      multithreaded: boolean = false): IGpObjectRingBuffer;
    procedure Clear;                                {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Count: integer;                       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Dequeue: TObject;                     {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Enqueue(obj: TObject): boolean;       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpObjectRingBufferEnumerator;{$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$ENDIF GpLists_Enumerators}
    function  Head: TObject;                        {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  IsEmpty: boolean;                     {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  IsFull: boolean;                      {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Lock;
    function  Remove(obj: TObject): TObject;
    function  Tail: TObject;                        {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Unlock;
    property BufferAlmostEmptyEvent: THandle read orbBufferAlmostEmptyEvent write
      orbBufferAlmostEmptyEvent;
    property BufferAlmostEmptyThreshold: integer read orbBufferAlmostEmptyThreshold write
      orbBufferAlmostEmptyThreshold;
    property BufferAlmostFullEvent: THandle read orbBufferAlmostFullEvent write
      orbBufferAlmostFullEvent;
    property BufferAlmostFullThreshold: integer read orbBufferAlmostFullThreshold write
      orbBufferAlmostFullThreshold;
    property BufferSize: integer read orbBufferSize;
    property Items[iObject: integer]: TObject read GetItem write SetItem; default;
    property OwnsObjects: boolean read orbOwnsObjects;
  end; { TGpObjectRingBuffer }
{$ENDIF}

{$IFDEF Unicode}
{$IFNDEF GpLists_LimitedGenerics}
  IGpRingBuffer<T> = interface
    function  GetBufferAlmostEmptyEvent: THandle;
    function  GetBufferAlmostEmptyThreshold: integer;
    function  GetBufferAlmostFullEvent: THandle;
    function  GetBufferAlmostFullThreshold: integer;
    function  GetItem(iItem: integer): T;
    procedure SetBufferAlmostEmptyEvent(const Value: THandle);
    procedure SetBufferAlmostEmptyThreshold(const Value: integer);
    procedure SetBufferAlmostFullEvent(const Value: THandle);
    procedure SetBufferAlmostFullThreshold(const Value: integer);
    procedure SetItem(iItem: integer; const value: T);
    //
    procedure Clear;
    function  Count: integer;
    function  Dequeue: T; overload;
    function  Dequeue(var item: T): boolean; overload;
    function  Enqueue(item: T): boolean;
    function  Head: T; overload;
    function  Head(var item: T): boolean; overload;
    function  IsEmpty: boolean;
    function  IsFull: boolean;
    procedure Lock;
    function  Tail: T; overload;
    function  Tail(var item: T): boolean; overload;
    procedure Unlock;
    property BufferAlmostEmptyEvent: THandle read GetBufferAlmostEmptyEvent write
      SetBufferAlmostEmptyEvent;
    property BufferAlmostEmptyThreshold: integer read GetBufferAlmostEmptyThreshold write
      SetBufferAlmostEmptyThreshold;
    property BufferAlmostFullEvent: THandle read GetBufferAlmostFullEvent write
      SetBufferAlmostFullEvent;
    property BufferAlmostFullThreshold: integer read GetBufferAlmostFullThreshold write
      SetBufferAlmostFullThreshold;
    property Items[iItem: integer]: T read GetItem write SetItem; default;
  end; { IGpRingBuffer<T> }

  {:Fixed-size ring buffer of T. Optionally thread-safe.
  }
  {$IFDEF MSWINDOWS}
  TGpRingBuffer<T> = class(TInterfacedObject, IGpRingBuffer<T>)
  private
    orbBuffer                    : array of T;
    orbBufferAlmostEmptyEvent    : THandle;
    orbBufferAlmostEmptyThreshold: integer;
    orbBufferAlmostFullEvent     : THandle;
    orbBufferAlmostFullThreshold : integer;
    orbBufferSize                : integer;
    orbCount                     : integer;
    orbHead                      : integer;
    orbLock                      : TCriticalSection;
    orbTail                      : integer;
  protected
    function  GetBufferAlmostEmptyEvent: THandle; virtual;
    function  GetBufferAlmostEmptyThreshold: integer; virtual;
    function  GetBufferAlmostFullEvent: THandle; virtual;
    function  GetBufferAlmostFullThreshold: integer; virtual;
    function  GetItem(iItem: integer): T; virtual;
    function  IncPointer(const ptr: integer; increment: integer = 1): integer;
    function  InternalIsFull: boolean;              {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  InternalDequeue(var item: T): boolean;
    procedure SetBufferAlmostEmptyEvent(const value: THandle); virtual;
    procedure SetBufferAlmostEmptyThreshold(const value: integer); virtual;
    procedure SetBufferAlmostFullEvent(const value: THandle); virtual;
    procedure SetBufferAlmostFullThreshold(const value: integer); virtual;
    procedure SetItem(iItem: integer; const value: T); virtual;
  public
    constructor Create(bufferSize: integer; multithreaded: boolean = false);
    destructor  Destroy; override;
    class function CreateInterface(bufferSize: integer;
      multithreaded: boolean = false): IGpRingBuffer<T>;
    procedure Clear;                                {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Count: integer;                       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Dequeue: T; overload;                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Dequeue(var item: T): boolean; overload;{$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Enqueue(item: T): boolean;            {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Head: T; overload;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Head(var item: T): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  IsEmpty: boolean;                     {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  IsFull: boolean;                      {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Lock;
    function  Tail: T; overload;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Tail(var item: T): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Unlock;
    property BufferAlmostEmptyEvent: THandle read orbBufferAlmostEmptyEvent write
      orbBufferAlmostEmptyEvent;
    property BufferAlmostEmptyThreshold: integer read orbBufferAlmostEmptyThreshold write
      orbBufferAlmostEmptyThreshold;
    property BufferAlmostFullEvent: THandle read orbBufferAlmostFullEvent write
      orbBufferAlmostFullEvent;
    property BufferAlmostFullThreshold: integer read orbBufferAlmostFullThreshold write
      orbBufferAlmostFullThreshold;
    property Items[iItem: integer]: T read GetItem write SetItem; default;
  end; { TGpRingBuffer<T> }
{$ENDIF ~GpLists_LimitedGenerics}
{$ENDIF Unicode}
{$ENDIF MSWINDOWS}

  {:Object map comparision function.
    @since   2003-08-02
  }        
  TGpObjectMapCompare = function(userValue, mapValue: TObject): boolean;

  IGpObjectMap = interface ['{24981792-92A9-4BB2-98B1-14CD5845B52C}']
    function  GetIndexedItem(idxItem: integer): TObject;
    function  GetIndexedValue(idxValue: integer): TObject;
    function  GetItems(item: TObject): TObject;
    procedure SetItems(item: TObject; const value: TObject);
    //
    procedure Clear;
    function  Count: integer;
    function  Exists(value: TObject; compareFunc: TGpObjectMapCompare): boolean;
    procedure Find(value: TObject; compareFunc: TGpObjectMapCompare;
      var item: TObject);
    property Items[idxItem: integer]: TObject read GetIndexedItem;
    property ValuesIdx[idxValue: integer]: TObject read GetIndexedValue;
    property Values[item: TObject]: TObject read GetItems write SetItems; default;
  end; { IGpObjectMap }

  {:List, indexed by objects and containing objects.
    @since   2003-08-02
  }
  TGpObjectMap = class(TInterfacedObject, IGpObjectMap)
  private
    omList: TGpInt64ObjectList;
  protected
    function  GetIndexedItem(idxItem: integer): TObject;   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetIndexedValue(idxValue: integer): TObject; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetItems(item: TObject): TObject; virtual;
    procedure SetItems(item: TObject; const value: TObject); virtual;
  public
    constructor Create(ownsObjects: boolean = true); overload;
    destructor  Destroy; override;
    class function CreateInterface(ownsObjects: boolean = true): IGpObjectMap;
    procedure Clear; virtual;
    function  Count: integer;                       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Exists(value: TObject; compareFunc: TGpObjectMapCompare): boolean;
    procedure Find(value: TObject; compareFunc: TGpObjectMapCompare;
      var item: TObject); virtual;
    property Items[idxItem: integer]: TObject read GetIndexedItem;
    property ValuesIdx[idxValue: integer]: TObject read GetIndexedValue;
    property Values[item: TObject]: TObject read GetItems write SetItems; default;
  end; { TGpObjectMap }

  IGpObjectObjectMap = interface ['{927A83FB-D348-425D-8A03-75028821BEFF}']
    function  GetItems(item1, item2: TObject): TObject;
    function  Map(item: TObject): TGpObjectMap;
    procedure SetItems(item1, item2: TObject; const value: TObject);
    //
    procedure Clear;
    function  Exists(value: TObject; compareFunc: TGpObjectMapCompare): boolean;
    procedure Find(value: TObject; compareFunc: TGpObjectMapCompare; var item1,
      item2: TObject);
    property Values[item1, item2: TObject]: TObject read GetItems write SetItems; default;
  end; { IGpObjectObjectMap }

  {:Matrix, indexed by two objects and containing objects.
    @since   2003-08-02
  }
  TGpObjectObjectMap = class(TInterfacedObject, IGpObjectObjectMap)
  private
    oomCompareFunc: TGpObjectMapCompare;
    oomFindValue  : TObject;
    oomItem2      : TObject;
    oomMap        : TGpObjectMap;
    oomOwnsObjects: boolean;
  protected
    function  GetItems(item1, item2: TObject): TObject; virtual;
    function  Map(item: TObject): TGpObjectMap; virtual;
    procedure SetItems(item1, item2: TObject; const value: TObject); virtual;
  public
    constructor Create(ownsObjects: boolean = true); overload;
    destructor  Destroy; override;
    class function CreateInterface(ownsObjects: boolean = true): IGpObjectObjectMap;
    procedure Clear; virtual;
    function  Exists(value: TObject; compareFunc: TGpObjectMapCompare): boolean; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Find(value: TObject; compareFunc: TGpObjectMapCompare; var item1,
      item2: TObject);
    property Values[item1, item2: TObject]: TObject read GetItems write SetItems; default;
  end; { TGpObjectObjectMap }

  TGpDoublyLinkedList = class;

  {:Ancestor for objects that can be insterted in a doubly-linked list.
    @since   2003-10-27
  }
  TGpDoublyLinkedListObject = class
  private
    dlloList    : TGpDoublyLinkedList;
    dlloNext    : TGpDoublyLinkedListObject;
    dlloPrevious: TGpDoublyLinkedListObject;
  protected
    procedure LinkAfter(list: TGpDoublyLinkedList; obj: TGpDoublyLinkedListObject);
    function  NextUnsafe: TGpDoublyLinkedListObject;
    function  PreviousUnsafe: TGpDoublyLinkedListObject;
    procedure Unlink;
  public
    destructor Destroy; override;
    function  Next: TGpDoublyLinkedListObject;
    function  Previous: TGpDoublyLinkedListObject;
    property List: TGpDoublyLinkedList read dlloList;
  end; { TGpDoublyLinkedListObject }

  {$IFDEF GpLists_Enumerators}
  TGpDoublyLinkedListEnumerator = class
  private
    dlleElement: TGpDoublyLinkedListObject;
    dlleList   : TGpDoublyLinkedList;
  public
    constructor Create(dlList: TGpDoublyLinkedList);
    destructor  Destroy; override;
    function  GetCurrent: TGpDoublyLinkedListObject;{$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  MoveNext: boolean;                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Current: TGpDoublyLinkedListObject read GetCurrent;
  end; { TGpDoublyLinkedListEnumerator }

  {$IFDEF Unicode}
  {$IFNDEF GpLists_LimitedGenerics}
  TGpDoublyLinkedListEnumerator<T:TGpDoublyLinkedListObject> = class
  private
    dlleEnumerator: TGpDoublyLinkedListEnumerator;
  public
    constructor Create(dlList: TGpDoublyLinkedList);
    destructor  Destroy; override;
    function  GetCurrent: T;
    function  MoveNext: boolean;
    property Current: T read GetCurrent;
  end; { TGpDoublyLinkedListEnumerator<T> }

  TGpDoublyLinkedListEnumeratorFactory<T:TGpDoublyLinkedListObject> = class
  private
    dllefList: TGpDoublyLinkedList;
  public
    constructor Create(dlList: TGpDoublyLinkedList);
    function  GetEnumerator: TGpDoublyLinkedListEnumerator<T>;
  end; { TGpDoublyLinkedListEnumeratorFactory<T> }
  {$ENDIF ~GpLists_LimitedGenerics}
  {$ENDIF Unicode}
  {$ENDIF GpLists_Enumerators}

  IGpDoublyLinkedList = interface ['{586756F9-3EB6-4965-B796-5074DE52215A}']
    function  Count: integer;
    procedure FreeAll;
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpDoublyLinkedListEnumerator;
    {$ENDIF GpLists_Enumerators}
    function  Head: TGpDoublyLinkedListObject;
    procedure InsertAfter(existingObject: TGpDoublyLinkedListObject;
      obj: TGpDoublyLinkedListObject);
    procedure InsertAtHead(obj: TGpDoublyLinkedListObject);
    procedure InsertAtTail(obj: TGpDoublyLinkedListObject);
    procedure InsertBefore(existingObject: TGpDoublyLinkedListObject;
      obj: TGpDoublyLinkedListObject);
    function  IsEmpty: boolean;
    procedure Lock;
    function  Next(obj: TGpDoublyLinkedListObject): TGpDoublyLinkedListObject;
    function  Previous(obj: TGpDoublyLinkedListObject): TGpDoublyLinkedListObject;
    function  RemoveFromHead: TGpDoublyLinkedListObject;
    function  RemoveFromTail: TGpDoublyLinkedListObject;
    function  Tail: TGpDoublyLinkedListObject;
    procedure Unlink(obj: TGpDoublyLinkedListObject);
    procedure UnlinkAll;
    procedure Unlock;
  end; { IGpDoublyLinkedList }

  {:A list of doubly-linked TGpDoublyLinkedListObject objects. NOT an owner of linked
    objects. Optionally thread-safe.
    @since   2003-10-27
  }
  TGpDoublyLinkedList = class(TInterfacedObject, IGpDoublyLinkedList)
  private
    dllCount: integer;
    dllHead : TGpDoublyLinkedListObject;
    dllLock : TCriticalSection;
    dllTail : TGpDoublyLinkedListObject;
  protected
    function  HeadUnsafe: TGpDoublyLinkedListObject;
    procedure InsertAtHeadUnsafe(obj: TGpDoublyLinkedListObject); {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure InsertAtTailUnsafe(obj: TGpDoublyLinkedListObject); {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Linking(obj: TGpDoublyLinkedListObject);      {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  TailUnsafe: TGpDoublyLinkedListObject;
    procedure Unlinking(obj: TGpDoublyLinkedListObject);    {$IFDEF GpLists_Inline}inline;{$ENDIF}
  public
    constructor Create(multithreaded: boolean = false); overload;
    destructor  Destroy; override;
    class function CreateInterface(multithreaded: boolean = false): IGpDoublyLinkedList;
    function  Count: integer;                               {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure FreeAll;
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpDoublyLinkedListEnumerator; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$IFDEF Unicode}
    {$IFNDEF GpLists_LimitedGenerics}
    function EnumerateAs<T: TGpDoublyLinkedListObject>:
      TGpDoublyLinkedListEnumeratorFactory<T>; overload;
    {$ENDIF ~GpLists_LimitedGenerics}
    {$ENDIF Unicode}
    {$ENDIF GpLists_Enumerators}
    function  Head: TGpDoublyLinkedListObject;
    procedure InsertAfter(existingObject: TGpDoublyLinkedListObject;
      obj: TGpDoublyLinkedListObject);                      {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure InsertAtHead(obj: TGpDoublyLinkedListObject); {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure InsertAtTail(obj: TGpDoublyLinkedListObject); {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure InsertBefore(existingObject: TGpDoublyLinkedListObject;
      obj: TGpDoublyLinkedListObject);                      {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  IsEmpty: boolean;                             {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Lock;                                         {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Next(obj: TGpDoublyLinkedListObject): TGpDoublyLinkedListObject; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Previous(obj: TGpDoublyLinkedListObject): TGpDoublyLinkedListObject; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  RemoveFromHead: TGpDoublyLinkedListObject;
    function  RemoveFromTail: TGpDoublyLinkedListObject;
    function  Tail: TGpDoublyLinkedListObject;
    procedure Unlink(obj: TGpDoublyLinkedListObject);       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure UnlinkAll;
    procedure Unlock;                                       {$IFDEF GpLists_Inline}inline;{$ENDIF}
  end; { TGpDoublyLinkedList }

  TGpFifoMemoryEvent = procedure(Sender: TObject; bufSize: integer; var buf: pointer) of object;

  ///<summary>Linked list of buffers with enforced maximum size and simple stream-like read access.</summary>
  IGpFifoBuffer = interface ['{2494FAE8-3113-4FB6-BD4F-594D9472CF5D}']
    function  GetDataSize: integer;
    function  GetOnFreeMem: TGpFifoMemoryEvent;
    function  GetOnGetMem: TGpFifoMemoryEvent;
    function  GetSize: integer;
    procedure SetOnFreeMem(value: TGpFifoMemoryEvent);
    procedure SetOnGetMem(value: TGpFifoMemoryEvent);
    procedure SetSize(value: integer);
  //
    function  FifoPlace: integer;
    {$IFDEF GpLists_RegionsSupported}{$REGION 'Documentation'} {$ENDIF}
    ///	<summary>Reads all available data from the FIFO into a
    ///	stream.</summary>
    ///	<param name="data">Output stream. Original content is preserved. Data
    ///	is stored at current position.</param>
    ///	<param name="maxSize">Maximum number of bytes to read.</param>
    ///	<returns>Number of bytes actually read.</returns>
    {$IFDEF GpLists_RegionsSupported}{$ENDREGION} {$ENDIF}
    function  Read(data: TStream; maxSize: integer = MaxInt): integer;
    {$IFDEF GpLists_RegionsSupported}{$REGION 'Documentation'}{$ENDIF}
    ///	<summary>Adds a copy of the buffer to the FIFO. If there's not enough
    ///	place for complete buffer, it will add just a part of the buffer and
    ///	return False.</summary>
    ///	<returns>True if full buffer was added to the FIFO. False if FIFO was
    ///	too full to contain complete buffer.</returns>
    {$IFDEF GpLists_RegionsSupported}{$ENDREGION}{$ENDIF}
    function  Write(const buf; bufSize: integer): boolean; overload;
    {$IFDEF GpLists_RegionsSupported}{$REGION 'Documentation'}{$ENDIF}
    ///	<summary>Adds a copy of the stream to the FIFO. If there's not enough
    ///	place for all stream data, it will add just a part of the stream and
    ///	return False. Data is read from the current position of the 'data' stream.</summary>
    ///	<param name="dataSize">Number of bytes to copy from the stream.
    /// Default: Copy entire stream.</param>
    ///	<returns>True if all data was added to the FIFO. False if FIFO was too
    ///	full to contain complete all stream data.</returns>
    {$IFDEF GpLists_RegionsSupported}{$ENDREGION}{$ENDIF}
    function  Write(data: TStream; dataSize: integer = MaxInt): boolean; overload;
    property DataSize: integer read GetDataSize;
    property Size: integer read GetSize write SetSize;
    property OnFreeMem: TGpFifoMemoryEvent read GetOnFreeMem write SetOnFreeMem;
    property OnGetMem: TGpFifoMemoryEvent read GetOnGetMem write SetOnGetMem;
  end; { IGpFifoBuffer }

  TFifoBlock = class(TGpDoublyLinkedListObject)
  {$IFDEF USE_STRICT} strict {$ENDIF} private
    FData          : pointer;
    FDataSize      : integer;
    FMemEventSender: TObject;
    FOnGetMem      : TGpFifoMemoryEvent;
    FOnFreeMem     : TGpFifoMemoryEvent;
  public
    {$IFDEF VER150}
    constructor CreateD7(const buf; bufSize: integer; memoryEventSender: TObject = nil;
      onGetMem: TGpFifoMemoryEvent = nil; onFreeMem: TGpFifoMemoryEvent = nil); overload;
    {$ELSE}
    constructor Create(const buf; bufSize: integer; memoryEventSender: TObject = nil;
      onGetMem: TGpFifoMemoryEvent = nil; onFreeMem: TGpFifoMemoryEvent = nil); overload;
    {$ENDIF}
    constructor Create(const data: TStream; bufSize: integer; memoryEventSender: TObject = nil;
      onGetMem: TGpFifoMemoryEvent = nil; onFreeMem: TGpFifoMemoryEvent = nil); overload;
    destructor  Destroy; override;
    procedure Reallocate(bufSize: integer);
    property Data: pointer read FData;
    property Size: integer read FDataSize;
  end; { TFifoBlock }

  TGpFifoBuffer = class(TInterfacedObject, IGpFifoBuffer)
  {$IFDEF USE_STRICT} strict {$ENDIF} private
    FActiveBlock     : TStream;
    FActiveBlockInUse: boolean;
    FBlocksToCache   : integer;
    FCachedBlocks    : TGpDoublyLinkedList;
    FCurrentSize     : integer;
    FFifo            : TGpDoublyLinkedList;
    FLastThreadId    : cardinal;
    FMaxSize         : integer;
    FOnGetMem        : TGpFifoMemoryEvent;
    FOnFreeMem       : TGpFifoMemoryEvent;
  {$IFDEF USE_STRICT} strict {$ENDIF} protected
    procedure AddBlock(block: TFifoBlock);          {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Truncate;
    procedure VerifyList;
  protected
    function  AllocateBlockBuf(const buf; bufSize: integer): TFifoBlock;         {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  AllocateBlockStream(data: TStream; dataSize: integer): TFifoBlock; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetDataSize: integer;                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetOnFreeMem: TGpFifoMemoryEvent;       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetOnGetMem: TGpFifoMemoryEvent;        {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetSize: integer;                       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure SetOnFreeMem(value: TGpFifoMemoryEvent);{$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure SetOnGetMem(value: TGpFifoMemoryEvent); {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure ReleaseBlock(var block: TFifoBlock);    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetCachedBlock(bufSize: integer): TFifoBlock;
    procedure SetSize(value: integer);                {$IFDEF GpLists_Inline}inline;{$ENDIF}
  public
    constructor Create(maxSize: integer; threadSafe: boolean; numBlocksToCache: integer = 0);
    destructor  Destroy; override;
    class function CreateInterface(maxSize: integer; threadSafe: boolean): IGpFifoBuffer;
    procedure Clear;
    function  FifoPlace: integer;                     {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Read(data: TStream; maxSize: integer = MaxInt): integer;
    function  Write(const buf; bufSize: integer): boolean; overload;               {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Write(data: TStream; dataSize: integer = MaxInt): boolean; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property DataSize: integer read GetDataSize;
    property Size: integer read GetSize write SetSize;
    property OnFreeMem: TGpFifoMemoryEvent read GetOnFreeMem write SetOnFreeMem;
    property OnGetMem: TGpFifoMemoryEvent read GetOnGetMem write SetOnGetMem;
  end; { TGpFifoBuffer }

{$IFDEF Unicode}
{$IFNDEF GpLists_LimitedGenerics}
const //http://www.csee.umbc.edu/courses/undergraduate/341/fall01/Lectures/SkipLists/skip_lists/skip_lists.html
  CDefaultLevelProbability = 0.25;
  CDefaultMaxLevels = 8; //good approximation: log(MaxElements)/log(1/LevelProbability)
                         //8 levels are fine for max 65536 elements and probability 0.25

type
  TGpSkipListEl<T> = class;

  TGpSkipListEl<T> = class
  strict private
    FElement : T;
    FPointers: array of TGpSkipListEl<T>;
  strict protected
    function  GetPointers(idx: integer): TGpSkipListEl<T>;
    procedure SetPointers(idx: integer; const value: TGpSkipListEl<T>);
  public
    constructor Create(const el: T; numLevels: integer); overload;
    constructor Create(numLevels: integer); overload;
    property Element: T read FElement write FElement;
    property Pointers[idx: integer]: TGpSkipListEl<T> read GetPointers write SetPointers;
  end; { TGpSkipListEl<T> }

  TGpSkipListCompare<K> = {$IFDEF Unicode}reference to{$ENDIF}
    function (const key1, key2: K): integer; {<0 => el1 < el2; 0 => el1 = el2; >0 => el1 > el2}

  TGpSkipListExtractKey<T,K> = {$IFDEF Unicode}reference to{$ENDIF}
    function (const el: T): K;

  TGpSkipListEnumerator<T> = record
  private
    FHead   : TGpSkipListEL<T>;
    FCurrent: TGpSkipListEl<T>;
    FTail   : TGpSkipListEl<T>;
  public
    constructor Create(head, tail: TGpSkipListEl<T>);
    function GetCurrent: T; inline;
    function MoveNext: boolean; inline;
    property Current: T read GetCurrent;
  end;

  TGpSkipList<T,K> = class
  strict private
    FComparer : TGpSkipListCompare<K>;
    FCount    : integer;
    FGetKey   : TGpSkipListExtractKey<T,K>;
    FHead     : TGpSkipListEl<T>;
    FLast     : array of TGpSkipListEl<T>;
    FLevelProb: real;
    FMaxLevels: integer;
    FTail     : TGpSkipListEl<T>;
    FTopLevel : integer;
  private
    procedure RemoveElements;
  strict protected
    function  GetKey(const el: T): K; inline;
    function  Compare(const key1, key2: K): integer; overload; inline;
    function  Compare(const key: K; el: TGpSkipListEl<T>): integer; overload; inline;
    function  RandomLevel: integer;
    function  WalkDown (const key: K; var x: TGpSkipListEl<T>): boolean;
  protected
    procedure Initialize;
  public
    constructor Create(
      keyExtractor: TGpSkipListExtractKey<T,K>;
      comparer: TGpSkipListCompare<K>;
      levelProbability: real = CDefaultLevelProbability;
      maxLevels: integer = CDefaultMaxLevels);
    destructor  Destroy; override;
    procedure Clear; virtual;
    function  Contains(const key: K): boolean; overload; inline;
    function  ContainsElement(const element: T): boolean; overload; inline;
    function  Count: integer; inline;
    function  Delete(const key: K): boolean; virtual;
    procedure DeleteElement(const element: T); inline;
    function  GetEnumerator: TGpSkipListEnumerator<T>; inline;
    function  Insert(const element: T): boolean; overload;
    function  Insert(const element: T; var position: TGpSkipListEl<T>): boolean; overload;
    function  IsEmpty: boolean; inline;
    function  Locate(const key: K; var element: T): boolean; overload;
    function  Locate(const key: K; var element: TGpSkipListEl<T>): boolean; overload;
    function  Next(el: TGpSkipListEl<T>): TGpSkipListEl<T>; inline;
    function  Replace(position: TGpSkipListEl<T>; const element: T): T; inline;
    property Head: TGpSkipListEl<T> read FHead;
    property Tail: TGpSkipListEl<T> read FTail;
  end; { TGpSkipList<T,K> }

  TGpSkipList<T> = class
  strict private
    FSkipList: TGpSkipList<T,T>;
  strict protected
    function  GetHead: TGpSkipListEl<T>; inline;
    function  GetTail: TGpSkipListEl<T>; inline;
  public
    constructor Create(
      comparer: TGpSkipListCompare<T>;
      levelProbability: real = CDefaultLevelProbability;
      maxLevels: integer = CDefaultMaxLevels);
    destructor  Destroy; override;
    class function CopyKeyExtractor(const el: T): T; inline;
    procedure Clear; virtual;
    function  Contains(const key: T): boolean; inline;
    function  Count: integer; inline;
    procedure Delete(const element: T); virtual;
    function  GetEnumerator: TGpSkipListEnumerator<T>; inline;
    function  Insert(const element: T): boolean; overload; inline;
    function  Insert(const element: T; var position: TGpSkipListEl<T>): boolean; overload; inline;
    function  IsEmpty: boolean; inline;
    function  Locate(const key: T; var element: T): boolean; overload; inline;
    function  Locate(const key: T; var element: TGpSkipListEl<T>): boolean; overload; inline;
    procedure Remove(const element: T); virtual;
    function  Replace(position: TGpSkipListEl<T>; const element: T): T; virtual;
    function Next(el: TGpSkipListEl<T>): TGpSkipListEl<T>; inline;
    property Head: TGpSkipListEl<T> read GetHead;
    property Tail: TGpSkipListEl<T> read GetTail;
  end; { TGpSkipList<T> }

  TGpSkipObjectList<T: class> = class(TGpSkipList<T>)
  strict private
    FOwnsObjects: boolean;
  public
    constructor Create(
      comparer: TGpSkipListCompare<T>;
      ownsObjects: boolean = true;
      levelProbability: real = CDefaultLevelProbability;
      maxLevels: integer = CDefaultMaxLevels);
    procedure Clear; override;
    procedure Delete(const element: T); override;
  end; { TGpSkipObjectList<T> }

  {$IF CompilerVersion >= 24} //XE2 has problems compiling this class

  ///  The TGpCache class maintains a dictionary of (key, index) pairs where
  ///  an 'index' is a pointer into a MRU linked list of values.
  ///  That allows us to remove the leastrecently used key from the dictionary
  //   when the cache becomes full in O(1) time.
  ///  As the linked list has a known maximum size, it is stored as an
  ///  array of list elements and 'index' from the dictionary is just the
  ///  element number.

  TGpCache<K,V> = class
  strict private
  const
    NilPointer = -1;
  type
    TListElement = record
      Next : integer;
      Prev : integer;
      Key  : K;
      Value: V;
    end;
    PListElement = ^TListElement;
  var
    FCache     : TDictionary<K,integer>;
    FFreeList  : integer;
    FHead      : integer;
    FKeys      : TArray<TListElement>;
    FOwnsValues: boolean;
    FTail      : integer;
  strict protected
    function  GetFree: integer;                                                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  IsNil(element: integer): boolean;                                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  RemoveOldest: integer;
    procedure BuildLinkedList(numElements: integer);
    procedure DestroyOwnedValues;
    procedure InsertInFront(elementIdx: integer);
    procedure Unlink(element: integer);
  public
    constructor Create(ANumElements: integer;
      const AComparer: IEqualityComparer<K> = nil;
      AOwnsValues: boolean = false); overload;
    destructor  Destroy; override;
    function  Remove(const key: K): boolean;
    function  IsFull: boolean;                                                    {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  TryGetValue(const key: K; var value: V): boolean;                   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Update(const key: K; const value: V);
  end; { TGpCache<K,V> }
{$IFEND CompilerVersion >= 24}
{$ENDIF}
{$ENDIF}

  {:Compares two TGpInt64objects for equality. Ready for use in
    TGpObject(Object)Map.
  }
  function GpCompareInt64(userValue, mapValue: TObject): boolean; {$IFDEF GpLists_Inline}inline;{$ENDIF}

  {:Useful helper function.
    @returns <0, 0, >0; depending on the result of the comparison
    @since   2003-12-18
  }
  function IntegerCompare(avalue1, avalue2: integer): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}

  {:Useful helper function.
    @returns <0, 0, >0; depending on the result of the comparison
    @since   2006-09-20
  }
  function Int64Compare(avalue1, avalue2: int64): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}

implementation

uses
  TypInfo;

{$IFDEF ConditionalExpressions}
{$IF CompilerVersion <= 20} //D2009 or older
type
  NativeInt  = integer;
  NativeUInt = cardinal;
{$IFEND}
{$ELSE}
type
  NativeInt  = integer;
  NativeUInt = cardinal;
{$ENDIF}

{ publics }

function IntegerCompare(avalue1, avalue2: integer): integer;
begin
  if avalue1 < avalue2 then
    Result := -1
  else if avalue1 > avalue2 then
    Result := 1
  else
    Result := 0;
end; { IntegerCompare }

function Int64Compare(avalue1, avalue2: int64): integer;
begin
  if avalue1 < avalue2 then
    Result := -1
  else if avalue1 > avalue2 then
    Result := 1
  else
    Result := 0;
end; { Int64Compare }

function GUIDCompare(avalue1, avalue2: TGUID): integer;
var
  guid1: TGUID absolute avalue1;
  guid2: TGUID absolute avalue2;
begin
  if guid1.D1 < guid2.D1 then
    Result := -1
  else if guid1.D1 > guid2.D1 then
    Result := 1
  else if guid1.D2 < guid2.D2 then
    Result := -1
  else if guid1.D2 > guid2.D2 then
    Result := 1
  else if guid1.D3 < guid2.D3 then
    Result := -1
  else if guid1.D3 > guid2.D3 then
    Result := 1
  else if DWORD(guid1.D4[0]) < DWORD(guid2.D4[0]) then
    Result := -1
  else if DWORD(guid1.D4[0]) > DWORD(guid2.D4[0]) then
    Result := 1
  else if DWORD(guid1.D4[4]) < DWORD(guid2.D4[4]) then
    Result := -1
  else if DWORD(guid1.D4[4]) > DWORD(guid2.D4[4]) then
    Result := 1
  else
    Result := 0;
end; { GUIDCompare }

function GpCompareInt64(userValue, mapValue: TObject): boolean;
begin
  Result := (TGpInt64(userValue).Value = TGpInt64(mapValue).Value);
end; { GpCompareInt64 }

function CreateFifoBuffer(maxSize: integer; threadSafe: boolean): IGpFifoBuffer;
begin
  Result := TGpFifoBuffer.Create(maxSize, threadSafe);
end; { CreateFifoBuffer }

{ globals }

function IntegerListCompare(List: TGpIntegerList; idx1, idx2: integer): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}
begin
  Result := IntegerCompare(List[idx1], List[idx2]);
end; { IntegerListCompare }

function Int64ListCompare(List: TGpInt64List; idx1, idx2: integer): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}
begin
  Result := Int64Compare(List[idx1], List[idx2]);
end; { Int64ListCompare }

function GUIDListCompare(List: TGpGUIDList; idx1, idx2: integer): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}
begin
  Result := GUIDCompare(List[idx1], List[idx2]);
end; { GUIDListCompare }

{ TGpInt64 }

constructor TGpInt64.Create(aValue: int64);
begin
  inherited Create;
  i64Value := aValue;
end; { TGpInt64.Create }

{ TGpDateTime }

constructor TGpDateTime.Create(aValue: TDateTime);
begin
  inherited Create;
  dtValue := aValue;
end; { TGpDateTime.Create }

{ TGpString }

constructor TGpString.Create(const aValue: string);
begin
  inherited Create;
  sValue := aValue;
end; { TGpString.Create }

{ TGpWideString }

{$IFDEF MSWINDOWS}
constructor TGpWideString.Create(const aValue: WideString);
begin
  inherited Create;
  sValue := aValue;
end; { TGpWideString.Create }
{$ENDIF}

{ TGpReal }

constructor TGpReal.Create(aValue: real);
begin
  inherited Create;
  rValue := aValue;
end; { TGpReal.Create }
{$IFDEF GpLists_Enumerators}

{ TGpIntegerListEnumerator }

constructor TGpIntegerListEnumerator.Create(aList: TGpIntegerList; idxFrom, idxTo, step:
  integer);
begin
  ileIndex := idxFrom - step;
  ileList := aList;
  ileIdxTo := idxTo;
  ileStep := step;
end; { TGpIntegerListEnumerator.Create }

function TGpIntegerListEnumerator.GetCurrent: integer;
begin
  Result := ileList[ileIndex];
end; { TGpIntegerListEnumerator.GetCurrent }

function TGpIntegerListEnumerator.MoveNext: boolean;
begin
  Inc(ileIndex, ileStep);
  Result := (ileIndex <= ileIdxTo);
end; { TGpIntegerListEnumerator.MoveNext }

{ TGpIntegerListWalkEnumerator }

constructor TGpIntegerListWalkEnumerator.Create(aList: TGpIntegerList; idxFrom, idxTo,
  step: integer);
begin
  inherited Create;
  ilweList := aList;
  ilweIndex := idxFrom - step;
  ilweIdxTo := idxTo;
  ilweStep := step;
  ilweList.RegisterNotification(HandleListChange);
end; { TGpIntegerListWalkEnumerator.Create }

destructor TGpIntegerListWalkEnumerator.Destroy;
begin
  ilweList.UnregisterNotification(HandleListChange);
  inherited;
end; { TGpIntegerListWalkEnumerator.Destroy }

function TGpIntegerListWalkEnumerator.GetCurrent: integer;
begin
  Result := ilweIndex;
end; { TGpIntegerListWalkEnumerator.GetCurrent }

procedure TGpIntegerListWalkEnumerator.HandleListChange(list: TObject; idxItem: integer;
  operation: TGpListOperation);
begin
  case operation of
    loInsert:
      if idxItem < ilweIndex then begin
        Inc(ilweIndex);
        Inc(ilweIdxTo);
      end
      else if idxItem <= ilweIdxTo then
        Inc(ilweIdxTo);
    loDelete:
      if idxItem < ilweIndex then begin
        Dec(ilweIndex);
        Dec(ilweIdxTo);
      end
      else if idxItem <= ilweIdxTo then
        Dec(ilweIdxTo);
    else raise Exception.Create('TGpIntegerListWalkEnumerator.HandleListChange: Unexpected list operation');
  end;
end; { TGpIntegerListWalkEnumerator.HandleListChange }

function TGpIntegerListWalkEnumerator.MoveNext: boolean;
begin
  Inc(ilweIndex, ilweStep);
  Result := (ilweIndex <= ilweIdxTo);
end; { TGpIntegerListWalkEnumerator.MoveNext }

{ TGpIntegerListSliceEnumeratorFactory }

constructor TGpIntegerListSliceEnumeratorFactory.Create(list: TGpIntegerList; idxFrom,
  idxTo, step: integer);
begin
  sefList := list;
  sefIdxFrom := idxFrom;
  sefIdxTo := idxTo;
  sefStep := step;
end; { TGpIntegerListSliceEnumeratorFactory.Create }

function TGpIntegerListSliceEnumeratorFactory.GetEnumerator: TGpIntegerListEnumerator;
begin
  Result := TGpIntegerListEnumerator.Create(sefList, sefIdxFrom, sefIdxTo, sefStep);
end; { TGpIntegerListSliceEnumeratorFactory.GetEnumerator }

{ TGpIntegerListWalkEnumeratorFactory }

constructor TGpIntegerListWalkEnumeratorFactory.Create(list: TGpIntegerList; idxFrom,
  idxTo, step: integer);
begin
  wefList := list;
  wefIdxFrom := idxFrom;
  wefIdxTo := idxTo;
  wefStep := step;
end; { TGpIntegerListWalkEnumeratorFactory.Create }

function TGpIntegerListWalkEnumeratorFactory.GetEnumerator: TGpIntegerListWalkEnumerator;
begin
  Result := TGpIntegerListWalkEnumerator.Create(wefList, wefIdxFrom, wefIdxTo, wefStep);
end; { TGpIntegerListWalkEnumeratorFactory.GetEnumerator }
{$ENDIF GpLists_Enumerators}

{ TGpIntegerList }

constructor TGpIntegerList.Create;
begin
  inherited;
  ilList := TList.Create;
  ilNotificationHandlers := TGpTMethodList.Create;
end; { TGpIntegerList.Create }

constructor TGpIntegerList.Create(const elements: array of integer);
begin
  Create;
  Assign(elements);
end; { TGpIntegerList.Create }

destructor TGpIntegerList.Destroy;
begin
  FreeAndNil(ilNotificationHandlers);
  FreeAndNil(ilList);
  inherited;
end; { TGpIntegerList.Destroy }

function TGpIntegerList.Add(item: integer): integer;
begin
  {$IFDEF GpLists_Sorting}
  if not Sorted then begin
  {$ENDIF}
    Result := ilList.Add(pointer(item));
    Notify(Result, loInsert);
  {$IFDEF GpLists_Sorting}
  end
  else begin
    if Find(item, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError : ilList.Error('List does not allow duplicates ($0%x)', item);
      end;
    InsertItem(Result, item);
  end;
  {$ENDIF}
end; { TGpIntegerList.Add }

procedure TGpIntegerList.Append(const elements: array of integer);
var
  iElement: integer;
begin
  SetCapacity(Count + Length(elements));
  for iElement := Low(elements) to High(elements) do
    Add(elements[iElement]);
end; { TGpIntegerList.Append }

procedure TGpIntegerList.Append(list: TGpIntegerList);
var
  iItem: integer;
begin
  SetCapacity(Count + list.Count);
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpIntegerList.Append }

procedure TGpIntegerList.Append(list: IGpIntegerList);
var
  iItem: integer;
begin
  SetCapacity(Count + list.Count);
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpIntegerList.Append }

function TGpIntegerList.AsDelimitedText(const delimiter: string): string;
begin
  Result := GetAsDelimitedText(delimiter, false);
end; { TGpIntegerList.AsDelimitedText }

function TGpIntegerList.AsHexText(const delimiter: string): string;
var
  hsl  : TStringList;
  iItem: integer;
begin
  hsl := TStringList.Create;
  try
    for iItem := 0 to Count-2 do
      hsl.Add(Format('%x', [Items[iItem]]));
    Result := hsl.Text;
    if delimiter <> '' then
      Result := StringReplace(Result, #13#10, delimiter, [rfReplaceAll]);
    if Count > 0 then
      Result := Result + Format('%x', [Items[Count-1]]);
  finally FreeAndNil(hsl); end;
end; { TGpIntegerList.AsHexText }

procedure TGpIntegerList.Assign(const elements: array of integer);
begin
  Clear;
  Append(elements);
end; { TGpIntegerList.Assign }

procedure TGpIntegerList.Assign(list: TGpIntegerList);
begin
  Clear;
  Append(list);
end; { TGpIntegerList.Assign }

procedure TGpIntegerList.Assign(list: IGpIntegerList);
begin
  Clear;
  Append(list);
end; { TGpIntegerList.Assign }

procedure TGpIntegerList.Clear;
begin
  ilList.Clear;
end; { TGpIntegerList.Clear }

function TGpIntegerList.Contains(item: integer; var idx: integer): boolean;
begin
  idx := IndexOf(item);
  Result := (idx >= 0);
end; { TGpIntegerList.Contains }

function TGpIntegerList.Contains(item: integer): boolean;
var
  idx: integer;
begin
  Result := Contains(item, idx);
end; { TGpIntegerList.Contains }

class function TGpIntegerList.CreateInterface: IGpIntegerList;
begin
  Result := TGpIntegerList.Create;
end; { TGpIntegerList.CreateInterface }

class function TGpIntegerList.CreateInterface(const elements: array of integer): IGpIntegerList;
begin
  Result := TGpIntegerList.Create(elements);
end; { TGpIntegerList.CreateInterface }

{$IFDEF GpLists_Sorting}
procedure TGpIntegerList.CustomSort(sortMethod: TGpIntegerListSortCompare);
begin
  if not Sorted and (Count > 1) then
    QuickSort(0, Count - 1, sortMethod);
end; { TGpIntegerList.CustomSort }
{$ENDIF}

procedure TGpIntegerList.Delete(idx: integer);
begin
  ilList.Delete(idx);
  Notify(idx, loDelete);
end; { TGpIntegerList.Delete }

{:Dumps the list into memory block starting at baseAddr.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpIntegerList.Dump(baseAddr: pointer): pointer;
var
  iList: integer;
  pList: PDWORD;
begin
  pList := baseAddr;
  pList^ := Count;
  Inc(pList);
  for iList := 0 to Count-1 do begin
    pList^ := DWORD(Items[iList]);
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerList.Dump }

function TGpIntegerList.Ensure(item: integer): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end; { TGpIntegerList.Ensure }

///<summary>Checks whether two lists contain equal elements.</summary>
///<returns>True if elements in all positions do match.</returns>
///<since>2007-02-18</since>
function TGpIntegerList.EqualTo(list: TGpIntegerList): boolean;
var
  iList: integer;
begin
  Result := Count = list.Count;
  if Result then begin
    for iList := 0 to Count - 1 do
      if Items[iList] <> list.GetItems(iList) then begin
        Result := false;
        break; //for iList
      end;
  end;
end; { TGpIntegerList.EqualTo }

procedure TGpIntegerList.Exchange(idx1, idx2: integer);
begin
  ilList.Exchange(idx1, idx2);
end; { TGpIntegerList.Exchange }

function TGpIntegerList.Find(avalue: integer; var idx: integer): boolean;
var
  L, H, I, C: integer;
begin
  Assert(ilSorted, 'Find only works on sorted lists!');
  Result := false;
  L := 0;
  H := Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := IntegerCompare(Items[I], avalue);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := true;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  idx := L;
end; { TGpIntegerList.Find }

function TGpIntegerList.First: integer;
begin
  Result := Items[0];
end; { TGpIntegerList.First }

function TGpIntegerList.GetAsDelimitedText(const delimiter: string;
  appendLastDelimiter: boolean): string;
var
  iItem   : integer;
  item    : integer;
  lenDelim: integer;
  lenItem : integer;
  p       : PChar;
  q       : PChar;
  sItem   : string;
  size    : integer;
begin
  size := 0;
  lenDelim := Length(delimiter);
  for iItem := 0 to Count-1 do begin
    item := GetItems(iItem);
    if item = 0 then
      lenItem := 1
    else if item < 0 then
      lenItem := Trunc(Log10(-item))+2
    else
      lenItem := Trunc(Log10(item))+1;
    Inc(size, lenItem);
    Inc(size, lenDelim);
  end;
  if not appendLastDelimiter then
    Dec(size, lenDelim);
  SetString(Result, nil, size);
  p := Pointer(Result);
  for iItem := 0 to Count-1 do begin
    sItem := IntToStr(GetItems(iItem));
    lenItem := Length(sItem);
    if lenItem <> 0 then begin
      System.Move(pointer(sItem)^, p^, lenItem*SizeOf(char));
      Inc(p, lenItem);
    end;
    if appendLastDelimiter or (iItem < (Count-1)) then begin
      q := Pointer(delimiter);
      while q^ <> #0 do begin
        p^ := q^;
        Inc(p);
        Inc(q);
      end; //while
    end;
  end;
end; { TGpIntegerList.GetAsDelimitedText }

function TGpIntegerList.GetCapacity: integer;
begin
  Result := ilList.Capacity;
end; { TGpIntegerList.GetCapacity }

function TGpIntegerList.GetCount: integer;
begin
  Result := ilList.Count;
end; { TGpIntegerList.GetCount }

function TGpIntegerList.GetDuplicates: TDuplicates;
begin
  Result := ilDuplicates;
end; { TGpIntegerList.GetDuplicates }

{$IFDEF GpLists_Enumerators}
function TGpIntegerList.GetEnumerator: TGpIntegerListEnumerator;
begin
  Result := TGpIntegerListEnumerator.Create(Self, 0, Count - 1, 1);
end; { TGpIntegerList.GetEnumerator }
{$ENDIF GpLists_Enumerators}

function TGpIntegerList.GetItems(idx: integer): integer;
begin
  Result := integer(ilList.Items[idx]);
end; { TGpIntegerList.GetItems }

function TGpIntegerList.GetSorted: boolean;
begin
  Result := ilSorted;
end; { TGpIntegerList.GetSorted }

function TGpIntegerList.GetText: string;
begin
  Result := GetAsDelimitedText(#13#10, true);
end; { TGpIntegerList.GetText }

function TGpIntegerList.IndexOf(item: integer): integer;
begin
  {$IFDEF GpLists_Sorting}
  if Sorted then begin
    if not Find(item, Result) then
      Result := -1
  end
  else
  {$ENDIF}
    Result := ilList.IndexOf(pointer(item));
end; { TGpIntegerList.IndexOf }

procedure TGpIntegerList.Insert(idx, item: integer);
begin
  {$IFDEF GpLists_Sorting}
  if Sorted then
    raise Exception.Create('Cannot insert element in sorted list.');
  {$ENDIF}
  InsertItem(idx, item);
end; { TGpIntegerList.Insert }

procedure TGpIntegerList.InsertItem(idx, item: integer);
begin
  ilList.Insert(idx, pointer(item));
  Notify(idx, loInsert);
end; { TGpIntegerList.InsertItem }

function TGpIntegerList.Last: integer;
begin
  Result := Items[Count-1];
end; { TGpIntegerList.Last }

function TGpIntegerList.LoadFromStream(stream: TStream): boolean;
var
  item: integer;
  read: integer;
begin
  Result := false;
  Clear;
  repeat
    read := stream.Read(item, 4);
    if read = 4 then
      Add(item)
    else if read > 0 then
      Exit;
  until read = 0;
  Result := true;
end; { TGpIntegerList.LoadFromStream }

procedure TGpIntegerList.Move(curIdx, newIdx: integer);
begin
  {$IFDEF GpLists_Sorting}
  if Sorted then
    raise Exception.Create('Cannot move elements in sorted list.');
  {$ENDIF}
  ilList.Move(curIdx, newIdx);
end; { TGpIntegerList.Move }

procedure TGpIntegerList.Notify(idxItem: integer; operation: TGpListOperation);
var
  iHandler: integer;
begin
  if ilNotificationHandlers.Count = 0 then
    Exit;
  for iHandler := 0 to ilNotificationHandlers.Count - 1 do
    TGpListNotificationEvent(ilNotificationHandlers[iHandler])(Self, idxItem, operation);
end; { TGpIntegerList.Notify }

procedure TGpIntegerList.QuickSort(L, R: integer; SCompare: TGpIntegerListSortCompare);
var
  I, J, P: integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while (SCompare(Self, I, P) < 0) do
        Inc(I);
      while (SCompare(Self, J, P) > 0) do
        Dec(J);
      if (I <= J) then begin
        Exchange(I, J);
        if (P = I) then
          P := J
        else if (P = J) then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until (I > J);
    if (L < J) then
      QuickSort(L, J, SCompare);
    L := I;
  until (I >= R);
end; { TGpIntegerList.QuickSort }

procedure TGpIntegerList.RegisterNotification(notificationHandler:
  TGpListNotificationEvent);
begin
  ilNotificationHandlers.Add(TMethod(notificationHandler));
end; { TGpIntegerList.RegisterNotification }

procedure TGpIntegerList.Remove(item: integer);
var
  idxItem: integer;
begin
  idxItem := IndexOf(item);
  if idxItem >= 0 then
    Delete(idxItem);
end; { TGpIntegerList.Remove }

{:Restores the list dumped by the Dump method.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpIntegerList.Restore(baseAddr: pointer): pointer;
var
  iList   : integer;
  numItems: integer;
  pList   : PDWORD;
begin
  pList := baseAddr;
  numItems := integer(pList^);
  Inc(pList);
  ilList.Capacity := numItems;
  for iList := 0 to numItems-1 do begin
    Add(integer(pList^));
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerList.Restore }

procedure TGpIntegerList.SaveToStream(stream: TStream);
var
  iItem: integer;
  item : integer;
begin
  for iItem := 0 to Count-1 do begin
    item := Items[iItem];
    stream.WriteBuffer(item, 4);
  end;
end; { TGpIntegerList.SaveToStream }

procedure TGpIntegerList.SetCapacity(const value: integer);
begin
  ilList.Capacity := value;
end; { TGpIntegerList.SetCapacity }

procedure TGpIntegerList.SetCount(const value: integer);
begin
  ilList.Count := value;
end; { TGpIntegerList.SetCount }

procedure TGpIntegerList.SetDuplicates(const value: TDuplicates);
begin
  ilDuplicates := value;
end; { TGpIntegerList.SetDuplicates }

procedure TGpIntegerList.SetItems(idx: integer; const value: integer);
begin
  ilList.Items[idx] := pointer(value);
end; { TGpIntegerList.SetItems }

{$IFDEF GpLists_Sorting}
procedure TGpIntegerList.SetSorted(const value: boolean);
begin
  if ilSorted <> value then begin
    if value then
      CustomSort(IntegerListCompare);
    ilSorted := value;
  end;
end; { TGpIntegerList.SetSorted }
{$ENDIF}

procedure TGpIntegerList.SetText(const value: string);
var
  p    : PChar;
  s    : string;
  start: PChar;
begin
  Clear;
  p := pointer(value);
  if P <> nil then
    while p^ <> #0 do begin
      start := p;
      while (p^ <> #0) and (p^ <> #10) and (p^ <> #13) do
        Inc(p);
      SetString(s, start, p - start);
      Add(StrToInt(s));
      if p^ = #13 then Inc(p);
      if p^ = #10 then Inc(p);
    end;
end; { TGpIntegerList.SetText }

{$IFDEF GpLists_Enumerators}
function TGpIntegerList.Slice(idxFrom, idxTo, step: integer): TGpIntegerListSliceEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpIntegerListSliceEnumeratorFactory.Create(Self, idxFrom, idxTo, step);
end; { TGpIntegerList.Slice }
{$ENDIF GpLists_Enumerators}

{$IFDEF GpLists_Sorting}
procedure TGpIntegerList.Sort;
begin
  Sorted := false;
  Sorted := true;
end; { TGpIntegerList.Sort }
{$ENDIF}

procedure TGpIntegerList.UnregisterNotification(notificationHandler:
  TGpListNotificationEvent);
begin
  ilNotificationHandlers.Remove(TMethod(notificationHandler));
end; { TGpIntegerList.UnregisterNotification }

{$IFDEF GpLists_Enumerators}
function TGpIntegerList.Walk(idxFrom, idxTo, step: integer): TGpIntegerListWalkEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpIntegerListWalkEnumeratorFactory.Create(Self, idxFrom, idxTo, step);
end; { TGpIntegerList.Walk }
{$ENDIF GpLists_Enumerators}

{$IFDEF GpLists_Enumerators}

{ TGpInt64ListEnumerator }

constructor TGpInt64ListEnumerator.Create(aList: TGpInt64List; idxFrom, idxTo, step:
  integer);
begin
  ileIndex := idxFrom - step;
  ileList := aList;
  ileIdxTo := idxTo;
  ileStep := step;
end; { TGpInt64ListEnumerator.Create }

function TGpInt64ListEnumerator.GetCurrent: int64;
begin
  Result := ileList[ileIndex];
end; { TGpInt64ListEnumerator.GetCurrent }

function TGpInt64ListEnumerator.MoveNext: boolean;
begin
  Inc(ileIndex, ileStep);
  Result := (ileIndex <= ileIdxTo);
end; { TGpInt64ListEnumerator.MoveNext }

{ TGpInt64ListWalkEnumerator }

constructor TGpInt64ListWalkEnumerator.Create(aList: TGpInt64List; idxFrom, idxTo, step:
  integer);
begin
  inherited Create;
  ilweList := aList;
  ilweIndex := idxFrom - step;
  ilweIdxTo := idxTo;
  ilweStep := step;
  ilweList.RegisterNotification(HandleListChange);
end; { TGpInt64ListWalkEnumerator.Create }

destructor TGpInt64ListWalkEnumerator.Destroy;
begin
  ilweList.UnregisterNotification(HandleListChange);
  inherited;
end; { TGpInt64ListWalkEnumerator.Destroy }

function TGpInt64ListWalkEnumerator.GetCurrent: integer;
begin
  Result := ilweIndex;
end; { TGpInt64ListWalkEnumerator.GetCurrent }

procedure TGpInt64ListWalkEnumerator.HandleListChange(list: TObject; idxItem: integer;
  operation: TGpListOperation);
begin
  case operation of
    loInsert:
      if idxItem < ilweIndex then begin
        Inc(ilweIndex);
        Inc(ilweIdxTo);
      end
      else if idxItem <= ilweIdxTo then
        Inc(ilweIdxTo);
    loDelete:
      if idxItem < ilweIndex then begin
        Dec(ilweIndex);
        Dec(ilweIdxTo);
      end
      else if idxItem <= ilweIdxTo then
        Dec(ilweIdxTo);
    else raise Exception.Create('TGpInt64ListWalkEnumerator.HandleListChange: Unexpected list operation');
  end;
end; { TGpInt64ListWalkEnumerator.HandleListChange }

function TGpInt64ListWalkEnumerator.MoveNext: boolean;
begin
  Inc(ilweIndex, ilweStep);
  Result := (ilweIndex <= ilweIdxTo);
end; { TGpInt64ListWalkEnumerator.MoveNext }

{ TGpInt64ListSliceEnumeratorFactory }

constructor TGpInt64ListSliceEnumeratorFactory.Create(list: TGpInt64List; idxFrom,
  idxTo, step: integer);
begin
  sefList := list;
  sefIdxFrom := idxFrom;
  sefIdxTo := idxTo;
  sefStep := step;
end; { TGpInt64ListSliceEnumeratorFactory.Create }

function TGpInt64ListSliceEnumeratorFactory.GetEnumerator: TGpInt64ListEnumerator;
begin
  Result := TGpInt64ListEnumerator.Create(sefList, sefIdxFrom, sefIdxTo, sefStep);
end; { TGpInt64ListSliceEnumeratorFactory.GetEnumerator }

{ TGpInt64ListWalkEnumeratorFactory }

constructor TGpInt64ListWalkEnumeratorFactory.Create(list: TGpInt64List; idxFrom, idxTo,
  step: integer);
begin
  wefList := list;
  wefIdxFrom := idxFrom;
  wefIdxTo := idxTo;
  wefStep := step;
end; { TGpInt64ListWalkEnumeratorFactory.Create }

function TGpInt64ListWalkEnumeratorFactory.GetEnumerator: TGpInt64ListWalkEnumerator;
begin
  Result := TGpInt64ListWalkEnumerator.Create(wefList, wefIdxFrom, wefIdxTo, wefStep);
end; { TGpInt64ListWalkEnumeratorFactory.GetEnumerator }
{$ENDIF GpLists_Enumerators}

{ TGpInt64List }

type
  PInteger64 = ^Int64; // Workaround for Delphi 6 "Internal error: URW699" below

constructor TGpInt64List.Create;
begin
  inherited;
  ilList := TList.Create;
  ilNotificationHandlers := TGpTMethodList.Create;
end; { TGpInt64List.Create }

constructor TGpInt64List.Create(const elements: array of int64);
begin
  Create;
  Assign(elements);
end; { TGpInt64List.Create }

destructor TGpInt64List.Destroy;
begin
  FreeAndNil(ilNotificationHandlers);
  FreeAndNil(ilList);
  inherited;
end; { TGpInt64List.Destroy }

function TGpInt64List.Add(item: int64): integer;
begin
  if not Sorted then begin
    ilList.Add(pointer(Int64Rec(item).Lo));
    Result := ilList.Add(pointer(Int64Rec(item).Hi)) div 2;
    Notify(Result, loInsert);
  end
  else begin
    if Find(item, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError : raise EListError.CreateFmt('List does not allow duplicates ($0%x)', [item]);
      end;
    InsertItem(Result, item);
  end;
end; { TGpInt64List.Add }

procedure TGpInt64List.Append(const elements: array of int64);
var
  iElement: integer;
begin
  SetCapacity(Count + Length(elements));
  for iElement := Low(elements) to High(elements) do
    Add(elements[iElement]);
end; { TGpInt64List.Append }

procedure TGpInt64List.Append(list: IGpIntegerList);
var
  iItem: integer;
begin
  SetCapacity(Count + list.Count);
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpInt64List.Append }

procedure TGpInt64List.Append(list: TGpIntegerList);
var
  iItem: integer;
begin
  SetCapacity(Count + list.Count);
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpInt64List.Append }

procedure TGpInt64List.Append(list: IGpInt64List);
var
  iItem: integer;
begin
  SetCapacity(Count + list.Count);
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpInt64List.Append }

procedure TGpInt64List.Append(list: TGpInt64List);
var
  iItem: integer;
begin
  SetCapacity(Count + list.Count);
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpInt64List.Append }

function TGpInt64List.AsDelimitedText(const delimiter: string): string;
begin
  Result := GetAsDelimitedText(delimiter, false);
end; { TGpInt64List.AsDelimitedText }

function TGpInt64List.AsHexText(const delimiter: string): string;
var
  hsl  : TStringList;
  iItem: integer;
begin
  hsl := TStringList.Create;
  try
    for iItem := 0 to Count-2 do
      hsl.Add(Format('%x', [Items[iItem]]));
    Result := hsl.Text;
    if delimiter <> '' then
      Result := StringReplace(Result, #13#10, delimiter, [rfReplaceAll]);
    if Count > 0 then
      Result := Result + Format('%x', [Items[Count-1]]);
  finally FreeAndNil(hsl); end;
end; { TGpInt64List.AsHexText }

procedure TGpInt64List.Assign(const elements: array of int64);
begin
  Clear;
  Append(elements);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Assign(list: TGpInt64List);
begin
  Clear;
  Append(list);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Assign(list: IGpInt64List);
begin
  Clear;
  Append(list);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Assign(list: TGpIntegerList);
begin
  Clear;
  Append(list);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Assign(list: IGpIntegerList);
begin
  Clear;
  Append(list);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Clear;
begin
  ilList.Clear;
end; { TGpInt64List.Clear }

function TGpInt64List.Contains(item: int64; var idx: integer): boolean;
begin
  idx := IndexOf(item);
  Result := (idx >= 0);
end; { TGpInt64List.Contains }

function TGpInt64List.Contains(item: int64): boolean;
var
  idx: integer;
begin
  Result := Contains(item, idx);
end; { TGpInt64List.Contains }

class function TGpInt64List.CreateInterface: IGpInt64List;
begin
  Result := TGpInt64List.Create;
end; { TGpInt64List.CreateInterface }

class function TGpInt64List.CreateInterface(const elements: array of int64): TGpInt64List;
begin
  Result := TGpInt64List.Create(elements);
end; { TGpInt64List.CreateInterface }

procedure TGpInt64List.CustomSort(sortMethod: TGpInt64ListSortCompare);
begin
  if not Sorted and (Count > 1) then
    QuickSort(0, Count - 1, sortMethod);
end; { TGpInt64List.CustomSort }

procedure TGpInt64List.Delete(idx: integer);
begin
  ilList.Delete(2*idx);
  ilList.Delete(2*idx);
  Notify(idx, loDelete);
end; { TGpInt64List.Delete }

{:Dumps the list into memory block starting at baseAddr.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2006-09-20
}
function TGpInt64List.Dump(baseAddr: pointer): pointer;
var
  iList: integer;
  pList: PInteger64;
begin
  pList := baseAddr;
  pList^ := Count;
  Inc(pList);
  for iList := 0 to Count-1 do begin
    pList^ := Items[iList];
    Inc(pList);
  end;
  Result := pList;
end; { TGpInt64List.Dump }

function TGpInt64List.Ensure(item: int64): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end; { TGpInt64List.Ensure }

procedure TGpInt64List.Exchange(idx1, idx2: integer);
begin
  ilList.Exchange(2*idx1, 2*idx2);
  ilList.Exchange(2*idx1+1, 2*idx2+1);
end; { TGpInt64List.Exchange }

function TGpInt64List.Find(avalue: int64; var idx: integer): boolean;
var
  L, H, I, C: integer;
begin
  Assert(ilSorted, 'Find only works on sorted lists!');
  Result := false;
  L := 0;
  H := Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := Int64Compare(Items[I], avalue);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := true;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  idx := L;
end; { TGpInt64List.Find }

function TGpInt64List.First: int64;
begin
  Result := Items[0];
end; { TGpInt64List.First }

function TGpInt64List.GetAsDelimitedText(const delimiter: string;
  appendLastDelimiter: boolean): string;
var
  iItem   : integer;
  item    : int64;
  lenDelim: integer;
  lenItem : integer;
  p       : PChar;
  q       : PChar;
  sItem   : string;
  size    : integer;
begin
  size := 0;
  lenDelim := Length(delimiter);
  for iItem := 0 to Count-1 do begin
    item := GetItems(iItem);
    if item = 0 then
      lenItem := 1
    else if item < 0 then
      lenItem := Trunc(Log10(-item))+2
    else
      lenItem := Trunc(Log10(item))+1;
    Inc(size, lenItem);
    Inc(size, lenDelim);
  end;
  if not appendLastDelimiter then
    Dec(size, lenDelim);
  SetString(Result, nil, size);
  p := Pointer(Result);
  for iItem := 0 to Count-1 do begin
    sItem := IntToStr(GetItems(iItem));
    lenItem := Length(sItem);
    if lenItem <> 0 then begin
      System.Move(pointer(sItem)^, p^, lenItem*SizeOf(char));
      Inc(p, lenItem);
    end;
    if appendLastDelimiter or (iItem < (Count-1)) then begin
      q := Pointer(delimiter);
      while q^ <> #0 do begin
        p^ := q^;
        Inc(p);
        Inc(q);
      end; //while
    end;
  end;
end; { TGpInt64List.GetAsDelimitedText }

function TGpInt64List.GetCapacity: integer;
begin
  Result := ilList.Capacity div 2;
end; { TGpInt64List.GetCapacity }

function TGpInt64List.GetCount: integer;
begin
  Result := ilList.Count div 2;
end; { TGpInt64List.GetCount }

function TGpInt64List.GetDuplicates: TDuplicates;
begin
  Result := ilDuplicates;
end; { TGpInt64List.GetDuplicates }

{$IFDEF GpLists_Enumerators}
function TGpInt64List.GetEnumerator: TGpInt64ListEnumerator;
begin
  Result := TGpInt64ListEnumerator.Create(Self, 0, Count - 1, 1);
end; { TGpInt64List.GetEnumerator }
{$ENDIF GpLists_Enumerators}

function TGpInt64List.GetItems(idx: integer): int64;
begin
  Int64Rec(Result).Lo := cardinal(ilList.Items[2*idx]);
  Int64Rec(Result).Hi := cardinal(ilList.Items[2*idx+1]);
end; { TGpInt64List.GetItems }

function TGpInt64List.GetSorted: boolean;
begin
  Result := ilSorted;
end; { TGpInt64List.GetSorted }

function TGpInt64List.GetText: string;
begin
  Result := GetAsDelimitedText(#13#10, true);
end; { TGpInt64List.GetText }

function TGpInt64List.IndexOf(item: int64): integer;
begin
  if Sorted then begin
    if not Find(item, Result) then
      Result := -1
  end
  else begin
    Result := 0;
    while Result < ilList.Count do begin
      if (pointer(Int64Rec(item).Lo) = ilList[Result]) and
         (pointer(Int64Rec(item).Hi) = ilList[Result+1]) then
      begin
        Result := Result div 2;
        Exit;
      end;
      Inc(Result, 2);
    end;
    Result := -1;
  end;
end; { TGpInt64List.IndexOf }

procedure TGpInt64List.Insert(idx: integer; item: int64);
begin
  if Sorted then
    raise Exception.Create('Cannot insert element in sorted list.');
  InsertItem(idx, item);
end; { TGpInt64List.Insert }

procedure TGpInt64List.InsertItem(idx: integer; item: int64);
begin
  ilList.Insert(2*idx, pointer(Int64Rec(item).Hi));
  ilList.Insert(2*idx, pointer(Int64Rec(item).Lo));
  Notify(idx, loInsert);
end; { TGpInt64List.InsertItem }

function TGpInt64List.Last: int64;
begin
  Result := Items[Count-1];
end; { TGpInt64List.Last }

function TGpInt64List.LoadFromStream(stream: TStream): boolean;
var
  item: int64;
  read: integer;
begin
  Result := false;
  Clear;
  repeat
    read := stream.Read(item, 8);
    if read = 8 then
      Add(item)
    else if read > 0 then
      Exit;
  until read = 0;
  Result := true;
end; { TGpInt64List.LoadFromStream }

procedure TGpInt64List.Move(curIdx, newIdx: integer);
begin
  if Sorted then
    raise Exception.Create('Cannot move elements in sorted list.');
  ilList.Move(2*curIdx, 2*newIdx);
  ilList.Move(2*curIdx+1, 2*newIdx+1);
end; { TGpInt64List.Move }

procedure TGpInt64List.Notify(idxItem: integer; operation: TGpListOperation);
var
  iHandler: integer;
begin
  if ilNotificationHandlers.Count = 0 then
    Exit;
  for iHandler := 0 to ilNotificationHandlers.Count - 1 do
    TGpListNotificationEvent(ilNotificationHandlers[iHandler])(Self, idxItem, operation);
end; { TGpInt64List.Notify }

procedure TGpInt64List.QuickSort(L, R: integer; SCompare: TGpInt64ListSortCompare);
var
  I, J, P: integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while (SCompare(Self, I, P) < 0) do
        Inc(I);
      while (SCompare(Self, J, P) > 0) do
        Dec(J);
      if (I <= J) then begin
        Exchange(I, J);
        if (P = I) then
          P := J
        else if (P = J) then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until (I > J);
    if (L < J) then
      QuickSort(L, J, SCompare);
    L := I;
  until (I >= R);
end; { TGpInt64List.QuickSort }

procedure TGpInt64List.RegisterNotification(notificationHandler:
  TGpListNotificationEvent);
begin
  ilNotificationHandlers.Add(TMethod(notificationHandler));
end; { TGpIntegerList.RegisterNotification }

procedure TGpInt64List.Remove(item: int64);
var
  idxItem: integer;
begin
  idxItem := IndexOf(item);
  if idxItem >= 0 then
    Delete(idxItem);
end; { TGpInt64List.Remove }

{:Restores the list dumped by the Dump method.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2006-09-20
}
function TGpInt64List.Restore(baseAddr: pointer): pointer;
var
  iList   : integer;
  numItems: integer;
  pList   : PInteger64;
begin
  pList := baseAddr;
  numItems := integer(pList^);
  Inc(pList);
  ilList.Capacity := numItems;
  for iList := 0 to numItems-1 do begin
    Add(int64(pList^));
    Inc(pList);
  end;
  Result := pList;
end; { TGpInt64List.Restore }

procedure TGpInt64List.SaveToStream(stream: TStream);
var
  iItem: integer;
  item : int64;
begin
  for iItem := 0 to Count-1 do begin
    item := Items[iItem];
    stream.WriteBuffer(item, 8);
  end;
end; { TGpInt64List.SaveToStream }

procedure TGpInt64List.SetCapacity(const value: integer);
begin
  ilList.Capacity := 2*value;
end; { TGpInt64List.SetCapacity }

procedure TGpInt64List.SetCount(const value: integer);
begin
  ilList.Count := 2*value;
end; { TGpInt64List.SetCount }

procedure TGpInt64List.SetDuplicates(const value: TDuplicates);
begin
  ilDuplicates := value;
end; { TGpInt64List.SetDuplicates }

procedure TGpInt64List.SetItems(idx: integer; value: int64);
begin
  ilList.Items[2*idx] := pointer(Int64Rec(value).Lo);
  ilList.Items[2*idx+1] := pointer(Int64Rec(value).Hi);
end; { TGpInt64List.SetItems }

procedure TGpInt64List.SetSorted(const value: boolean);
begin
  if (ilSorted <> value) then begin
    if value then
      CustomSort(Int64ListCompare);
    ilSorted := value;
  end;
end; { TGpInt64List.SetSorted }

procedure TGpInt64List.SetText(const value: string);
var
  p    : PChar;
  s    : string;
  start: PChar;
begin
  Clear;
  p := pointer(value);
  if P <> nil then
    while p^ <> #0 do begin
      start := p;
      while (p^ <> #0) and (p^ <> #10) and (p^ <> #13) do
        Inc(p);
      SetString(s, start, p - start);
      Add(StrToInt64(s));
      if p^ = #13 then Inc(p);
      if p^ = #10 then Inc(p);
    end;
end; { TGpInt64List.SetText }

{$IFDEF GpLists_Enumerators}
function TGpInt64List.Slice(idxFrom, idxTo, step: integer): TGpInt64ListSliceEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpInt64ListSliceEnumeratorFactory.Create(Self, idxFrom, idxTo, step);
end; { TGpInt64List.Slice }
{$ENDIF GpLists_Enumerators}

procedure TGpInt64List.Sort;
begin
  Sorted := false;
  Sorted := true;
end; { TGpInt64List.Sort }

procedure TGpInt64List.UnregisterNotification(notificationHandler:
  TGpListNotificationEvent);
begin
  ilNotificationHandlers.Remove(TMethod(notificationHandler));
end; { TGpIntegerList.UnregisterNotification }

{$IFDEF GpLists_Enumerators}
function TGpInt64List.Walk(idxFrom, idxTo, step: integer): TGpInt64ListWalkEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpInt64ListWalkEnumeratorFactory.Create(Self, idxFrom, idxTo, step);
end; { TGpInt64List.Walk }
{$ENDIF GpLists_Enumerators}

{$IFDEF GpLists_Enumerators}

{ TGpIntegerObjectListWalkKVEnumerator }

constructor TGpIntegerObjectListWalkKVEnumerator.Create(aList: TGpIntegerObjectList;
  idxFrom, idxTo, step: integer);
begin
  inherited Create;
  wkeListEnumerator := TGpIntegerListWalkEnumerator.Create(aList, idxFrom, idxTo, step);
  wkeCurrentKV := TGpKeyValue.Create;
end; { TGpIntegerObjectListWalkKVEnumerator.Create }

destructor TGpIntegerObjectListWalkKVEnumerator.Destroy;
begin
  FreeAndNil(wkeCurrentKV);
  FreeAndNil(wkeListEnumerator);
  inherited;
end; { TGpIntegerObjectListWalkKVEnumerator.Destroy }

function TGpIntegerObjectListWalkKVEnumerator.GetCurrent: TGpKeyValue;
var
  idx: integer;
begin
  idx := wkeListEnumerator.GetCurrent;
  wkeCurrentKV.Key := wkeListEnumerator.List[idx];
  wkeCurrentKV.Value := TGpIntegerObjectList(wkeListEnumerator.List).Objects[idx];
  Result := wkeCurrentKV;
end; { TGpIntegerObjectListWalkKVEnumerator.GetCurrent }

function TGpIntegerObjectListWalkKVEnumerator.MoveNext: boolean;
begin
  Result := wkeListEnumerator.MoveNext;
end; { TGpIntegerObjectListWalkKVEnumerator.MoveNext }

{ TGpIntegerObjectListWalkKVEnumeratorFactory }

constructor TGpIntegerObjectListWalkKVEnumeratorFactory.Create(list:
  TGpIntegerObjectList; idxFrom, idxTo, step: integer);
begin
  wkefList := list;
  wkefIdxFrom := idxFrom;
  wkefIdxTo := idxTo;
  wkefStep := step;
end; { TGpIntegerObjectListWalkKVEnumeratorFactory.Create }

function TGpIntegerObjectListWalkKVEnumeratorFactory.GetEnumerator:
  TGpIntegerObjectListWalkKVEnumerator;
begin
  Result := TGpIntegerObjectListWalkKVEnumerator.Create(wkefList, wkefIdxFrom, wkefIdxTo, wkefStep);
end; { TGpIntegerObjectListWalkKVEnumeratorFactory.GetEnumerator }

{$ENDIF GpLists_Enumerators}

{ TGpIntegerObjectList }

constructor TGpIntegerObjectList.Create(ownsObjects: boolean);
begin
  inherited Create;
  iolObjects := TObjectList.Create(ownsObjects);
end; { TGpIntegerObjectList.Create }

destructor TGpIntegerObjectList.Destroy;
begin
  FreeAndNil(iolObjects);
  inherited;
end; { TGpIntegerObjectList.Destroy }

function TGpIntegerObjectList.Add(item: integer): integer;
begin
  Result := AddObject(item, nil); 
end; { TGpIntegerObjectList.Add }

function TGpIntegerObjectList.AddObject(item: integer; obj: TObject): integer;
begin
  {$IFDEF GpLists_Sorting}
  if Sorted and (Duplicates = dupIgnore) then begin
    Result := IndexOf(item);
    if Result >= 0 then begin
      Objects[Result] := obj;
      Exit;
    end;
  end;
  {$ENDIF}
  Result := inherited Add(item);
  iolObjects.Insert(Result, obj);
  Assert(Count = iolObjects.Count,
    'TGpIntegerObjectList.AddObject: Number of items and objects differ');
end; { TGpIntegerObjectList.AddObject }                    

procedure TGpIntegerObjectList.Clear;
begin
  inherited;
  iolObjects.Clear;
end; { TGpIntegerObjectList.Clear }

class function TGpIntegerObjectList.CreateInterface(ownsObjects: boolean):
  IGpIntegerObjectList;
begin
  Result := TGpIntegerObjectList.Create(ownsObjects);
end; { TGpIntegerObjectList.CreateInterface }

procedure TGpIntegerObjectList.Delete(idx: integer);
begin
  inherited;
  iolObjects.Delete(idx);
  Assert(Count = iolObjects.Count,
    'TGpIntegerObjectList.Delete: Number of items and objects differ');
end; { TGpIntegerObjectList.Delete }

{:Dumps the list into memory block starting at baseAddr.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpIntegerObjectList.Dump(baseAddr: pointer): pointer;
var
  iList: integer;
  pList: PDWORD;
begin
  pList := baseAddr;
  pList^ := Count;
  Inc(pList);
  for iList := 0 to Count-1 do begin
    pList^ := DWORD(Items[iList]);
    Inc(pList);
    pList^ := DWORD(Objects[iList]);
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerObjectList.Dump }

function TGpIntegerObjectList.EnsureObject(item: integer; obj: TObject): integer;
begin
  Result := inherited Ensure(item);
  Objects[Result] := obj;
end; { TGpIntegerObjectList.EnsureObject }

function TGpIntegerObjectList.EnsureObject(item: integer; objClass: TClass): integer;
begin
  Result := inherited Ensure(item);
  if not assigned(Objects[Result]) then
    Objects[Result] := objClass.Create;
end; { TGpIntegerObjectList.EnsureObject }

procedure TGpIntegerObjectList.Exchange(idx1, idx2: integer);
begin
  inherited;
  iolObjects.Exchange(idx1, idx2);
end; { TGpIntegerObjectList.Exchange }

function TGpIntegerObjectList.ExtractObject(idxObject: integer): TObject;
begin
  Result := Objects[idxObject];
  iolObjects.Extract(iolObjects[idxObject]);
  inherited Delete(idxObject);
end; { TGpIntegerObjectList.ExtractObject }

function TGpIntegerObjectList.FetchObject(item: integer): TObject;
var
  idxItem: integer;
begin
  idxItem := IndexOf(item);
  if idxItem >= 0 then
    Result := Objects[idxItem]
  else
    Result := nil;
end; { TGpIntegerObjectList.FetchObject }

procedure TGpIntegerObjectList.FreeObjects;
var
  iObject: integer;
begin
  for iObject := 0 to Count - 1 do begin
    Objects[iObject].Free;
    Objects[iObject] := nil;
  end;
end; { TGpIntegerObjectList.FreeObjects }

function TGpIntegerObjectList.GetObject(idxObject: integer): TObject;
begin
  Result := iolObjects[idxObject];
end; { TGpIntegerObjectList.GetObject }

procedure TGpIntegerObjectList.Insert(idx, item: integer);
begin
  InsertObject(idx, item, nil);
end; { TGpIntegerObjectList.Insert }

procedure TGpIntegerObjectList.InsertObject(idx, item: integer; obj: TObject);
begin
  inherited Insert(idx, item);
  iolObjects.Insert(idx, obj);
  Assert(Count = iolObjects.Count,
    'TGpIntegerObjectList.InsertObject: Number of items and objects differ');
end; { TGpIntegerObjectList.InsertObject }

function TGpIntegerObjectList.LoadFromStream(stream: TStream): boolean;
var
  item: integer;
  obj : TObject;
  read: integer;
begin
  Result := false;
  Clear;
  repeat
    read := stream.Read(item, 4);
    if read = 0 then
      break; //repeat
    if read <> 4 then
      Exit;
    read := stream.Read(obj, 4);
    if read <> 4 then
      Exit;
    AddObject(item, obj)
  until read = 0;
  Result := true;
end; { TGpIntegerObjectList.LoadFromStream }

procedure TGpIntegerObjectList.Move(curIdx, newIdx: integer);
begin
  inherited;
  iolObjects.Move(curIdx, newIdx);
end; { TGpIntegerObjectList.Move }

{:Restores the list dumped by the Dump method.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpIntegerObjectList.Restore(baseAddr: pointer): pointer;
var
  iList   : integer;
  item    : integer;
  numItems: integer;
  pList   : PDWORD;
begin
  pList := baseAddr;
  numItems := integer(pList^);
  Inc(pList);
  ilList.Capacity := numItems;
  iolObjects.Capacity := numItems;
  for iList := 0 to numItems-1 do begin
    item := integer(pList^);
    Inc(pList);
    AddObject(item, TObject(pList^));
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerObjectList.Restore }

procedure TGpIntegerObjectList.SaveToStream(stream: TStream);
var
  iItem: integer;
  item : integer;
  obj  : TObject;
begin
  for iItem := 0 to Count-1 do begin
    item := Items[iItem];
    stream.WriteBuffer(item, 4);
    obj := Objects[iItem];
    stream.WriteBuffer(obj, 4);
  end;
end; { TGpIntegerObjectList.SaveToStream }

procedure TGpIntegerObjectList.SetObject(idxObject: integer; const value: TObject);
begin
  iolObjects[idxObject] := value;
end; { TGpIntegerObjectList.SetObject }

{$IFDEF GpLists_Enumerators}
function TGpIntegerObjectList.WalkKV(idxFrom, idxTo, step: integer):
  TGpIntegerObjectListWalkKVEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpIntegerObjectListWalkKVEnumeratorFactory.Create(Self, idxFrom, idxTo, step);
end; { TGpIntegerObjectList.WalkKV }
{$ENDIF GpLists_Enumerators}

{$IFDEF Unicode}
{$IFNDEF GpLists_LimitedGenerics}
{ TGpIntegerObjectList<T> }

class function TGpIntegerObjectList<T>.CreateInterface(ownsObjects: boolean = true):
  IGpIntegerObjectList<T>;
begin
  Result := TGpIntegerObjectList<T>.Create(ownsObjects);
end; { TGpIntegerObjectList<T>.CreateInterface }

function TGpIntegerObjectList<T>.AddObject(item: integer; obj: T): integer;
begin
  Result := inherited AddObject(item, obj);
end; { TGpIntegerObjectList<T>.AddObject }

function TGpIntegerObjectList<T>.EnsureObject(item: integer; obj: T): integer;
begin
  Result := inherited EnsureObject(item, TObject(PPointer(@obj)^));
end; { TGpIntegerObjectList<T>.EnsureObject }

function TGpIntegerObjectList<T>.EnsureObject(item: integer): integer;
begin
  Result := inherited EnsureObject(item, TClass(T));
end; { TGpIntegerObjectList<T>.EnsureObject }

function TGpIntegerObjectList<T>.ExtractObject(idxObject: integer): T;
begin
  Result := T(inherited ExtractObject(idxObject));
end; { TGpIntegerObjectList<T>.ExtractObject }

function TGpIntegerObjectList<T>.FetchObject(item: integer): T;
begin
  Result := T(inherited FetchObject(item));
end; { TGpIntegerObjectList<T>.FetchObject }

function TGpIntegerObjectList<T>.GetObject(idxObject: integer): T;
begin
  Result := T(inherited GetObject(idxObject));
end; { TGpIntegerObjectList<T>.GetObject }

procedure TGpIntegerObjectList<T>.InsertObject(idx, item: integer; obj: T);
begin
  inherited InsertObject(idx, item, obj);
end; { TGpIntegerObjectList<T>.InsertObject }

procedure TGpIntegerObjectList<T>.SetObject(idxObject: integer; const value: T);
begin
  inherited SetObject(idxObject, value);
end; { TGpIntegerObjectList<T>.SetObject }
{$ENDIF GpLists_LimitedGenerics}
{$ENDIF Unicode}

{ TGpCountedIntegerList }

function CompareAscending_CIL(list: TGpIntegerList; index1, index2: integer): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}
var
  item1: integer;
  item2: integer;
begin
  item1 := TGpCountedIntegerList(list).Counter[index1];
  item2 := TGpCountedIntegerList(list).Counter[index2];
  if item1 < item2 then
    Result := -1
  else if item1 > item2 then
    Result := 1
  else
    Result := 0;
end; { CompareAscending_CIL }

function CompareDescending_CIL(list: TGpIntegerList; index1, index2: integer): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}
var
  item1: integer;
  item2: integer;
begin
  item1 := TGpCountedIntegerList(list).Counter[index1];
  item2 := TGpCountedIntegerList(list).Counter[index2];
  if item1 > item2 then
    Result := -1
  else if item1 < item2 then
    Result := 1
  else
    Result := 0;
end; { CompareDescending_CIL }

constructor TGpCountedIntegerList.Create;
begin
  inherited Create(false);
end; { TGpCountedIntegerList.Create }

function TGpCountedIntegerList.Add(item, count: integer): integer;
begin
  Result := inherited AddObject(item, TObject(count));
end; { TGpCountedIntegerList.Add }

class function TGpCountedIntegerList.CreateInterface: IGpCountedIntegerList;
begin
  Result := TGpCountedIntegerList.Create;
end; { TGpCountedIntegerList.CreateInterface }

function TGpCountedIntegerList.Ensure(item, count: integer): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item, count)
  else
    Counter[Result] := count;
end; { TGpCountedIntegerList.Ensure }

function TGpCountedIntegerList.Fetch(item: integer): integer;
var
  idx: integer;
begin
  idx := IndexOf(item);
  if idx < 0 then
    Result := 0
  else
    Result := Counter[idx];
end; { TGpCountedIntegerList.Fetch }

function TGpCountedIntegerList.GetCounter(idx: integer): integer;
begin
  Result := integer(Objects[idx]);
end; { TGpCountedIntegerList.GetCounter }

function TGpCountedIntegerList.GetItemCounter(item: integer): integer;
begin
  Result := Counter[IndexOf(item)];
end; { TGpCountedInt64List.GetItemCounter }

procedure TGpCountedIntegerList.Insert(idx: integer; item, count: integer);
begin
  inherited InsertObject(idx, item, TObject(count));
end; { TGpCountedIntegerList.Insert }

procedure TGpCountedIntegerList.SetCounter(idx: integer; const value: integer);
begin
  Objects[idx] := TObject(value);
end; { TGpCountedIntegerList.SetCounter }

procedure TGpCountedIntegerList.SetItemCounter(item: integer; const value: integer);
begin
  Counter[IndexOf(item)] := value;
end; { TGpCountedInt64List.SetItemCounter }

{$IFDEF GpLists_Sorting}
procedure TGpCountedIntegerList.SortByCounter(descending: boolean);
begin
  Sorted := false;
  if descending then
    CustomSort(CompareDescending_CIL)
  else
    CustomSort(CompareAscending_CIL);
  Sorted := false;
end; { TGpCountedIntegerList.SortByCounter }
{$ENDIF}

{$IFDEF GpLists_Enumerators}

{ TGpInt64ObjectListWalkKVEnumerator }

constructor TGpInt64ObjectListWalkKVEnumerator.Create(aList: TGpInt64ObjectList; idxFrom,
  idxTo, step: integer);
begin
  inherited Create;
  wkeListEnumerator := TGpInt64ListWalkEnumerator.Create(aList, idxFrom, idxTo, step);
  wkeCurrentKV := TGpKeyValue.Create;
end; { TGpInt64ObjectListWalkKVEnumerator.Create }

destructor TGpInt64ObjectListWalkKVEnumerator.Destroy;
begin
  FreeAndNil(wkeCurrentKV);
  FreeAndNil(wkeListEnumerator);
  inherited;
end; { TGpInt64ObjectListWalkKVEnumerator.Destroy }

function TGpInt64ObjectListWalkKVEnumerator.GetCurrent: TGpKeyValue;
var
  idx: integer;
begin
  idx := wkeListEnumerator.GetCurrent;
  wkeCurrentKV.Key := wkeListEnumerator.List[idx];
  wkeCurrentKV.Value := TGpInt64ObjectList(wkeListEnumerator.List).Objects[idx];
  Result := wkeCurrentKV;
end; { TGpInt64ObjectListWalkKVEnumerator.GetCurrent }

function TGpInt64ObjectListWalkKVEnumerator.MoveNext: boolean;
begin
  Result := wkeListEnumerator.MoveNext;
end; { TGpInt64ObjectListWalkKVEnumerator.MoveNext }

{ TGpInt64ObjectListWalkKVEnumeratorFactory }

constructor TGpInt64ObjectListWalkKVEnumeratorFactory.Create(list: TGpInt64ObjectList;
  idxFrom, idxTo, step: integer);
begin
  wkefList := list;
  wkefIdxFrom := idxFrom;
  wkefIdxTo := idxTo;
  wkefStep := step;
end; { TGpInt64ObjectListWalkKVEnumeratorFactory.Create }

function TGpInt64ObjectListWalkKVEnumeratorFactory.GetEnumerator:
  TGpInt64ObjectListWalkKVEnumerator;
begin
  Result := TGpInt64ObjectListWalkKVEnumerator.Create(wkefList, wkefIdxFrom, wkefIdxTo, wkefStep);
end; { TGpInt64ObjectListWalkKVEnumeratorFactory.GetEnumerator }
{$ENDIF GpLists_Enumerators}

{ TGpInt64ObjectList }

constructor TGpInt64ObjectList.Create(ownsObjects: boolean);
begin
  inherited Create;
  iolObjects := TObjectList.Create(ownsObjects);
end; { TGpInt64ObjectList.Create }

destructor TGpInt64ObjectList.Destroy;
begin
  FreeAndNil(iolObjects);
  inherited;
end; { TGpInt64ObjectList.Destroy }

function TGpInt64ObjectList.Add(item: int64): integer;
begin
  Result := AddObject(item, nil);
end; { TGpInt64ObjectList.Add }

function TGpInt64ObjectList.AddObject(item: int64; obj: TObject): integer;
begin
  if Sorted and (Duplicates = dupIgnore) then begin
    Result := IndexOf(item);
    if Result >= 0 then begin
      Objects[Result] := obj;
      Exit;
    end;
  end;
  Result := inherited Add(item);
  iolObjects.Insert(Result, obj);
  Assert(Count = iolObjects.Count,
    'TGpInt64ObjectList.AddObject: Number of items and objects differ');
end; { TGpInt64ObjectList.AddObject }

procedure TGpInt64ObjectList.Clear;
begin
  inherited;
  iolObjects.Clear;
end; { TGpInt64ObjectList.Clear }

class function TGpInt64ObjectList.CreateInterface(ownsObjects: boolean):
  IGpInt64ObjectList;
begin
  Result := TGpInt64ObjectList.Create(ownsObjects);
end; { TGpInt64ObjectList.CreateInterface }

procedure TGpInt64ObjectList.Delete(idx: integer);
begin
  inherited;
  iolObjects.Delete(idx);
  Assert(Count = iolObjects.Count,
    'TGpInt64ObjectList.Delete: Number of items and objects differ');
end; { TGpInt64ObjectList.Delete }

{:Dumps the list into memory block starting at baseAddr.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpInt64ObjectList.Dump(baseAddr: pointer): pointer;
var
  iList: integer;
  pList: PDWORD;
begin
  pList := baseAddr;
  pList^ := Count;
  Inc(pList);
  for iList := 0 to Count-1 do begin
    pList^ := DWORD(Int64Rec(Items[iList]).Lo);
    Inc(pList);
    pList^ := DWORD(Int64Rec(Items[iList]).Hi);
    Inc(pList);
    pList^ := DWORD(Objects[iList]);
    Inc(pList);
  end;
  Result := pList;
end; { TGpInt64ObjectList.Dump }

function TGpInt64ObjectList.EnsureObject(item: int64; obj: TObject): integer;
begin
  Result := inherited Ensure(item);
  Objects[Result] := obj;
end; { TGpInt64ObjectList.EnsureObject }

function TGpInt64ObjectList.EnsureObject(item: int64; objClass: TClass): integer;
begin
  Result := inherited Ensure(item);
  if not assigned(Objects[Result]) then
    Objects[Result] := objClass.Create;
end; { TGpInt64ObjectList.EnsureObject }

procedure TGpInt64ObjectList.Exchange(idx1, idx2: integer);
begin
  inherited;
  iolObjects.Exchange(idx1, idx2);
end; { TGpInt64ObjectList.Exchange }

function TGpInt64ObjectList.ExtractObject(idxObject: integer): TObject;
begin
  Result := Objects[idxObject];
  iolObjects.Extract(iolObjects[idxObject]);
  inherited Delete(idxObject);
end; { TGpInt64ObjectList.ExtractObject }

function TGpInt64ObjectList.FetchObject(item: int64): TObject;
var
  idxItem: integer;
begin
  idxItem := IndexOf(item);
  if idxItem >= 0 then
    Result := Objects[idxItem]
  else
    Result := nil;
end; { TGpInt64ObjectList.FetchObject }

procedure TGpInt64ObjectList.FreeObjects;
var
  iObject: integer;
begin
  for iObject := 0 to Count - 1 do begin
    Objects[iObject].Free;
    Objects[iObject] := nil;
  end;
end; { TGpInt64ObjectList.FreeObjects }

function TGpInt64ObjectList.GetObject(idxObject: integer): TObject;
begin
  Result := iolObjects[idxObject];
end; { TGpInt64ObjectList.GetObject }

procedure TGpInt64ObjectList.Insert(idx: integer; item: int64);
begin
  InsertObject(idx, item, nil);
end; { TGpInt64ObjectList.Insert }

procedure TGpInt64ObjectList.InsertObject(idx: integer; item: int64; obj: TObject);
begin
  inherited Insert(idx, item);
  iolObjects.Insert(idx, obj);
  Assert(Count = iolObjects.Count,
    'TGpInt64ObjectList.InsertObject: Number of items and objects differ');
end; { TGpInt64ObjectList.InsertObject }

function TGpInt64ObjectList.LoadFromStream(stream: TStream): boolean;
var
  item: int64;
  obj : TObject;
  read: integer;
begin
  Result := false;
  Clear;
  repeat
    read := stream.Read(item, 8);
    if read = 0 then
      break; //repeat
    if read <> 4 then
      Exit;
    read := stream.Read(obj, 4);
    if read <> 4 then
      Exit;
    AddObject(item, obj)
  until read = 0;
  Result := true;
end; { TGpInt64ObjectList.LoadFromStream }

procedure TGpInt64ObjectList.Move(curIdx, newIdx: integer);
begin
  inherited;
  iolObjects.Move(curIdx, newIdx);
end; { TGpInt64ObjectList.Move }

{:Restores the list dumped by the Dump method.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpInt64ObjectList.Restore(baseAddr: pointer): pointer;
var
  iList   : integer;
  item    : int64;
  numItems: integer;
  pList   : PDWORD;
begin
  pList := baseAddr;
  numItems := integer(pList^);
  Inc(pList);
  ilList.Capacity := numItems;
  iolObjects.Capacity := numItems;
  for iList := 0 to numItems-1 do begin
    Int64Rec(item).Lo := cardinal(pList^);
    Inc(pList);
    Int64Rec(item).Hi := cardinal(pList^);
    Inc(pList);
    AddObject(item, TObject(pList^));
    Inc(pList);
  end;
  Result := pList;
end; { TGpInt64ObjectList.Restore }

procedure TGpInt64ObjectList.SaveToStream(stream: TStream);
var
  iItem: integer;
  item : int64;
  obj  : TObject;
begin
  for iItem := 0 to Count-1 do begin
    item := Items[iItem];
    stream.WriteBuffer(item, 8);
    obj := Objects[iItem];
    stream.WriteBuffer(obj, 4);
  end;
end; { TGpInt64ObjectList.SaveToStream }

procedure TGpInt64ObjectList.SetObject(idxObject: integer; const value: TObject);
begin
  iolObjects[idxObject] := value;
end; { TGpInt64ObjectList.SetObject }

{$IFDEF GpLists_Enumerators}
function TGpInt64ObjectList.WalkKV(idxFrom, idxTo, step: integer):
  TGpInt64ObjectListWalkKVEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpInt64ObjectListWalkKVEnumeratorFactory.Create(Self, idxFrom, idxTo, step);
end; { TGpInt64ObjectList.WalkKV }
{$ENDIF GpLists_Enumerators}

{$IFDEF Unicode}
{$IFNDEF GpLists_LimitedGenerics}
{ TGpInt64ObjectList<T> }

class function TGpInt64ObjectList<T>.CreateInterface(ownsObjects: boolean = true):
  IGpInt64ObjectList<T>;
begin
  Result := TGpInt64ObjectList<T>.Create(ownsObjects);
end; { TGpInt64ObjectList<T>.CreateInterface }

function TGpInt64ObjectList<T>.AddObject(item: int64; obj: T): integer;
begin
  Result := inherited AddObject(item, obj);
end; { TGpInt64ObjectList<T>.AddObject }

function TGpInt64ObjectList<T>.EnsureObject(item: int64; obj: T): integer;
begin
  Result := inherited EnsureObject(item, TObject(PPointer(@obj)^));
end; { TGpInt64ObjectList<T>.EnsureObject }

function TGpInt64ObjectList<T>.EnsureObject(item: int64): integer;
begin
  Result := inherited EnsureObject(item, TClass(T));
end; { TGpInt64ObjectList<T>.EnsureObject }

function TGpInt64ObjectList<T>.ExtractObject(idxObject: integer): T;
begin
  Result := T(inherited ExtractObject(idxObject));
end; { TGpInt64ObjectList<T>.ExtractObject }

function TGpInt64ObjectList<T>.FetchObject(item: int64): T;
begin
  Result := T(inherited FetchObject(item));
end; { TGpInt64ObjectList<T>.FetchObject }

function TGpInt64ObjectList<T>.GetObject(idxObject: integer): T;
begin
  Result := T(inherited GetObject(idxObject));
end; { TGpInt64ObjectList<T>.GetObject }

procedure TGpInt64ObjectList<T>.InsertObject(idx: integer; item: int64; obj: T);
begin
  inherited InsertObject(idx, item, obj);
end; { TGpInt64ObjectList<T>.InsertObject }

procedure TGpInt64ObjectList<T>.SetObject(idxObject: integer; const value: T);
begin
  inherited SetObject(idxObject, value);
end; { TGpInt64ObjectList<T>.SetObject }
{$ENDIF GpLists_LimitedGenerics}
{$ENDIF Unicode}

{ TGpCountedInt64List }

function CompareAscending_CI64L(list: TGpInt64List; index1, index2: integer): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}
var
  item1: int64;
  item2: int64;
begin
  item1 := TGpCountedInt64List(list).Counter[index1];
  item2 := TGpCountedInt64List(list).Counter[index2];
  if item1 < item2 then
    Result := -1
  else if item1 > item2 then
    Result := 1
  else
    Result := 0;
end; { CompareAscending_CI64L }

function CompareDescending_CI64L(list: TGpInt64List; index1, index2: integer): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}
var
  item1: int64;
  item2: int64;
begin
  item1 := TGpCountedInt64List(list).Counter[index1];
  item2 := TGpCountedInt64List(list).Counter[index2];
  if item1 > item2 then
    Result := -1
  else if item1 < item2 then
    Result := 1
  else
    Result := 0;
end; { CompareDescending_CI64L }

function TGpCountedInt64List.Add(item: int64; count: int64): integer;
begin
  Result := inherited AddObject(item, TGpInt64.Create(count));
end; { TGpCountedInt64List.Add }

class function TGpCountedInt64List.CreateInterface(ownsObjects: boolean):
  IGpCountedInt64List;
begin
  Result := TGpCountedInt64List.Create(ownsObjects);
end; { TGpCountedInt64List.CreateInterface }

function TGpCountedInt64List.Ensure(item: int64; count: int64): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item, count)
  else
    Counter[Result] := count;
end; { TGpCountedInt64List.Ensure }

function TGpCountedInt64List.Fetch(item: integer): int64;
var
  idx: integer;
begin
  idx := IndexOf(item);
  if idx < 0 then
    Result := 0
  else
    Result := Counter[idx];
end; { TGpCountedInt64List.Fetch }

function TGpCountedInt64List.GetCounter(idx: integer): int64;
begin
  Result := TGpInt64(Objects[idx]).Value;
end; { TGpCountedInt64List.GetCounter }

function TGpCountedInt64List.GetItemCounter(item: int64): int64;
begin
  Result := Counter[IndexOf(item)];
end; { TGpCountedInt64List.GetItemCounter }

procedure TGpCountedInt64List.Insert(idx: integer; item, count: int64);
begin
  inherited InsertObject(idx, item, TGpInt64.Create(count));
end; { TGpCountedInt64List.Insert }

procedure TGpCountedInt64List.SetCounter(idx: integer; const value: int64);
begin
  TGpInt64(Objects[idx]).Value := value;
end; { TGpCountedInt64List.SetCounter }

procedure TGpCountedInt64List.SetItemCounter(item: int64; const value: int64);
begin
  Counter[IndexOf(item)] := value;
end; { TGpCountedInt64List.SetItemCounter }

procedure TGpCountedInt64List.SortByCounter(descending: boolean);
begin
  Sorted := false;
  if descending then
    CustomSort(CompareDescending_CI64L)
  else
    CustomSort(CompareAscending_CI64L);
  Sorted := false;
end; { TGpCountedInt64List.SortByCounter }

{$IFDEF GpLists_Enumerators}

{ TGpGUIDListEnumerator }

constructor TGpGUIDListEnumerator.Create(aList: TGpGUIDList; idxFrom, idxTo, step:
  integer);
begin
  gleIndex := idxFrom - step;
  gleList := aList;
  gleIdxTo := idxTo;
  gleStep := step;
end; { TGpGUIDListEnumerator.Create }

function TGpGUIDListEnumerator.GetCurrent: TGUID;
begin
  Result := gleList[gleIndex];
end; { TGpGUIDListEnumerator.GetCurrent }

function TGpGUIDListEnumerator.MoveNext: boolean;
begin
  Inc(gleIndex, gleStep);
  Result := (gleIndex <= gleIdxTo);
end; { TGpGUIDListEnumerator.MoveNext }

{ TGpGUIDListWalkEnumerator }

constructor TGpGUIDListWalkEnumerator.Create(aList: TGpGUIDList; idxFrom, idxTo, step:
  integer);
begin
  inherited Create;
  glweList := aList;
  glweIndex := idxFrom - step;
  glweIdxTo := idxTo;
  glweStep := step;
  glweList.RegisterNotification(HandleListChange);
end; { TGpGUIDListWalkEnumerator.Create }

destructor TGpGUIDListWalkEnumerator.Destroy;
begin
  glweList.UnregisterNotification(HandleListChange);
  inherited;
end; { TGpGUIDListWalkEnumerator.Destroy }

function TGpGUIDListWalkEnumerator.GetCurrent: integer;
begin
  Result := glweIndex;
end; { TGpGUIDListWalkEnumerator.GetCurrent }

procedure TGpGUIDListWalkEnumerator.HandleListChange(list: TObject; idxItem: integer;
  operation: TGpListOperation);
begin
  case operation of
    loInsert:
      if idxItem < glweIndex then begin
        Inc(glweIndex);
        Inc(glweIdxTo);
      end
      else if idxItem < glweIdxTo then
        Inc(glweIdxTo);
    loDelete:
      if idxItem < glweIndex then begin
        Dec(glweIndex);
        Dec(glweIdxTo);
      end
      else if idxItem < glweIdxTo then
        Dec(glweIdxTo);
    else raise Exception.Create('TGpGUIDListWalkEnumerator.HandleListChange: Unexpected list operation');
  end;
end; { TGpGUIDListWalkEnumerator.HandleListChange }

function TGpGUIDListWalkEnumerator.MoveNext: boolean;
begin
  Inc(glweIndex, glweStep);
  Result := (glweIndex <= glweIdxTo);
end; { TGpGUIDListWalkEnumerator.MoveNext }

{ TGpGUIDListSliceEnumeratorFactory }

constructor TGpGUIDListSliceEnumeratorFactory.Create(list: TGpGUIDList; idxFrom,
  idxTo, step: integer);
begin
  sefList := list;
  sefIdxFrom := idxFrom;
  sefIdxTo := idxTo;
  sefStep := step;
end; { TGpGUIDListSliceEnumeratorFactory.Create }

function TGpGUIDListSliceEnumeratorFactory.GetEnumerator: TGpGUIDListEnumerator;
begin
  Result := TGpGUIDListEnumerator.Create(sefList, sefIdxFrom, sefIdxTo, sefStep);
end; { TGpGUIDListSliceEnumeratorFactory.GetEnumerator }

{ TGpGUIDListWalkEnumeratorFactory }

constructor TGpGUIDListWalkEnumeratorFactory.Create(list: TGpGUIDList; idxFrom, idxTo,
  step: integer);
begin
  wefList := list;
  wefIdxFrom := idxFrom;
  wefIdxTo := idxTo;
  wefStep := step;
end; { TGpGUIDListWalkEnumeratorFactory.Create }

function TGpGUIDListWalkEnumeratorFactory.GetEnumerator: TGpGUIDListWalkEnumerator;
begin
  Result := TGpGUIDListWalkEnumerator.Create(wefList, wefIdxFrom, wefIdxTo, wefStep);
end; { TGpGUIDListWalkEnumeratorFactory.GetEnumerator }

{$ENDIF GpLists_Enumerators}

{ TGpGUIDList }

constructor TGpGUIDList.Create;
begin
  inherited Create;
  glList := TGpCountedInt64List.Create;
  glNotificationHandlers := TGpTMethodList.Create;
end; { TGpGUIDList.Create }

destructor TGpGUIDList.Destroy;
begin
  FreeAndNil(glNotificationHandlers);
  FreeAndNil(glList);
  inherited;
end; { TGpGUIDList.Destroy }

function TGpGUIDList.Add(item: TGUID): integer;
begin
  if not glSorted then
    Result := glList.Add(TGpGUIDRec(item).Lo, TGpGUIDRec(item).Hi)
  else begin
    if Find(item, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError : raise EListError.CreateFmt(SGUIDDuplicateItem, [GUIDToString(item)]);
      end;
    InsertItem(Result, item);
  end;
  Notify(Result, loInsert);
end; { TGpGUIDList.Add }

procedure TGpGUIDList.Append(const elements: array of TGUID);
var
  iElement: integer;
begin
  SetCapacity(Count + Length(elements));
  for iElement := Low(elements) to High(elements) do
    Add(elements[iElement]);
end; { TGpGUIDList.Append }

procedure TGpGUIDList.Append(list: TGpGUIDList);
var
  iItem: integer;
begin
  SetCapacity(Count + list.Count);
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpGUIDList.Append }

procedure TGpGUIDList.Append(list: IGpGUIDList);
var
  iItem: integer;
begin
  SetCapacity(Count + list.Count);
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpGUIDList.Append }

function TGpGUIDList.AsDelimitedText(const delimiter: string): string;
begin
  Result := GetAsDelimitedText(delimiter, false);
end; { TGpGUIDList.AsDelimitedText }

procedure TGpGUIDList.Assign(const elements: array of TGUID);
begin
  Clear;
  Append(elements);
end; { TGpGUIDList.Assign }

procedure TGpGUIDList.Assign(list: TGpGUIDList);
begin
  Clear;
  Append(list);
end; { TGpGUIDList.Assign }

procedure TGpGUIDList.Assign(list: IGpGUIDList);
begin
  Clear;
  Append(list);
end; { TGpGUIDList.Assign }

procedure TGpGUIDList.Clear;
begin
  glList.Clear;
end; { TGpGUIDList.Clear }

function TGpGUIDList.Contains(const item: TGUID; var idx: integer): boolean;
begin
  idx := IndexOf(item);
  Result := (idx >= 0);
end; { TGpGUIDList.Contains }

function TGpGUIDList.Contains(const item: TGUID): boolean;
var
  idx: integer;
begin
  Result := Contains(item, idx);
end; { TGpGUIDList.Contains }

class function TGpGUIDList.CreateInterface: IGpGUIDList;
begin
  Result := TGpGUIDList.Create;
end; { TGpGUIDList.CreateInterface }

{$IFDEF GpLists_Sorting}
procedure TGpGUIDList.CustomSort(sortMethod: TGpGUIDListSortCompare);
begin
  if not Sorted and (Count > 1) then
    QuickSort(0, Count - 1, sortMethod);
end; { TGpGUIDList.CustomSort }
{$ENDIF}

procedure TGpGUIDList.Delete(idx: integer);
begin
  glList.Delete(idx);
  Notify(idx, loDelete);
end; { TGpGUIDList.Delete }

function TGpGUIDList.Ensure(const item: TGUID): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end; { TGpGUIDList.Ensure }

function TGpGUIDList.EqualTo(list: TGpGUIDList): boolean;
var
  iList: integer;
begin
  Result := Count = list.Count;
  if Result then begin
    for iList := 0 to Count - 1 do
      if GUIDCompare(Items[iList], list.GetItems(iList)) <> 0 then begin
        Result := false;
        break; //for iList
      end;
  end;
end; { TGpGUIDList.EqualTo }

procedure TGpGUIDList.Exchange(idx1, idx2: integer);
begin
  glList.Exchange(idx1, idx2);
end; { TGpGUIDList.Exchange }

function TGpGUIDList.Find(const avalue: TGUID; var idx: integer): boolean;
var
  L, H, I, C: integer;
begin
  Assert(glSorted, 'Find only works on sorted lists!');
  Result := false;
  L := 0;
  H := Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := GUIDCompare(Items[I], avalue);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := true;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  idx := L;
end; { TGpGUIDList.Find }

function TGpGUIDList.First: TGUID;
begin
  Result := Items[0];
end; { TGpGUIDList.First }

function TGpGUIDList.GetAsDelimitedText(const delimiter: string;
  appendLastDelimiter: boolean): string;
var
  guid    : TGUID;
  iItem   : integer;
  lenDelim: integer;
  lenItem : integer;
  p       : PChar;
  q       : PChar;
  sItem   : string;
  size    : integer;
begin
  FillChar(guid, SizeOf(guid), 0);
  lenDelim := Length(delimiter);
  size := Count * (lenDelim + Length(GUIDToString(guid)));
  if not appendLastDelimiter then
    Dec(size, lenDelim);
  SetString(Result, nil, size);
  p := Pointer(Result);
  for iItem := 0 to Count-1 do begin
    sItem := GUIDToString(Items[iItem]);
    lenItem := Length(sItem);
    if lenItem <> 0 then begin
      System.Move(pointer(sItem)^, p^, lenItem*SizeOf(char));
      Inc(p, lenItem);
    end;
    if appendLastDelimiter or (iItem < (Count-1)) then begin
      q := Pointer(delimiter);
      while q^ <> #0 do begin
        p^ := q^;
        Inc(p);
        Inc(q);
      end; //while
    end;
  end;
end; { TGpGUIDList.GetAsDelimitedText }

function TGpGUIDList.GetCapacity: integer;
begin
  Result := glList.Capacity;
end; { TGpGUIDList.GetCapacity }

function TGpGUIDList.GetCount: integer;
begin
  Result := glList.Count;
end; { TGpGUIDList.GetCount }

function TGpGUIDList.GetDuplicates: TDuplicates;
begin
  Result := glDuplicates;
end; { TGpGUIDList.GetDuplicates }

function TGpGUIDList.GetItems(idx: integer): TGUID;
begin
  TGpGUIDRec(Result).Lo := glList.Items[idx];
  TGpGUIDRec(Result).Hi := glList.Counter[idx];
end; { TGpGUIDList.GetItems }

function TGpGUIDList.GetText: string;
begin
  Result := GetAsDelimitedText(#13#10, true);
end; { TGpGUIDList.GetText }

{$IFDEF GpLists_Enumerators}
function TGpGUIDList.GetEnumerator: TGpGUIDListEnumerator;
begin
  Result := TGpGUIDListEnumerator.Create(Self, 0, Count - 1, 1);
end; { TGpGUIDList.GetEnumerator }

function TGpGUIDList.GetSorted: boolean;
begin
  Result := glSorted;
end; { TGpGUIDList.GetSorted }

{$ENDIF GpLists_Enumerators}

function TGpGUIDList.IndexOf(const item: TGUID): integer;
begin
  {$IFDEF GpLists_Sorting}
  if Sorted then begin
    if not Find(item, Result) then
      Result := -1
  end
  else begin
  {$ENDIF}
    Result := 0;
    while Result < Count do
      if GUIDCompare(item, Items[Result]) = 0 then
        Exit;
    Result := -1;
  {$IFDEF GpLists_Sorting}
  end;
  {$ENDIF}
end; { TGpGUIDList.IndexOf }

procedure TGpGUIDList.Insert(idx: integer; const item: TGUID);
begin
  {$IFDEF GpLists_Sorting}
  if Sorted then
    raise Exception.Create('Cannot insert element in sorted list.');
  {$ENDIF}
  InsertItem(idx, item);
end; { TGpGUIDList.Insert }

procedure TGpGUIDList.InsertItem(idx: integer; const item: TGUID);
begin
  glList.Insert(idx, TGpGUIDRec(item).Lo, TGpGUIDRec(item).Hi);
  Notify(idx, loInsert);
end; { TGpGUIDList.InsertItem }

function TGpGUIDList.Last: TGUID;
begin
  Result := Items[Count-1];
end; { TGpGUIDList.Last }

function TGpGUIDList.LoadFromStream(stream: TStream): boolean;
begin
  Result := glList.LoadFromStream(stream);
end; { TGpGUIDList.LoadFromStream }

procedure TGpGUIDList.Move(curIdx, newIdx: integer);
begin
  glList.Move(curIdx, newIdx);
end; { TGpGUIDList.Move }

procedure TGpGUIDList.Notify(idxItem: integer; operation: TGpListOperation);
var
  iHandler: integer;
begin
  if glNotificationHandlers.Count = 0 then
    Exit;
  for iHandler := 0 to glNotificationHandlers.Count - 1 do
    TGpListNotificationEvent(glNotificationHandlers[iHandler])(Self, idxItem, operation);
end; { TGpGUIDList.Notify }

procedure TGpGUIDList.QuickSort(L, R: integer; SCompare: TGpGUIDListSortCompare);
var
  I, J, P: integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while (SCompare(Self, I, P) < 0) do
        Inc(I);
      while (SCompare(Self, J, P) > 0) do
        Dec(J);
      if (I <= J) then begin
        Exchange(I, J);
        if (P = I) then
          P := J
        else if (P = J) then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until (I > J);
    if (L < J) then
      QuickSort(L, J, SCompare);
    L := I;
  until (I >= R);
end; { TGpGUIDList.QuickSort }

procedure TGpGUIDList.RegisterNotification(notificationHandler: TGpListNotificationEvent);
begin
  glNotificationHandlers.Add(TMethod(notificationHandler));
end; { TGpGUIDList.RegisterNotification }

procedure TGpGUIDList.Remove(const item: TGUID);
var
  idxItem: integer;
begin
  idxItem := IndexOf(item);
  if idxItem >= 0 then
    Delete(idxItem);
end; { TGpGUIDList.Remove }

procedure TGpGUIDList.SaveToStream(stream: TStream);
begin
  glList.SaveToStream(stream);
end; { TGpGUIDList.SaveToStream }

procedure TGpGUIDList.SetCapacity(const value: integer);
begin
  glList.Capacity := value;
end; { TGpGUIDList.SetCapacity }

procedure TGpGUIDList.SetCount(const value: integer);
begin
  glList.Count := value;
end; { TGpGUIDList.SetCount }

procedure TGpGUIDList.SetDuplicates(const value: TDuplicates);
begin
  glDuplicates := value;
end; { TGpGUIDList.SetDuplicates }

procedure TGpGUIDList.SetItems(idx: integer; const value: TGUID);
begin
  glList.SetItems(idx, TGpGUIDRec(value).Lo);
  glList.SetItemCounter(idx, TGpGUIDRec(value).Hi);
end; { TGpGUIDList.SetItems }


{$IFDEF GpLists_Sorting}
procedure TGpGUIDList.SetSorted(const value: boolean);
begin
  if (glSorted <> value) then begin
    if value then
      CustomSort(GUIDListCompare);
    glSorted := value;
  end;
end; { TGpGUIDList.SetSorted }
{$ENDIF}

procedure TGpGUIDList.SetText(const value: string);
var
  p    : PChar;
  s    : string;
  start: PChar;
begin
  Clear;
  p := pointer(value);
  if P <> nil then
    while p^ <> #0 do begin
      start := p;
      while (p^ <> #0) and (p^ <> #10) and (p^ <> #13) do
        Inc(p);
      SetString(s, start, p - start);
      Add(StringToGUID(s));
      if p^ = #13 then Inc(p);
      if p^ = #10 then Inc(p);
    end;
end; { TGpGUIDList.SetText }

{$IFDEF GpLists_Sorting}
procedure TGpGUIDList.Sort;
begin
  Sorted := false;
  Sorted := true;
end; { TGpGUIDList.Sort }
{$ENDIF}

{$IFDEF GpLists_Enumerators}
function TGpGUIDList.Slice(idxFrom, idxTo, step: integer): TGpGUIDListSliceEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpGUIDListSliceEnumeratorFactory.Create(Self, idxFrom, idxTo, step);
end; { TGpGUIDList.Slice }
{$ENDIF GpLists_Enumerators}

procedure TGpGUIDList.UnregisterNotification(
  notificationHandler: TGpListNotificationEvent);
begin
  glNotificationHandlers.Remove(TMethod(notificationHandler));
end; { TGpGUIDList.UnregisterNotification }

{$IFDEF GpLists_Enumerators}
function TGpGUIDList.Walk(idxFrom, idxTo, step: integer): TGpGUIDListWalkEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpGUIDListWalkEnumeratorFactory.Create(Self, idxFrom, idxTo, step);
end; { TGpGUIDList.Walk }
{$ENDIF GpLists_Enumerators}

{$IFDEF GpLists_TStringsHelper}

{$IFDEF Unicode}
{$IFNDEF GpLists_LimitedGenerics}
{ TGpInterfaceList<T> }

constructor TGpInterfaceList<T>.Create;
begin
  inherited Create;
  ilList := TList<T>.Create;
end; { TGpInterfaceList }

destructor TGpInterfaceList<T>.Destroy;
begin
  FreeAndNil(ilList);
  inherited;
end; { TGpInterfaceList }

function TGpInterfaceList<T>.Add(const item: T): integer;
begin
  ilList.Add(item);
end; { TGpInterfaceList<T>.Add }

procedure TGpInterfaceList<T>.Clear;
begin
  ilList.Clear;
end; { TGpInterfaceList<T>.Clear }

function TGpInterfaceList<T>.Contains(const item: T; var idx: integer): boolean;
begin
  idx := IndexOf(item);
  Result := (idx >= 0);
end; { TGpInterfaceList }

function TGpInterfaceList<T>.Contains(const item: T): boolean;
var
  idx: integer;
begin
  Result := Contains(item, idx);
end; { TGpInterfaceList<T>.Contains }

class function TGpInterfaceList<T>.CreateInterface: IGpInterfaceList<T>;
begin
  Result := TGpInterfaceList<T>.Create;
end; { TGpInterfaceList<T>.CreateInterface }

procedure TGpInterfaceList<T>.Delete(idx: integer);
begin
  Items[idx] := Default(T);
  ilList.Delete(idx);
end; { TGpInterfaceList<T>.Delete }

function TGpInterfaceList<T>.Ensure(const item: T): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end; { TGpInterfaceList<T>.Ensure }

procedure TGpInterfaceList<T>.Exchange(idx1, idx2: integer);
begin
  ilList.Exchange(idx1, idx2);
end; { TGpInterfaceList<T>.Exchange }

function TGpInterfaceList<T>.First: T;
begin
  Result := Items[0];
end; { TGpInterfaceList<T>.First }

function TGpInterfaceList<T>.GetCapacity: integer;
begin
  Result := ilList.Capacity;
end; { TGpInterfaceList<T>.GetCapacity }

function TGpInterfaceList<T>.GetCount: integer;
begin
  Result := ilList.Count;
end; { TGpInterfaceList<T>.GetCount }

function TGpInterfaceList<T>.GetEnumerator: TList<T>.TEnumerator;
begin
  Result := ilList.GetEnumerator;
end; { TGpInterfaceList<T>.GetEnumerator }

function TGpInterfaceList<T>.GetItem(idx: integer): T;
begin
  Result := ilList[idx];
end; { TGpInterfaceList<T>.GetItem }

function TGpInterfaceList<T>.IndexOf(const item: T): integer;
begin
  Result := ilList.IndexOf(item);
end; { TGpInterfaceList<T>.IndexOf }

procedure TGpInterfaceList<T>.Insert(idx: integer; const item: T);
begin
  ilList.Insert(idx, item);
end; { TGpInterfaceList<T>.Insert }

function TGpInterfaceList<T>.Last: T;
begin
  Result := Items[Count - 1];
end; { TGpInterfaceList<T>.Last }

procedure TGpInterfaceList<T>.Move(curIdx, newIdx: integer);
begin
  ilList.Move(curIdx, newIdx);
end; { TGpInterfaceList<T>.Move }

procedure TGpInterfaceList<T>.Remove(const item: T);
var
  iItem: integer;
begin
  iItem := IndexOf(item);
  if iItem >= 0 then
    Delete(iItem);
end; { TGpInterfaceList<T>.Remove }

procedure TGpInterfaceList<T>.SetCapacity(const value: integer);
begin
  ilList.Capacity := value;
end; { TGpInterfaceList<T>.SetCapacity }

procedure TGpInterfaceList<T>.SetCount(const value: integer);
begin
  ilList.Count := value;
end; { TGpInterfaceList<T>.SetCount }

procedure TGpInterfaceList<T>.SetItem(idx: integer; const value: T);
begin
  ilList[idx] := value;
end; { TGpInterfaceList<T>.SetItem }
{$ENDIF GpLists_LimitedGenerics}
{$ENDIF Unicode}

{ TGpStringsHelperWalkKVEnumerator }

constructor TGpStringsHelperWalkKVEnumerator.Create(sl: TStrings; idxFrom, idxTo, step:
  integer);
begin
  sheStrings := sl;
  sheIndex := idxFrom - step;
  sheIdxTo := idxTo;
  sheStep := step;
end; { TGpStringsHelperWalkKVEnumerator.Create }

function TGpStringsHelperWalkKVEnumerator.GetCurrent: TGpStringValue;
begin
  Result.Key := sheStrings[sheIndex];
  Result.Value := sheStrings.Objects[sheIndex];
end; { TGpStringsHelperWalkKVEnumerator.GetCurrent }

function TGpStringsHelperWalkKVEnumerator.GetEnumerator: TGpStringsHelperWalkKVEnumerator;
begin
  Result := Self;
end; { TGpStringsHelperWalkKVEnumerator.GetEnumerator }

function TGpStringsHelperWalkKVEnumerator.MoveNext: boolean;
begin
  Inc(sheIndex, sheStep);
  Result := (sheIndex <= sheIdxTo);
end; { TGpStringsHelperWalkKVEnumerator.MoveNext }

{ TGpStringsHelper }

function TGpStringsHelper.Contains(const s: string; var idx: integer): boolean;
begin
  idx := IndexOf(s);
  Result := (idx >= 0);
end; { TGpStringsHelper.Contains }

function TGpStringsHelper.Contains(const s: string): boolean;
var
  idx: integer;
begin
  Result := Contains(s, idx);
end; { TGpStringsHelper.Contains }

function TGpStringsHelper.FetchObject(const s: string): TObject;
var
  idxItem: integer;
begin
  idxItem := IndexOf(s);
  if idxItem >= 0 then
    Result := Objects[idxItem]
  else
    Result := nil;
end; { TGpStringsHelper.FetchObject }

procedure TGpStringsHelper.FreeObjects;
var
  iObject: integer;
begin
  for iObject := 0 to Count - 1 do begin
    Objects[iObject].Free;
    Objects[iObject] := nil;
  end;
end; { TGpStringsHelper.FreeObjects }

function TGpStringsHelper.Last: string;
begin
  Result := Strings[Count - 1];
end; { TGpStringsHelper.Last }

procedure TGpStringsHelper.Remove(const s: string);
var
  idxItem: integer;
begin
  idxItem := IndexOf(s);
  if idxItem >= 0 then
    Delete(idxItem);
end; { TGpStringsHelper.Remove }

procedure TGpStringsHelper.RemoveObject(const s: string);
var
  idxItem: integer;
begin
  idxItem := IndexOf(s);
  if idxItem >= 0 then begin
    Objects[idxItem].Free;
    Delete(idxItem);
  end;
end; { TGpStringsHelper.RemoveObject }

function TGpStringsHelper.WalkKV(idxFrom: integer; idxTo: integer;
  step: integer): TGpStringsHelperWalkKVEnumerator;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpStringsHelperWalkKVEnumerator.Create(Self, idxFrom, idxTo, step);
end; { TGpStringsHelper.WalkKV }

{ TGpStringListHelper }

procedure TGpStringListHelper.Sort;
begin
  Sorted := false;
  Sorted := true;
end; { TGpStringListHelper.Sort }

{$ENDIF GpLists_TStringsHelper}

{ TGpCountedStringList }

function CompareAscending_CSL(list: TStringList; index1, index2: integer): integer;
var
  item1: integer;
  item2: integer;
begin
  item1 := TGpCountedStringList(list).Counter[index1];
  item2 := TGpCountedStringList(list).Counter[index2];
  if item1 < item2 then
    Result := -1
  else if item1 > item2 then
    Result := 1
  else
    Result := 0;
end; { CompareAscending_CSL }

function CompareDescending_CSL(list: TStringList; index1, index2: integer): integer;
var
  item1: integer;
  item2: integer;
begin
  item1 := TGpCountedStringList(list).Counter[index1];
  item2 := TGpCountedStringList(list).Counter[index2];
  if item1 > item2 then
    Result := -1
  else if item1 < item2 then
    Result := 1
  else
    Result := 0;
end; { CompareDescending_CSL }

function TGpCountedStringList.Add(const s: string; count: integer): integer;
begin
  Result := inherited AddObject(s, pointer(count));
end; { TGpCountedStringList.Add }

function TGpCountedStringList.Contains(const s: string; var idx: integer): boolean;
begin
  idx := IndexOf(s);
  Result := (idx >= 0);
end; { TGpCountedStringList.Contains }

function TGpCountedStringList.Contains(const s: string): boolean;
var
  idx: integer;
begin
  Result := Contains(s, idx);
end; { TGpCountedStringList.Contains }

function TGpCountedStringList.Ensure(const s: string;
  count: integer): integer;
begin
  Result := IndexOf(s);
  if Result < 0 then
    Result := Add(s, count)
  else
    Counter[Result] := count;
end; { TGpCountedStringList.Ensure }

function TGpCountedStringList.GetItemCount(idx: integer): integer;
begin
  Result := integer(Objects[idx]);
end; { TGpCountedStringList.GetItemCount }

procedure TGpCountedStringList.SetItemCount(idx: integer; const value: integer);
begin
  Objects[idx] := pointer(value);
end; { TGpCountedStringList.SetItemCount }

procedure TGpCountedStringList.SortByCounter(descending: boolean);
begin
  Sorted := false;
  if descending then
    CustomSort(CompareDescending_CSL)
  else
    CustomSort(CompareAscending_CSL);
  Sorted := false;
end; { TGpCountedStringList.SortByCounter }

{ TAnsiStringStream }

{$IFDEF MSWINDOWS}
constructor TAnsiStringStream.Create(const AString: AnsiString);
begin
  inherited Create;
  FDataString := AString;
end;

function TAnsiStringStream.Read(var Buffer; Count: Longint): Longint;
begin
 Result := Length(FDataString) - FPosition;
  if Result > Count then Result := Count;
  Move(PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)])^, Buffer,
    Result * SizeOf(AnsiChar));
  Inc(FPosition, Result);
end;

function TAnsiStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  SetLength(FDataString, (FPosition + Result));
  Move(Buffer, PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)])^,
    Result * SizeOf(AnsiChar));
  Inc(FPosition, Result);
end;

function TAnsiStringStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FDataString) - Offset;
  end;
  if FPosition > Length(FDataString) then
    FPosition := Length(FDataString)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

function TAnsiStringStream.ReadString(Count: Longint): AnsiString;
var
  Len: integer;
begin
  Len := Length(FDataString) - FPosition;
  if Len > Count then Len := Count;
  SetString(Result, PChar(@FDataString[FPosition + SizeOf(Char)]), Len);
  Inc(FPosition, Len);
end;

procedure TAnsiStringStream.WriteString(const AString: AnsiString);
begin
  Write(PAnsiChar(AString)^, Length(AString) * SizeOf(AnsiChar));
end;

procedure TAnsiStringStream.SetSize(NewSize: Longint);
begin
  SetLength(FDataString, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
end;
{$ENDIF}

{ TGpTMethodListEnumerator }

{$IFDEF GpLists_Enumerators}
constructor TGpTMethodListEnumerator.Create(aList: TGpTMethodList);
begin
  mleIndex := -1;
  mleList := aList;
end; { TGpTMethodListEnumerator.Create }

function TGpTMethodListEnumerator.GetCurrent: TMethod;
begin
  Result := mleList[mleIndex];
end; { TGpTMethodListEnumerator.GetCurrent }

function TGpTMethodListEnumerator.MoveNext: boolean;
begin
  Result := mleIndex < (mleList.Count - 1);
  if Result then
    Inc(mleIndex);
end; { TGpTMethodListEnumerator.MoveNext }
{$ENDIF GpLists_Enumerators}

{ TGpTMethodList }

constructor TGpTMethodList.Create;
begin
  inherited;
  mlCode := TList.Create;
  mlData := TList.Create;
end; { TGpTMethodList.Create }

destructor TGpTMethodList.Destroy;
begin
  FreeAndNil(mlData);
  FreeAndNil(mlCode);
  inherited;
end; { TGpTMethodList.Destroy }

function TGpTMethodList.Add(item: TMethod): integer;
begin
  Result := mlCode.Add(item.Code);
  mlData.Add(item.Data);
end; { TGpTMethodList.Add }

procedure TGpTMethodList.Assign(list: TGpTMethodList);
begin
  Clear;
  mlCode.Assign(list.mlCode);
  mlData.Assign(list.mlData);
end; { TGpTMethodList.Assign }

procedure TGpTMethodList.Clear;
begin
  mlCode.Clear;
  mlData.Clear;
end; { TGpTMethodList.Clear }

function TGpTMethodList.Contains(item: TMethod; var idx: integer): boolean;
begin
  idx := IndexOf(item);
  Result := (idx >= 0);
end; { TGpTMethodList.Contains }

function TGpTMethodList.Contains(item: TMethod): boolean;
var
  idx: integer;
begin
  Result := Contains(item, idx);
end; { TGpTMethodList.Contains }

class function TGpTMethodList.CreateInterface: IGpTMethodList;
begin
  Result := TGpTMethodList.Create;
end; { TGpTMethodList.CreateInterface }

procedure TGpTMethodList.Delete(idx: integer);
begin
  mlCode.Delete(idx);
  mlData.Delete(idx);
end; { TGpTMethodList.Delete }

function TGpTMethodList.Ensure(item: TMethod): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end; { TGpTMethodList.Ensure }

function TGpTMethodList.GetCapacity: integer;
begin
  Result := mlCode.Capacity;
end; { TGpTMethodList.GetCapacity }

function TGpTMethodList.GetCount: integer;
begin
  Result := mlCode.Count;
end; { TGpTMethodList.GetCount }

{$IFDEF GpLists_Enumerators}
function TGpTMethodList.GetEnumerator: TGpTMethodListEnumerator;
begin
  Result := TGpTMethodListEnumerator.Create(Self);
end; { TGpTMethodList.GetEnumerator }
{$ENDIF GpLists_Enumerators}

function TGpTMethodList.GetItems(idx: integer): TMethod;
begin
  Result.Code := mlCode[idx];
  Result.Data := mlData[idx];
end; { TGpTMethodList.GetItems }

function TGpTMethodList.IndexOf(item: TMethod): integer;
begin
  for Result := 0 to Count - 1 do
    if (mlCode[Result] = item.Code) and (mlData[Result] = item.Data) then
      Exit;
  Result := -1;
end; { TGpTMethodList.IndexOf }

procedure TGpTMethodList.Insert(idx: integer; item: TMethod);
begin
  mlCode.Insert(idx, item.Code);
  mlData.Insert(idx, item.Data);
end; { TGpTMethodList.Insert }

procedure TGpTMethodList.Remove(item: TMethod);
var
  idxMethod: integer;
begin
  idxMethod := IndexOf(item);
  if idxMethod >= 0 then
    Delete(idxMethod);
end; { TGpTMethodList.Remove }

procedure TGpTMethodList.SetCapacity(const value: integer);
begin
  mlCode.Capacity := value;
  mlData.Capacity := value;
end; { TGpTMethodList.SetCapacity }

procedure TGpTMethodList.SetCount(const value: integer);
begin
  mlCode.Count := value;
  mlData.Count := value;
end; { TGpTMethodList.SetCount }

procedure TGpTMethodList.SetItems(idx: integer; const value: TMethod);
begin
  mlCode[idx] := value.Code;
  mlData[idx] := value.Data;
end; { TGpTMethodList.SetItems }

{ TGpClassListEnumerator }

{$IFDEF GpLists_Enumerators}
constructor TGpClassListEnumerator.Create(aList: TGpClassList);
begin
  cleIndex := -1;
  cleList := aList;
end; { TGpClassListEnumerator.Create }

function TGpClassListEnumerator.GetCurrent: TClass;
begin
  Result := cleList[cleIndex];
end; { TGpClassListEnumerator.GetCurrent }

function TGpClassListEnumerator.MoveNext: boolean;
begin
  Result := cleIndex < (cleList.Count - 1);
  if Result then
    Inc(cleIndex);
end; { TGpClassListEnumerator.MoveNext }
{$ENDIF GpLists_Enumerators}

{ TGpClassList }

constructor TGpClassList.Create;
begin
  inherited Create;
  clClasses := TStringList.Create;
  clClasses.Sorted := true;
end; { TGpClassList.Create }

destructor TGpClassList.Destroy;
begin
  FreeAndNil(clClasses);
  inherited Destroy;
end; { TGpClassList.Destroy }

procedure TGpClassList.Clear;
begin
  clClasses.Clear;
end; { TGpClassList.Clear }

procedure TGpClassList.Delete(idx: integer);
begin
  clClasses.Delete(idx);
end; { TGpClassList.Delete }

function TGpClassList.GetCapacity: integer;
begin
  Result := clClasses.Capacity;
end; { TGpClassList.GetCapacity }

function TGpClassList.GetCount: integer;
begin
  Result := clClasses.Count;
end; { TGpClassList.GetCount }

function TGpClassList.GetItems(idx: integer): TClass;
begin
  Result := TClass(clClasses.Objects[idx]);
end; { TGpClassList.GetItems }

function TGpClassList.Add(aClass: TClass): integer;
begin
  Result := IndexOf(aClass);
  if Result < 0 then
    Result := clClasses.AddObject(aClass.ClassName, TObject(aClass));
end; { TGpClassList.Add }

class function TGpClassList.CreateInterface: IGpClassList;
begin
  Result := TGpClassList.Create;
end; { TGpClassList.CreateInterface }

function TGpClassList.CreateObject(const sClass: string): TObject;
var
  idxClass: integer;
begin
  idxClass := IndexOf(sClass);
  if idxClass < 0 then
    raise Exception.CreateFmt('TGpClassList.CreateObject: Class %s is not registered',
      [sClass]);
  Result := Items[idxClass].Create;
end; { TGpClassList.CreateObject }

{$IFDEF GpLists_Enumerators}
function TGpClassList.GetEnumerator: TGpClassListEnumerator;
begin
  Result := TGpClassListEnumerator.Create(Self);
end; { TGpClassList.GetEnumerator }
{$ENDIF GpLists_Enumerators}

function TGpClassList.IndexOf(aClass: TClass): integer;
begin
  Result := clClasses.IndexOf(aClass.ClassName);
end; { TGpClassList.IndexOf }

function TGpClassList.IndexOf(const sClass: string): integer;
begin
  Result := clClasses.IndexOf(sClass);
end; { TGpClassList.IndexOf }

procedure TGpClassList.Remove(const sClass: string);
var
  idxItem: integer;
begin
  idxItem := IndexOf(sClass);
  if idxItem >= 0 then
    Delete(idxItem);
end; { TGpClassList.Remove }

procedure TGpClassList.Remove(aClass: TClass);
begin
  Remove(aClass.ClassName); 
end; { TGpClassList.Remove }

procedure TGpClassList.SetCapacity(const value: integer);
begin
  clClasses.Capacity := value;
end; { TGpClassList.SetCapacity }

{$IFDEF GpLists_Enumerators}

{$IFDEF MSWINDOWS}
{ TGpObjectRingBufferEnumerator }

constructor TGpObjectRingBufferEnumerator.Create(ringBuffer: TGpObjectRingBuffer);
begin
  inherited Create;
  rbeRingBuffer := ringBuffer;
  rbeRingBuffer.Lock;
  rbeIndex := -1;
end; { TGpObjectRingBufferEnumerator.Create }

destructor TGpObjectRingBufferEnumerator.Destroy;
begin
  rbeRingBuffer.Unlock;
  inherited;
end; { TGpObjectRingBufferEnumerator.Destroy }

function TGpObjectRingBufferEnumerator.GetCurrent: TObject;
begin
  Result := rbeRingBuffer[rbeIndex];
end; { TGpObjectRingBufferEnumerator.GetCurrent }

function TGpObjectRingBufferEnumerator.MoveNext: boolean;
begin
  Result := rbeIndex < (rbeRingBuffer.Count - 1);
  if Result then
    Inc(rbeIndex);
end; { TGpObjectRingBufferEnumerator.MoveNext }
{$ENDIF GpLists_Enumerators}
{$ENDIF}

{$IFDEF MSWINDOWS}
{ TGpObjectRingBuffer }

constructor TGpObjectRingBuffer.Create(bufferSize: integer; ownsObjects,
  multithreaded: boolean);
begin
  if bufferSize <= 0 then
    raise Exception.Create('TGpObjectRingBuffer.Create: invalid buffer size is');
  if multithreaded then
    orbLock := TCriticalSection.Create;
  orbBufferSize := bufferSize;
  orbOwnsObjects := ownsObjects;
  SetLength(orbBuffer, orbBufferSize+1);
end; { TGpObjectRingBuffer.Create }

{:Destroys ring buffer. If OwnsObjects is set, destroys all objects currently
  in the buffer.
  @since   2003-07-26
}
destructor TGpObjectRingBuffer.Destroy;
begin
  BufferAlmostEmptyEvent := 0;
  BufferAlmostFullEvent := 0;
  Clear;
  FreeAndNil(orbLock);
  inherited;
end; { TGpObjectRingBuffer.Destroy }

procedure TGpObjectRingBuffer.Clear;
begin
  Lock;
  try
    if orbOwnsObjects then begin
      while not IsEmpty do
        Dequeue.Free;
    end
    else
      orbTail := orbHead;
    orbCount := 0;
  finally Unlock; end;
end; { TGpObjectRingBuffer.Clear }

{:Returns number of objects in the buffer.
  @since   2003-07-26
}
function TGpObjectRingBuffer.Count: integer;
begin
  Result := orbCount;
end; { TGpObjectRingBuffer.Count }

class function TGpObjectRingBuffer.CreateInterface(bufferSize: integer;
  ownsObjects: boolean; multithreaded: boolean): IGpObjectRingBuffer;
begin
  Result := TGpObjectRingBuffer.Create(bufferSize, ownsObjects, multithreaded);
end; { TGpObjectRingBuffer.CreateInterface }

{:Removes tail object from the buffer, without destroying it. Returns nil if
  buffer is empty.
  @since   2003-07-26
}
function TGpObjectRingBuffer.Dequeue: TObject;
begin
  Lock;
  try
    Result := InternalDequeue;
  finally Unlock; end;
end; { TGpObjectRingBuffer.Dequeue }

{:Inserts object into the buffer. Returns false if the buffer is full.
  @since   2003-07-26
}
function TGpObjectRingBuffer.Enqueue(obj: TObject): boolean;
begin
  Lock;
  try
    if InternalIsFull then
      Result := false
    else begin
      orbBuffer[orbHead] := obj;
      orbHead := IncPointer(orbHead);
      Inc(orbCount);
      if (BufferAlmostFullEvent <> 0) and (BufferAlmostFullThreshold = orbCount) then
        Win32Check(SetEvent(BufferAlmostFullEvent));
      Result := true;
    end;
  finally Unlock; end;
end; { TGpObjectRingBuffer.Enqueue }

function TGpObjectRingBuffer.GetBufferAlmostEmptyEvent: THandle;
begin
  Result := orbBufferAlmostEmptyEvent;
end; { TGpObjectRingBuffer.GetBufferAlmostEmptyEvent }

function TGpObjectRingBuffer.GetBufferAlmostEmptyThreshold: integer;
begin
  Result := orbBufferAlmostEmptyThreshold;
end; { TGpObjectRingBuffer.GetBufferAlmostEmptyThreshold }

function TGpObjectRingBuffer.GetBufferAlmostFullEvent: THandle;
begin
  Result := orbBufferAlmostFullEvent;
end; { TGpObjectRingBuffer.GetBufferAlmostFullEvent }

function TGpObjectRingBuffer.GetBufferAlmostFullThreshold: integer;
begin
  Result := orbBufferAlmostFullThreshold;
end; { TGpObjectRingBuffer.GetBufferAlmostFullThreshold }

{$IFDEF GpLists_Enumerators}
function TGpObjectRingBuffer.GetEnumerator: TGpObjectRingBufferEnumerator;
begin
  Result := TGpObjectRingBufferEnumerator.Create(Self);
end; { TGpObjectRingBuffer.GetEnumerator }
{$ENDIF GpLists_Enumerators}

function TGpObjectRingBuffer.GetItem(iObject: integer): TObject;
begin
  if (iObject < 0) or (iObject >= Count) then
    raise Exception.CreateFmt('TGpObjectRingBuffer.GetItem: Invalid index %d', [iObject])
  else
    Result := orbBuffer[IncPointer(orbTail, iObject)];
end; { TGpObjectRingBuffer.GetItem }

function TGpObjectRingBuffer.GetOwnsObjects: boolean;
begin
  Result := orbOwnsObjects;
end; { TGpObjectRingBuffer.GetOwnsObjects }

{:Returns head (newest) object or nil if the buffer is empty.
  @since   2003-07-26
}
function TGpObjectRingBuffer.Head: TObject;
begin
  Lock;
  try
    if IsEmpty then
      Result := nil
    else
      Result := orbBuffer[IncPointer(orbTail, Count-1)];
  finally Unlock; end;
end; { TGpObjectRingBuffer.Head }

{:Increments internal pointer (head or tail), wraps it to the buffer size and
  returns new value.
  @since   2003-07-26
}
function TGpObjectRingBuffer.IncPointer(const ptr: integer;
  increment: integer): integer;
begin
  Result := (ptr + increment) mod (orbBufferSize + 1);
end; { TGpObjectRingBuffer.IncPointer }

function TGpObjectRingBuffer.InternalIsFull: boolean;
begin
  Result := (IncPointer(orbHead) = orbTail);
end; { TGpObjectRingBuffer.InternalIsFull }

function TGpObjectRingBuffer.InternalDequeue: TObject;
begin
  if IsEmpty then
    Result := nil
  else begin
    Result := orbBuffer[orbTail];
    orbTail := IncPointer(orbTail);
    Dec(orbCount);
    if (BufferAlmostEmptyEvent <> 0) and (BufferAlmostEmptyThreshold = orbCount) then
      Win32Check(SetEvent(BufferAlmostEmptyEvent));
  end;
end; { TGpObjectRingBuffer.InternalDequeue }

{:Checks whether the buffer is empty.
  @since   2003-07-26
}
function TGpObjectRingBuffer.IsEmpty: boolean;
begin
  Result := (orbCount = 0);
end; { TGpObjectRingBuffer.IsEmpty }

{:Checks whether the buffer is full.
  @since   2003-07-26
}
function TGpObjectRingBuffer.IsFull: boolean;
begin
  Lock;
  try
    Result := InternalIsFull;
  finally Unlock; end;
end; { TGpObjectRingBuffer.IsFull }

procedure TGpObjectRingBuffer.Lock;
begin
  if assigned(orbLock) then
    orbLock.Acquire;
end; { TGpObjectRingBuffer.Lock }

function TGpObjectRingBuffer.Remove(obj: TObject): TObject;
var
  iObj: integer;
begin
  Lock;
  try
    for iObj := 0 to Count - 1 do begin
      Result := InternalDequeue;
      if Result = obj then
        Exit;
      Enqueue(Result);
    end;
  finally Unlock; end;
  Result := nil;
end; { TGpObjectRingBuffer.Remove }

procedure TGpObjectRingBuffer.SetBufferAlmostEmptyEvent(const value: THandle);
begin
  orbBufferAlmostEmptyEvent := value;
end; { TGpObjectRingBuffer.SetBufferAlmostEmptyEvent }

procedure TGpObjectRingBuffer.SetBufferAlmostEmptyThreshold(const value: integer);
begin
  orbBufferAlmostEmptyThreshold := value;
end; { TGpObjectRingBuffer.SetBufferAlmostEmptyThreshold }

procedure TGpObjectRingBuffer.SetBufferAlmostFullEvent(const value: THandle);
begin
  orbBufferAlmostFullEvent := value;
end; { TGpObjectRingBuffer.SetBufferAlmostFullEvent }

procedure TGpObjectRingBuffer.SetBufferAlmostFullThreshold(const value: integer);
begin
  orbBufferAlmostFullThreshold := value;
end; { TGpObjectRingBuffer.SetBufferAlmostFullThreshold }

procedure TGpObjectRingBuffer.SetItem(iObject: integer; const value: TObject);
var
  idxObject: integer;
begin
  if (iObject < 0) or (iObject >= Count) then
    raise Exception.CreateFmt('TGpObjectRingBuffer.SetItem: Invalid index %d', [iObject])
  else begin
    idxObject := IncPointer(orbTail, iObject);
    if orbOwnsObjects then
      orbBuffer[idxObject].Free;
    orbBuffer[idxObject] := value;
  end;
end; { TGpObjectRingBuffer.SetItem }

{:Returns tail (oldest) object or nil if the buffer is empty.
  @since   2003-07-26
}
function TGpObjectRingBuffer.Tail: TObject;
begin
  Lock;
  try
    if IsEmpty then
      Result := nil
    else
      Result := orbBuffer[orbTail];
  finally Unlock; end;
end; { TGpObjectRingBuffer.Tail }

procedure TGpObjectRingBuffer.Unlock;
begin
  if assigned(orbLock) then
    orbLock.Release;
end; { TGpObjectRingBuffer.Unlock }

{$IFDEF Unicode}
{$IFNDEF GpLists_LimitedGenerics}
{ TGpRingBuffer<T> }

constructor TGpRingBuffer<T>.Create(bufferSize: integer; multithreaded: boolean);
begin
  if bufferSize <= 0 then
    raise Exception.Create('TGpRingBuffer<T>.Create: invalid buffer size');
  if multithreaded then
    orbLock := TCriticalSection.Create;
  orbBufferSize := bufferSize;
  SetLength(orbBuffer, orbBufferSize+1);
end; { TGpRingBuffer<T>.Create }

{:Destroys ring buffer.
}
destructor TGpRingBuffer<T>.Destroy;
begin
  BufferAlmostEmptyEvent := 0;
  BufferAlmostFullEvent := 0;
  Clear;
  FreeAndNil(orbLock);
  inherited;
end; { TGpRingBuffer<T>.Destroy }

procedure TGpRingBuffer<T>.Clear;
begin
  Lock;
  try
    while not IsEmpty do
      Dequeue;
    orbCount := 0;
  finally Unlock; end;
end; { TGpRingBuffer<T>.Clear }

{:Returns number of objects in the buffer.
}
function TGpRingBuffer<T>.Count: integer;
begin
  Result := orbCount;
end; { TGpRingBuffer<T>.Count }

class function TGpRingBuffer<T>.CreateInterface(bufferSize: integer;
  multithreaded: boolean): IGpRingBuffer<T>;
begin
  Result := TGpRingBuffer<T>.Create(bufferSize, multithreaded);
end; { TGpRingBuffer<T>.CreateInterface }

{:Removes tail object from the buffer, without destroying it. Returns nil if
  buffer is empty.
}
function TGpRingBuffer<T>.Dequeue: T;
begin
  if not Dequeue(Result) then
    Result := Default(T);
end; { TGpRingBuffer<T>.Dequeue }

function TGpRingBuffer<T>.Dequeue(var item: T): boolean;
begin
  Lock;
  try
    Result := InternalDequeue(item);
  finally Unlock; end;
end; { TGpRingBuffer }

{:Inserts object into the buffer. Returns false if the buffer is full.
  @since   2003-07-26
}
function TGpRingBuffer<T>.Enqueue(item: T): boolean;
begin
  Lock;
  try
    if InternalIsFull then
      Result := false
    else begin
      orbBuffer[orbHead] := item;
      orbHead := IncPointer(orbHead);
      Inc(orbCount);
      if (BufferAlmostFullEvent <> 0) and (BufferAlmostFullThreshold = orbCount) then
        Win32Check(SetEvent(BufferAlmostFullEvent));
      Result := true;
    end;
  finally Unlock; end;
end; { TGpRingBuffer<T>.Enqueue }

function TGpRingBuffer<T>.GetBufferAlmostEmptyEvent: THandle;
begin
  Result := orbBufferAlmostEmptyEvent;
end; { TGpRingBuffer<T>.GetBufferAlmostEmptyEvent }

function TGpRingBuffer<T>.GetBufferAlmostEmptyThreshold: integer;
begin
  Result := orbBufferAlmostEmptyThreshold;
end; { TGpRingBuffer<T>.GetBufferAlmostEmptyThreshold }

function TGpRingBuffer<T>.GetBufferAlmostFullEvent: THandle;
begin
  Result := orbBufferAlmostFullEvent;
end; { TGpRingBuffer<T>.GetBufferAlmostFullEvent }

function TGpRingBuffer<T>.GetBufferAlmostFullThreshold: integer;
begin
  Result := orbBufferAlmostFullThreshold;
end; { TGpRingBuffer<T>.GetBufferAlmostFullThreshold }

function TGpRingBuffer<T>.GetItem(iItem: integer): T;
begin
  if (iItem < 0) or (iItem >= Count) then
    raise Exception.CreateFmt('TGpObjectRingBuffer.GetItem: Invalid index %d', [iItem])
  else
    Result := orbBuffer[IncPointer(orbTail, iItem)];
end; { TGpRingBuffer<T>.GetItem }

{:Returns head (newest) item or nil if the buffer is empty.
  @since   2003-07-26
}
function TGpRingBuffer<T>.Head: T;
begin
  Head(Result);
end; { TGpRingBuffer<T>.Head }

function TGpRingBuffer<T>.Head(var item: T): boolean;
begin
  Lock;
  try
    Result := not IsEmpty;
    if Result then
      item := orbBuffer[IncPointer(orbTail, Count-1)];
  finally Unlock; end;
end; { TGpRingBuffer }

{:Increments internal pointer (head or tail), wraps it to the buffer size and
  returns new value.
  @since   2003-07-26
}
function TGpRingBuffer<T>.IncPointer(const ptr: integer;
  increment: integer): integer;
begin
  Result := (ptr + increment) mod (orbBufferSize + 1);
end; { TGpRingBuffer<T>.IncPointer }

function TGpRingBuffer<T>.InternalIsFull: boolean;
begin
  Result := (IncPointer(orbHead) = orbTail);
end; { TGpRingBuffer<T>.InternalIsFull }

function TGpRingBuffer<T>.InternalDequeue(var item: T): boolean;
begin
  Result := not IsEmpty;
  if Result then begin
    item := orbBuffer[orbTail];
    orbTail := IncPointer(orbTail);
    Dec(orbCount);
    if (BufferAlmostEmptyEvent <> 0) and (BufferAlmostEmptyThreshold = orbCount) then
      Win32Check(SetEvent(BufferAlmostEmptyEvent));
  end;
end; { TGpRingBuffer<T>.InternalDequeue }

{:Checks whether the buffer is empty.
  @since   2003-07-26
}
function TGpRingBuffer<T>.IsEmpty: boolean;
begin
  Result := (orbCount = 0);
end; { TGpRingBuffer<T>.IsEmpty }

{:Checks whether the buffer is full.
  @since   2003-07-26
}
function TGpRingBuffer<T>.IsFull: boolean;
begin
  Lock;
  try
    Result := InternalIsFull;
  finally Unlock; end;
end; { TGpRingBuffer<T>.IsFull }

procedure TGpRingBuffer<T>.Lock;
begin
  if assigned(orbLock) then
    orbLock.Acquire;
end; { TGpRingBuffer<T>.Lock }

procedure TGpRingBuffer<T>.SetBufferAlmostEmptyEvent(const value: THandle);
begin
  orbBufferAlmostEmptyEvent := value;
end; { TGpRingBuffer<T>.SetBufferAlmostEmptyEvent }

procedure TGpRingBuffer<T>.SetBufferAlmostEmptyThreshold(const value: integer);
begin
  orbBufferAlmostEmptyThreshold := value;
end; { TGpRingBuffer<T>.SetBufferAlmostEmptyThreshold }

procedure TGpRingBuffer<T>.SetBufferAlmostFullEvent(const value: THandle);
begin
  orbBufferAlmostFullEvent := value;
end; { TGpRingBuffer<T>.SetBufferAlmostFullEvent }

procedure TGpRingBuffer<T>.SetBufferAlmostFullThreshold(const value: integer);
begin
  orbBufferAlmostFullThreshold := value;
end; { TGpRingBuffer<T>.SetBufferAlmostFullThreshold }

procedure TGpRingBuffer<T>.SetItem(iItem: integer; const value: T);
var
  idxObject: integer;
begin
  if (iItem < 0) or (iItem >= Count) then
    raise Exception.CreateFmt('TGpObjectRingBuffer.SetItem: Invalid index %d', [iItem])
  else begin
    idxObject := IncPointer(orbTail, iItem);
    orbBuffer[idxObject] := value;
  end;
end; { TGpRingBuffer<T>.SetItem }

{:Returns tail (oldest) object or nil if the buffer is empty.
}
function TGpRingBuffer<T>.Tail: T;
begin
  Tail(Result);
end; { TGpRingBuffer<T>.Tail }

function TGpRingBuffer<T>.Tail(var item: T): boolean;
begin
  Lock;
  try
    Result := not IsEmpty;
    if Result then
      item := orbBuffer[orbTail];
  finally Unlock; end;
end; { TGpRingBuffer }

procedure TGpRingBuffer<T>.Unlock;
begin
  if assigned(orbLock) then
    orbLock.Release;
end; { TGpRingBuffer<T>.Unlock }
{$ENDIF ~GpLists_LimitedGenerics}
{$ENDIF Unicode}
{$ENDIF}

{ TGpObjectMap }

constructor TGpObjectMap.Create(ownsObjects: boolean);
begin
  inherited Create;
  omList := TGpInt64ObjectList.Create(ownsObjects);
end; { TGpObjectMap.Create }

destructor TGpObjectMap.Destroy;
begin
  FreeAndNil(omList);
  inherited;
end; { TGpObjectMap.Destroy }

procedure TGpObjectMap.Clear;
begin
  omList.Clear;
end; { TGpObjectMap.Clear }

function TGpObjectMap.Count: integer;
begin
  Result := omList.Count;
end; { TGpObjectMap.Count }

class function TGpObjectMap.CreateInterface(ownsObjects: boolean): IGpObjectMap;
begin
  Result := TGpObjectMap.Create(ownsObjects);
end; { TGpObjectMap.CreateInterface }

function TGpObjectMap.Exists(value: TObject; compareFunc: TGpObjectMapCompare): boolean;
var
  item: TObject;
begin
  Find(value, compareFunc, item);
  Result := assigned(item);
end; { TGpObjectMap.Exists }

procedure TGpObjectMap.Find(value: TObject; compareFunc: TGpObjectMapCompare;
  var item: TObject);
var
  iItem: integer;
begin
  for iItem := 0 to Count-1 do begin
    if compareFunc(value, omList.Objects[iItem]) then begin
      item := TObject(omList[iItem]);
      Exit;
    end;
  end; //for
  item := nil;
end; { TGpObjectMap.Find }

function TGpObjectMap.GetIndexedItem(idxItem: integer): TObject;
begin
  Result := TObject(omList[idxItem]); 
end; { TGpObjectMap.GetIndexedItem }

function TGpObjectMap.GetIndexedValue(idxValue: integer): TObject;
begin
  Result := omList.Objects[idxValue];
end; { TGpObjectMap.GetIndexedObject }

function TGpObjectMap.GetItems(item: TObject): TObject;
var
  idxItem: integer;
begin
  idxItem := omList.IndexOf(NativeUInt(item));
  if idxItem >= 0 then
    Result := omList.Objects[idxItem]
  else
    Result := nil;
end; { TGpObjectMap.GetItems }

procedure TGpObjectMap.SetItems(item: TObject; const value: TObject);
var
  idxItem: integer;
begin
  idxItem := omList.IndexOf(NativeUint(item));
  if idxItem >= 0 then begin
    if assigned(value) then
      omList.Objects[idxItem] := value
    else
      omList.Delete(idxItem);
  end
  else
    omList.AddObject(NativeUInt(item), value);
end; { TGpObjectMap.SetItems }

constructor TGpObjectObjectMap.Create(ownsObjects: boolean);
begin
  inherited Create;
  oomMap := TGpObjectMap.Create(true);
  oomOwnsObjects := ownsObjects;
end; { TGpObjectObjectMap.Create }

destructor TGpObjectObjectMap.Destroy;
begin
  FreeAndNil(oomMap);
  inherited;
end; { TGpObjectObjectMap.Destroy }

{ TGpObjectObjectMap }

function MapCompare(userValue, mapValue: TObject): boolean; {$IFDEF GpLists_Inline}inline;{$ENDIF}
begin
  with TGpObjectObjectMap(userValue) do begin
    TGpObjectMap(mapValue).Find(oomFindValue, oomCompareFunc, oomItem2);
    Result := assigned(oomItem2);
  end; //with
end; { MapCompare }

procedure TGpObjectObjectMap.Clear;
begin
  oomMap.Clear;
end; { TGpObjectObjectMap.Clear }

class function TGpObjectObjectMap.CreateInterface(ownsObjects: boolean):
  IGpObjectObjectMap;
begin
  Result := TGpObjectObjectMap.Create(ownsObjects);
end; { TGpObjectObjectMap.CreateInterface }

function TGpObjectObjectMap.Exists(value: TObject; compareFunc: TGpObjectMapCompare): boolean;
var
  item1: TObject;
  item2: TObject;
begin
  Find(value, compareFunc, item1, item2);
  Result := assigned(item1) and assigned(item2);
end; { TGpObjectObjectMap.Exists }

procedure TGpObjectObjectMap.Find(value: TObject; compareFunc: TGpObjectMapCompare;
  var item1, item2: TObject);
begin
  oomFindValue := value;
  oomCompareFunc := compareFunc;
  oomMap.Find(Self, MapCompare, item1);
  item2 := oomItem2;
end; { TGpObjectObjectMap.Find }

function TGpObjectObjectMap.GetItems(item1, item2: TObject): TObject;
begin
  Result := Map(item1)[item2];
end; { TGpObjectObjectMap.GetItems }

function TGpObjectObjectMap.Map(item: TObject): TGpObjectMap;
begin
  Result := TGpObjectMap(oomMap[item]);
  if not assigned(Result) then begin
    Result := TGpObjectMap.Create(oomOwnsObjects);
    oomMap[item] := Result;
  end;
end; { TGpObjectObjectMap.Map }

procedure TGpObjectObjectMap.SetItems(item1, item2: TObject; const value: TObject);
begin
  Map(item1)[item2] := value;
end; { TGpObjectObjectMap.SetItems }

{ TGpDoublyLinkedListObject }

{:Unlinks the object from the list and destroys it.
  @since   2003-10-28
}        
destructor TGpDoublyLinkedListObject.Destroy;
begin
  Unlink;
  inherited;
end; { TGpDoublyLinkedListObject.Destroy }

{:Inserts self into the doubly-linked list after the specified object.
  @since   2003-10-28
}
procedure TGpDoublyLinkedListObject.LinkAfter(list: TGpDoublyLinkedList;
  obj: TGpDoublyLinkedListObject);
begin
  Unlink;
  list.Lock;
  try
    dlloNext := obj.dlloNext;
    dlloNext.dlloPrevious := Self;
    dlloPrevious := obj;
    dlloPrevious.dlloNext := Self;
    dlloList := list;
    dlloList.Linking(Self);
  finally list.Unlock; end;
end; { TGpDoublyLinkedListObject.LinkAfter }

{:Returns the next object in the list or nil if next object is the tail
  sentinel.
  @since   2003-10-27
}
function TGpDoublyLinkedListObject.Next: TGpDoublyLinkedListObject;
begin
  dlloList.Lock;
  try
    Result := NextUnsafe;
  finally dlloList.Unlock; end;
end; { TGpDoublyLinkedListObject.Next }

function TGpDoublyLinkedListObject.NextUnsafe: TGpDoublyLinkedListObject;
begin
  Result := dlloNext;
  if Result.dlloNext = nil then // we are at the tail sentinel
    Result := nil;
end; { TGpDoublyLinkedListObject.NextUnsafe }

{:Returns the previous object in the list or nil if next object is the tail
  sentinel.
  @since   2003-10-27
}
function TGpDoublyLinkedListObject.Previous: TGpDoublyLinkedListObject;
begin
  dlloList.Lock;
  try
    Result := PreviousUnsafe;
  finally dlloList.Unlock; end;
end; { TGpDoublyLinkedListObject.Previous }

function TGpDoublyLinkedListObject.PreviousUnsafe: TGpDoublyLinkedListObject;
begin
  Result := dlloPrevious;
  if Result.dlloPrevious = nil then // we are at the head sentinel
    Result := nil;
end; { TGpDoublyLinkedListObject.PreviousUnsafe }

{:Unlinks the object from the list.
  @since   2003-10-28
}
procedure TGpDoublyLinkedListObject.Unlink;
begin
  if assigned(dlloList) then begin
    dlloList.Lock;
    try
      if assigned(dlloNext) then
        dlloNext.dlloPrevious := dlloPrevious;
      if assigned(dlloPrevious) then
        dlloPrevious.dlloNext := dlloNext;
      dlloList.Unlinking(Self);
    finally dlloList.Unlock; end;
  end;
  dlloList := nil;
  dlloNext := nil;
  dlloPrevious := nil;
end; { TGpDoublyLinkedListObject.Unlink }

{$IFDEF GpLists_Enumerators}

{ TGpDoublyLinkedListEnumerator }

constructor TGpDoublyLinkedListEnumerator.Create(dlList: TGpDoublyLinkedList);
begin
  inherited Create;
  dlleList := dlList;
  dlleList.Lock;
  dlleElement := nil;
end; { TGpDoublyLinkedListEnumerator.Create }

destructor TGpDoublyLinkedListEnumerator.Destroy;
begin
  dlleList.Unlock;
  inherited;
end; { TGpDoublyLinkedListEnumerator.Destroy }

function TGpDoublyLinkedListEnumerator.GetCurrent: TGpDoublyLinkedListObject;
begin
  Result := dlleElement;
end; { TGpDoublyLinkedListEnumerator.GetCurrent }

function TGpDoublyLinkedListEnumerator.MoveNext: boolean;
begin
  if not assigned(dlleElement) then
    dlleElement := dlleList.HeadUnsafe
  else
    dlleElement := dlleElement.NextUnsafe;
  Result := assigned(dlleElement);
end; { TGpDoublyLinkedListEnumerator.MoveNext }

{$IFDEF Unicode}
{$IFNDEF GpLists_LimitedGenerics}
constructor TGpDoublyLinkedListEnumerator<T>.Create(dlList: TGpDoublyLinkedList);
begin
  inherited Create;
  dlleEnumerator := TGpDoublyLinkedListEnumerator.Create(dlList);
end; { TGpDoublyLinkedListEnumerator<T>.Create }

destructor TGpDoublyLinkedListEnumerator<T>.Destroy;
begin
  FreeAndNil(dlleEnumerator);
  inherited;
end; { TGpDoublyLinkedListEnumerator<T>.Destroy }

function TGpDoublyLinkedListEnumerator<T>.GetCurrent: T;
begin
  Result := T(dlleEnumerator.GetCurrent);
end; { TGpDoublyLinkedListEnumerator<T>.GetCurrent }

function TGpDoublyLinkedListEnumerator<T>.MoveNext: boolean;
begin
  Result := dlleEnumerator.MoveNext;
end; { TGpDoublyLinkedListEnumerator<T>.MoveNext }

constructor TGpDoublyLinkedListEnumeratorFactory<T>.Create(dlList: TGpDoublyLinkedList);
begin
  inherited Create;
  dllefList := dlList;
end; { TGpDoublyLinkedListEnumeratorFactory<T>.Create }

function TGpDoublyLinkedListEnumeratorFactory<T>.GetEnumerator:
  TGpDoublyLinkedListEnumerator<T>;
begin
  Result := TGpDoublyLinkedListEnumerator<T>.Create(dllefList);
end; { TGpDoublyLinkedListEnumeratorFactory<T>.GetEnumerator }
{$ENDIF ~GpLists_LimitedGenerics}
{$ENDIF Unicode}

{$ENDIF GpLists_Enumerators}

{ TGpDoublyLinkedList }

constructor TGpDoublyLinkedList.Create(multithreaded: boolean);
begin
  inherited Create;
  if multithreaded then
    dllLock := TCriticalSection.Create;
  dllHead := TGpDoublyLinkedListObject.Create;
  dllTail := TGpDoublyLinkedListObject.Create;
  dllHead.dlloNext := dllTail;
  dllTail.dlloPrevious := dllHead;
end; { TGpDoublyLinkedList.Create }

destructor TGpDoublyLinkedList.Destroy;
begin
  FreeAndNil(dllHead);
  FreeAndNil(dllTail);
  FreeAndNil(dllLock);
  inherited;
end; { TGpDoublyLinkedList.Destroy }

{:Returns number of items in the list.
  @since   2003-10-28
}
function TGpDoublyLinkedList.Count: integer;
begin
  Result := dllCount;
end; { TGpDoublyLinkedList.Count }

class function TGpDoublyLinkedList.CreateInterface(multithreaded: boolean):
  IGpDoublyLinkedList;
begin
  Result := TGpDoublyLinkedList.Create(multithreaded);
end; { TGpDoublyLinkedList.CreateInterface }

{$IFDEF GpLists_Enumerators}
{$IFDEF Unicode}
{$IFNDEF GpLists_LimitedGenerics}
function TGpDoublyLinkedList.EnumerateAs<T>: TGpDoublyLinkedListEnumeratorFactory<T>;
begin
  Result := TGpDoublyLinkedListEnumeratorFactory<T>.Create(Self);
end; { TGpDoublyLinkedList.EnumerateAs<T> }
{$ENDIF}
{$ENDIF}
{$ENDIF}

{:Destroy all elements of the list.
  @since   2005-06-02
}
procedure TGpDoublyLinkedList.FreeAll;
begin
  if assigned(dllLock) then dllLock.Acquire;
  try
    while TailUnsafe <> nil do
      TailUnsafe.Free;
  finally if assigned(dllLock) then dllLock.Release; end;
end; { TGpDoublyLinkedList.FreeAll }

{$IFDEF GpLists_Enumerators}
function TGpDoublyLinkedList.GetEnumerator: TGpDoublyLinkedListEnumerator;
begin
  Result := TGpDoublyLinkedListEnumerator.Create(Self);
end; { TGpDoublyLinkedList.GetEnumerator }
{$ENDIF GpLists_Enumerators}

{:Returns first element in the list or nil if list is empty.
  @since   2003-10-28
}
function TGpDoublyLinkedList.Head: TGpDoublyLinkedListObject;
begin
  if assigned(dllLock) then dllLock.Acquire;
  try
    Result := HeadUnsafe;
  finally if assigned(dllLock) then dllLock.Release; end;
end; { TGpDoublyLinkedList.Head }

{:Returns first element in the list or nil if list is empty.
  @since   2003-10-28
}
function TGpDoublyLinkedList.HeadUnsafe: TGpDoublyLinkedListObject;
begin
  if IsEmpty then
    Result := nil
  else
    Result := dllHead.dlloNext;
end; { TGpDoublyLinkedList.HeadUnsafe }

{:Inserts the object into the list after the existing linked object.
  @since   2003-10-28
}        
procedure TGpDoublyLinkedList.InsertAfter(existingObject,
  obj: TGpDoublyLinkedListObject);
begin
  if assigned(dllLock) then dllLock.Acquire;
  try
    obj.LinkAfter(Self, existingObject);
  finally if assigned(dllLock) then dllLock.Release; end;
end; { TGpDoublyLinkedList.InsertAfter }

procedure TGpDoublyLinkedList.InsertAtHead(obj: TGpDoublyLinkedListObject);
begin
  if assigned(dllLock) then dllLock.Acquire;
  try
    InsertAfter(dllHead, obj);
  finally if assigned(dllLock) then dllLock.Release; end;
end; { TGpDoublyLinkedList.InsertAtHead }

procedure TGpDoublyLinkedList.InsertAtHeadUnsafe(obj: TGpDoublyLinkedListObject);
begin
  InsertAfter(dllHead, obj);
end; { TGpDoublyLinkedList.InsertAtHeadUnsafe }

procedure TGpDoublyLinkedList.InsertAtTail(obj: TGpDoublyLinkedListObject);
begin
  if assigned(dllLock) then dllLock.Acquire;
  try
    InsertBefore(dllTail, obj);
  finally if assigned(dllLock) then dllLock.Release; end;
end; { TGpDoublyLinkedList.InsertAtTail }

procedure TGpDoublyLinkedList.InsertAtTailUnsafe(obj: TGpDoublyLinkedListObject);
begin
  InsertBefore(dllTail, obj);
end; { TGpDoublyLinkedList.InsertAtTailUnsafe }

procedure TGpDoublyLinkedList.InsertBefore(existingObject,
  obj: TGpDoublyLinkedListObject);
begin
  InsertAfter(existingObject.dlloPrevious, obj);
end; { TGpDoublyLinkedList.InsertBefore }

function TGpDoublyLinkedList.IsEmpty: boolean;
begin
  if assigned(dllLock) then dllLock.Acquire;
  try
    Result := (dllHead.dlloNext = dllTail);
  finally if assigned(dllLock) then dllLock.Release; end;
end; { TGpDoublyLinkedList.IsEmpty }

{:Called from the linked object when it is being linked from the list.
  @since   2003-10-28
}
procedure TGpDoublyLinkedList.Linking(obj: TGpDoublyLinkedListObject);
begin
  Inc(dllCount);
end; { TGpDoublyLinkedList.Linking }

procedure TGpDoublyLinkedList.Lock;
begin
  if assigned(dllLock) then
    dllLock.Acquire;
end; { TGpDoublyLinkedList.Lock }

function TGpDoublyLinkedList.Next(obj: TGpDoublyLinkedListObject):
  TGpDoublyLinkedListObject;
begin
  Result := obj.Next;
end; { TGpDoublyLinkedList.Next }

function TGpDoublyLinkedList.Previous(obj: TGpDoublyLinkedListObject):
  TGpDoublyLinkedListObject;
begin
  Result := obj.Previous;
end; { TGpDoublyLinkedList.Previous }

function TGpDoublyLinkedList.RemoveFromHead: TGpDoublyLinkedListObject;
begin
  if assigned(dllLock) then dllLock.Acquire;
  try
    Result := HeadUnsafe;
    if assigned(Result) then
      Result.Unlink;
  finally if assigned(dllLock) then dllLock.Release; end;
end; { TGpDoublyLinkedList.RemoveFromHead }

function TGpDoublyLinkedList.RemoveFromTail: TGpDoublyLinkedListObject;
begin
  if assigned(dllLock) then dllLock.Acquire;
  try
    Result := TailUnsafe;
    if assigned(Result) then
      Result.Unlink;
  finally if assigned(dllLock) then dllLock.Release; end;
end; { TGpDoublyLinkedList.RemoveFromTail }

function TGpDoublyLinkedList.Tail: TGpDoublyLinkedListObject;
begin
  if assigned(dllLock) then dllLock.Acquire;
  try
    Result := TailUnsafe;
  finally if assigned(dllLock) then dllLock.Release; end;
end; { TGpDoublyLinkedList.Tail }

function TGpDoublyLinkedList.TailUnsafe: TGpDoublyLinkedListObject;
begin
  if IsEmpty then
    Result := nil
  else
    Result := dllTail.dlloPrevious;
end; { TGpDoublyLinkedList.TailUnsafe }

procedure TGpDoublyLinkedList.Unlink(obj: TGpDoublyLinkedListObject);
begin
  obj.Unlink;
end; { TGpDoublyLinkedList.Unlink }

{:Remove all elements from the list without destroying them.
  @since   2005-06-02
}
procedure TGpDoublyLinkedList.UnlinkAll;
begin
  if assigned(dllLock) then dllLock.Acquire;
  try
    while TailUnsafe <> nil do
      RemoveFromTail;
  finally if assigned(dllLock) then dllLock.Release; end;
end; { TGpDoublyLinkedList.UnlinkAll }

{:Called from the linked object when it is being unliniked from the list.
  @since   2003-10-28
}
procedure TGpDoublyLinkedList.Unlinking(obj: TGpDoublyLinkedListObject);
begin
  Dec(dllCount);
end; { TGpDoublyLinkedList.Unlinking }

procedure TGpDoublyLinkedList.Unlock;
begin
  if assigned(dllLock) then
    dllLock.Release;
end; { TGpDoublyLinkedList.Unlock }

{ TFifoBlock }

{$IFDEF VER150}
constructor TFifoBlock.CreateD7(const buf; bufSize: integer; memoryEventSender: TObject;
  onGetMem, onFreeMem: TGpFifoMemoryEvent);
{$ELSE}
constructor TFifoBlock.Create(const buf; bufSize: integer; memoryEventSender: TObject;
  onGetMem, onFreeMem: TGpFifoMemoryEvent);
{$ENDIF}
begin
  inherited Create;
  if assigned(onGetMem) then
    onGetMem(memoryEventSender, bufSize, FData)
  else
    GetMem(FData, bufSize);
  Move(buf, FData^, bufSize);
  FDataSize := bufSize;
  FMemEventSender := memoryEventSender;
  FOnGetMem := onGetMem;
  FOnFreeMem := onFreeMem;
end; { TFifoBlock.Create }

constructor TFifoBlock.Create(const data: TStream; bufSize: integer;
  memoryEventSender: TObject; onGetMem, onFreeMem: TGpFifoMemoryEvent);
begin
  inherited Create;
  if assigned(onGetMem) then
    onGetMem(FMemEventSender, bufSize, FData)
  else
    GetMem(FData, bufSize);
  data.Read(FData^, bufSize);
  FDataSize := bufSize;
  FMemEventSender := memoryEventSender;
  FOnGetMem := onGetMem;
  FOnFreeMem := onFreeMem;
end; { TFifoBlock.Create }

destructor TFifoBlock.Destroy;
begin
  if assigned(FOnFreeMem) then
    FOnFreeMem(FMemEventSender, FDataSize, FData)
  else
    FreeMem(FData);
  inherited;
end; { TFifoBlock.Destroy }

procedure TFifoBlock.Reallocate(bufSize: integer);
begin
  if not (assigned(FOnGetMem) or assigned(FOnFreeMem)) then
    ReallocMem(FData, bufSize)
  else begin
    if assigned(FOnFreeMem) then
      FOnFreeMem(FMemEventSender, FDataSize, FData)
    else
      FreeMem(FData);
    if assigned(FOnGetMem) then
      FOnGetMem(FMemEventSender, bufSize, FData)
    else
      GetMem(FData, bufSize);
  end;
  FDataSize := bufSize;
end; { TFifoBlock.Reallocate }

{ TGpFifoBuffer }

constructor TGpFifoBuffer.Create(maxSize: integer; threadSafe: boolean;
  numBlocksToCache: integer);
begin
  inherited Create;
  FMaxSize := maxSize;
  FFifo := TGpDoublyLinkedList.Create(threadSafe);
  FBlocksToCache := numBlocksToCache;
  if FBlocksToCache > 0 then
    FCachedBlocks := TGpDoublyLinkedList.Create(threadSafe);
  FActiveBlock := TMemoryStream.Create;
  FActiveBlockInUse := false;
end; { TGpFifoBuffer.Create }

destructor TGpFifoBuffer.Destroy;
var
  block: TFifoBlock;
begin
  FreeAndNil(FActiveBlock);
  repeat
    block := (FFifo.RemoveFromHead as TFifoBlock);
    if not assigned(block) then
      break; //repeat
    FreeAndNil(block);
  until false;
  FreeAndNil(FFifo);
  if assigned(FCachedBlocks) then begin
    repeat
      block := (FCachedBlocks.RemoveFromHead as TFifoBlock);
      if not assigned(block) then
        break; //repeat
      FreeAndNil(block);
    until false;
    FreeAndNil(FCachedBlocks);
  end;
  inherited Destroy;
end; { TGpFifoBuffer.Destroy }

procedure TGpFifoBuffer.AddBlock(block: TFifoBlock);
begin
  FFifo.InsertAtTail(block);
  Inc(FCurrentSize, block.Size);
  Assert(FCurrentSize <= FMaxSize);
end; { TGpFifoBuffer.AddBlock }

function TGpFifoBuffer.AllocateBlockBuf(const buf; bufSize: integer): TFifoBlock;
begin
  Result := GetCachedBlock(bufSize);
  if assigned(Result) then
    Move(buf, Result.Data^, bufSize)
  else
    {$IFDEF VER150}
    Result := TFifoBlock.CreateD7(buf, Min(bufSize, FifoPlace), Self, FOnGetMem, FOnFreeMem);
    {$ELSE}
    Result := TFifoBlock.Create(buf, Min(bufSize, FifoPlace), Self, FOnGetMem, FOnFreeMem);
    {$ENDIF}
end; { TGpFifoBuffer.AllocateBlockBuf }

function TGpFifoBuffer.AllocateBlockStream(data: TStream; dataSize: integer): TFifoBlock;
begin
  Result := GetCachedBlock(dataSize);
  if assigned(Result) then
    data.Read(Result.Data^, dataSize)
  else
    Result := TFifoBlock.Create(data, dataSize, Self, FOnGetMem, FOnFreeMem);
end; { TGpFifoBuffer.AllocateBlockStream }

procedure TGpFifoBuffer.Clear;
var
  origSize: integer;
begin
  origSize := Size;
  Size := 0;
  Size := origSize;
end; { TGpFifoBuffer.Clear }

class function TGpFifoBuffer.CreateInterface(maxSize: integer; threadSafe: boolean):
  IGpFifoBuffer;
begin
  Result := TGpFifoBuffer.Create(maxSize, threadSafe);
end; { TGpFifoBuffer.CreateInterface }

function TGpFifoBuffer.FifoPlace: integer;
begin
  Result := FMaxSize - FCurrentSize;
  Assert(Result >= 0);
end; { TGpFifoBuffer.FifoPlace }

function TGpFifoBuffer.GetCachedBlock(bufSize: integer): TFifoBlock;
begin
  Result := nil;
  if FBlocksToCache > 0 then begin
    FCachedBlocks.Lock;
    try
      Result := TFifoBlock(FCachedBlocks.HeadUnsafe);
      while assigned(Result) do begin
        if Result.Size = bufSize then
          break; //while
        Result := TFifoBlock(Result.NextUnsafe);
      end; //while
      if not assigned(Result) then begin
        Result := TFifoBlock(FCachedBlocks.HeadUnsafe);
        if assigned(Result) then
          Result.Reallocate(bufSize);
      end;
    finally FCachedBlocks.Unlock; end;
  end;
end; { TGpFifoBuffer.GetCachedBlock }

function TGpFifoBuffer.GetDataSize: integer;
begin
  Result := FCurrentSize;
end; { TGpFifoBuffer.GetDataSize }

function TGpFifoBuffer.GetOnFreeMem: TGpFifoMemoryEvent;
begin
  Result := FOnFreeMem;
end; { TGpFifoBuffer.GetOnFreeMem }

function TGpFifoBuffer.GetOnGetMem: TGpFifoMemoryEvent;
begin
  Result := FOnGetMem;
end; { TGpFifoBuffer.GetOnGetMem }

function TGpFifoBuffer.GetSize: integer;
begin
  Result := FMaxSize;
end; { TGpFifoBuffer.GetSize }

function TGpFifoBuffer.Read(data: TStream; maxSize: integer): integer;
var
  block   : TFifoBlock;
  readSize: integer;
begin
  Result := 0;
  while Result < maxSize do begin
    if FActiveBlockInUse then begin
      readSize := data.CopyFrom(FActiveBlock, Min(FActiveBlock.Size - FActiveBlock.Position, maxSize - Result));
      Dec(FCurrentSize, readSize);
      Inc(Result, readSize);
      if FActiveBlock.Position >= FActiveBlock.Size then
        FActiveBlockInUse := false
      else begin
        Assert(Result = maxSize);
        break; //while
      end;
    end;
    block := (FFifo.RemoveFromHead as TFifoBlock);
    if not assigned(block) then begin
      break;
    end;
    if block.Size > (maxSize - Result) then begin
      FActiveBlockInUse := true;
      FActiveBlock.Position := 0;
      FActiveBlock.Write(block.Data^, block.Size);
      FActiveBlock.Size := FActiveBlock.Position;
      FActiveBlock.Position := 0;
    end
    else begin
      Dec(FCurrentSize, block.Size);
      data.Write(block.Data^, block.Size);
      Inc(Result, block.Size);
    end;
    ReleaseBlock(block);
  end; //while
  Assert(FCurrentSize >= 0);
  Assert((Result = maxSize) or (FCurrentSize = 0));
end; { TGpFifoBuffer.Read }

procedure TGpFifoBuffer.ReleaseBlock(var block: TFifoBlock);
begin
  if FBlocksToCache = 0 then
    FreeAndNil(block)
  else begin
    FCachedBlocks.Lock;
    try
      if FCachedBlocks.Count >= FBlocksToCache then
        FreeAndNil(block)
      else begin
        FCachedBlocks.InsertAtTailUnsafe(block);
        block := nil;
      end;
    finally FCachedBlocks.Unlock; end;
  end;
end; { TGpFifoBuffer.ReleaseBlock }

procedure TGpFifoBuffer.SetOnFreeMem(value: TGpFifoMemoryEvent);
begin
  FOnFreeMem := value;
end; { TGpFifoBuffer.SetOnFreeMem }

procedure TGpFifoBuffer.SetOnGetMem(value: TGpFifoMemoryEvent);
begin
  FOnGetMem := value;
end; { TGpFifoBuffer.SetOnGetMem }

procedure TGpFifoBuffer.SetSize(value: integer);
begin
  FMaxSize := value;
  Truncate;
end; { TGpFifoBuffer.SetSize }

procedure TGpFifoBuffer.Truncate;
var
  block: TFifoBlock;
begin
  while FCurrentSize > Size do begin
    block := (FFifo.RemoveFromTail as TFifoBlock);
    if not assigned(block) then begin
      // Truncate active block
      Assert(FActiveBlockInUse);
      FActiveBlock.Size := FActiveBlock.Position + Size;
      FCurrentSize := FActiveBlock.Size - FActiveBlock.Position;
      break; //while
    end;
    Dec(FCurrentSize, block.Size);
    ReleaseBlock(block);
  end;
  Assert(FCurrentSize >= 0);
  Assert(FCurrentSize <= Size);
end; { TGpFifoBuffer.Truncate }

procedure TGpFifoBuffer.VerifyList;
var
  listObj  : TGpDoublyLinkedListObject;
  totalSize: integer;
begin
  if FLastThreadId = 0 then
    FLastThreadId := GetCurrentThreadID
  else if FLastThreadId <> GetCurrentThreadID then
    raise Exception.CreateFmt('TGpFifoBuffer: Used from %d and %d', [FLastThreadId, GetCurrentThreadID]);
  totalSize := 0;
  if FActiveBlockInUse then
    Inc(totalSize, FActiveBlock.Size - FActiveBlock.Position);
  {$IFDEF VER150}
  // TP Non tested D7 friendly loop
  listObj := FFifo.Head;
  while Assigned(listObj) do
  begin
    Inc(totalSize, TFifoBlock(listObj).Size);
    listObj := listObj.Next;
  end;
  {$ELSE}
  for listobj in FFifo do
    Inc(totalSize, TFifoBlock(listObj).Size);
  {$ENDIF}
  if totalSize <> FCurrentSize then
    raise Exception.CreateFmt('TGpFifoBuffer: Current size %d, actual size %d', [FCurrentSize, totalSize]);
end; { TGpFifoBuffer.VerifyList }

function TGpFifoBuffer.Write(const buf; bufSize: integer): boolean;
begin
  Result := FifoPlace >= bufSize;
  if Result then
    AddBlock(AllocateBlockBuf(buf,bufSize));
end; { TGpFifoBuffer.Write }

function TGpFifoBuffer.Write(data: TStream; dataSize: integer): boolean;
begin
  dataSize := Min(dataSize, data.Size);
  Result := FifoPlace >= dataSize;
  if Result then
    AddBlock(AllocateBlockStream(data, Min(dataSize, FifoPlace)));
end; { TGpFifoBuffer.Write }

{$IFDEF Unicode}
{$IFNDEF GpLists_LimitedGenerics}
{ TGpSkipListEnumerator<T, K> }

constructor TGpSkipListEnumerator<T>.Create(head, tail: TGpSkipListEl<T>);
begin
  FHead := head;
  FTail := tail;
  FCurrent := FHead;
end; { TGpSkipListEnumerator<T>.Create }

function TGpSkipListEnumerator<T>.GetCurrent: T;
begin
  Result := FCurrent.Element;
end; { TGpSkipListEnumerator<T>.GetCurrent }

function TGpSkipListEnumerator<T>.MoveNext: boolean;
begin
  FCurrent := FCurrent.Pointers[1];
  Result := (FCurrent <> FTail);
end; { TGpSkipListEnumerator<T>.MoveNext }

{ TGpSkipListEl<T> }

constructor TGpSkipListEl<T>.Create(numLevels: integer);
begin
  inherited Create;
  SetLength(FPointers, numLevels);
end; { TGpSkipListEl<T>.Create }

constructor TGpSkipListEl<T>.Create(const el: T; numLevels: integer);
begin
  Create(numLevels);
  FElement := el;
end; { TGpSkipListEl<T>.Create }

function TGpSkipListEl<T>.GetPointers(idx: integer): TGpSkipListEl<T>;
begin
  Result := FPointers[idx-1];
end; { TGpSkipListEl }

procedure TGpSkipListEl<T>.SetPointers(idx: integer; const value: TGpSkipListEl<T>);
begin
  FPointers[idx-1] := value;
end; { TGpSkipListEl }

{ TGpSkipList<T,K> }

constructor TGpSkipList<T,K>.Create(keyExtractor: TGpSkipListExtractKey<T,K>;
  comparer: TGpSkipListCompare<K>; levelProbability: real; maxLevels: integer);
var
  iPtr: integer;
begin
  FGetKey := keyExtractor;
  FComparer := comparer;
  FLevelProb := levelProbability;
  FMaxLevels := maxLevels;
  SetLength(FLast, FMaxLevels+1); //1-base index
  Initialize;
end; { TGpSkipList<T,K>.Create }

destructor TGpSkipList<T,K>.Destroy;
begin
  RemoveElements;
  inherited;
end; { TGpSkipList<T,K>.Destroy }

function TGpSkipList<T,K>.Compare(const key1, key2: K): integer;
begin
  Result := FComparer(key1, key2);
end; { TGpSkipList<T,K>.Compare }

procedure TGpSkipList<T, K>.Clear;
begin
  RemoveElements;
  Initialize;
end; { TGpSkipList }

function TGpSkipList<T,K>.Compare(const key: K; el: TGpSkipListEl<T>): integer;
begin
  Result := Compare(key, GetKey(el.Element));
end; { TGpSkipList<T,K>.Compare }

function TGpSkipList<T,K>.Contains(const key: K): boolean;
var
  x: TGpSkipListEl<T>;
begin
  Result := WalkDown(key, x);
end; { TGpSkipList<T,K>.Contains }

function TGpSkipList<T,K>.ContainsElement(const element: T): boolean;
begin
  Result := Contains(GetKey(element));
end; { TGpSkipList<T,K>.ContainsElement }

function TGpSkipList<T, K>.Count: integer;
begin
  Result := FCount;
end; { TGpSkipList }

function TGpSkipList<T,K>.WalkDown(const key: K; var x: TGpSkipListEl<T>): boolean;
var
  i: integer;
begin
  x := FHead;
  for i := FTopLevel downto 1 do begin
    while (x.Pointers[i] <> FTail) and (Compare(key, x.Pointers[i]) > 0) do
      x := x.Pointers[i];
    FLast[i] := x;
  end;
  x := x.Pointers[1];
  Result := (x <> FTail) and assigned(x) and (Compare(key, x) = 0);
end; { TGpSkipList<T,K>.WalkDown }

function TGpSkipList<T,K>.Insert(const element: T): boolean;
var
  i    : integer;
  level: integer;
  x    : TGpSkipListEl<T>;
begin
  Result := false;
  if not ContainsElement(element) then begin
    level := RandomLevel;
    if level > FTopLevel then begin
      for i := FTopLevel+1 to level do
        FLast[i] := FHead;
      FTopLevel := level;
    end;
    x := TGpSkipListEl<T>.Create(element, level);
    for i := 1 to level do begin
      x.Pointers[i] := FLast[i].Pointers[i];
      FLast[i].Pointers[i] := x;
    end;
    Inc(FCount);
    Result := true;
  end;
end; { TGpSkipList<T,K>.Insert }

function TGpSkipList<T, K>.Insert(const element: T; var position: TGpSkipListEl<T>): boolean;
var
  i    : integer;
  level: integer;
begin
  Result := false;
  if not ContainsElement(element) then begin
    level := RandomLevel;
    if level > FTopLevel then begin
      for i := FTopLevel+1 to level do
        FLast[i] := FHead;
      FTopLevel := level;
    end;
    position := TGpSkipListEl<T>.Create(element, level);
    for i := 1 to level do begin
      position.Pointers[i] := FLast[i].Pointers[i];
      FLast[i].Pointers[i] := position;
    end;
    Inc(FCount);
    Result := true;
  end;
end; { TGpSkipList }

function TGpSkipList<T,K>.Locate(const key: K; var element: T): boolean;
var
  x: TGpSkipListEl<T>;
begin
  Result := Locate(key, x);
  if Result then
    element := x.Element;
end; { TGpSkipList<T,K>.Locate }

function TGpSkipList<T, K>.Locate(const key: K; var element: TGpSkipListEl<T>): boolean;
begin
  Result := WalkDown(key, element);
end; { TGpSkipList }

function TGpSkipList<T,K>.Delete(const key: K): boolean;
var
  x: TGpSkipListEl<T>;
  i: integer;
begin
  Result := false;
  if WalkDown(key, x) then begin
    for i := 1 to FTopLevel do begin
      if FLast[i].Pointers[i] <> x then
        break; //for i
      FLast[i].Pointers[i] := x.Pointers[i];
    end;
    x.Free;
    while (FTopLevel > 1) and (FHead.Pointers[FTopLevel] = FTail) do
      Dec(FTopLevel);
    Dec(FCount);
    Result := true;
  end;
end; { TGpSkipList<T,K>.Delete }

procedure TGpSkipList<T,K>.DeleteElement(const element: T);
begin
  Delete(GetKey(element));
end; { TGpSkipList<T,K>.DeleteElement }

function TGpSkipList<T,K>.IsEmpty: boolean;
begin
  Result := (FCount = 0);
end; { TGpSkipList<T,K>.IsEmpty }

function TGpSkipList<T,K>.GetEnumerator: TGpSkipListEnumerator<T>;
begin
  Result := TGpSkipListEnumerator<T>.Create(FHead, FTail);
end; { TGpSkipList }

function TGpSkipList<T,K>.GetKey(const el: T): K;
begin
  Result := FGetKey(el);
end; { TGpSkipList<T,K> }

procedure TGpSkipList<T, K>.Initialize;
var
  iPtr: integer;
begin
  FTail := TGpSkipListEl<T>.Create(FMaxLevels);
  FHead := TGpSkipListEl<T>.Create(FMaxLevels);
  for iPtr := 1 to FMaxLevels do
    FHead.Pointers[iPtr] := FTail;
  FTopLevel := 1;
  FCount := 0;
end; { TGpSkipList }

function TGpSkipList<T, K>.Next(el: TGpSkipListEl<T>): TGpSkipListEl<T>;
begin
  Result := el.Pointers[1];
end; { TGpSkipList }

function TGpSkipList<T,K>.RandomLevel: integer;
begin
  Result := 1;
  while (Random < FLevelProb) and (Result < FMaxLevels) do
    Inc(Result);
end; { TGpSkipList<T,K>RandomLevel }

procedure TGpSkipList<T, K>.RemoveElements;
var
  p: TGpSkipListEl<T>;
  q: TGpSkipListEl<T>;
begin
  p := FHead;
  while p <> FTail do begin
    q := p;
    p := p.Pointers[1];
    q.Free;
  end;
  FHead := nil;
  FreeAndNil(FTail);
  FCount := 0;
end; { TGpSkipList }

function TGpSkipList<T, K>.Replace(position: TGpSkipListEl<T>; const element: T): T;
begin
  Result := position.Element;
  position.Element := element;
end; { TGpSkipList }

{ TGpSkipList<T> }

constructor TGpSkipList<T>.Create(comparer: TGpSkipListCompare<T>; levelProbability: real;
  maxLevels: integer);
begin
  inherited Create;
  FSkipList := TGpSkipList<T,T>.Create(CopyKeyExtractor, comparer, levelProbability,
    maxLevels);
end; { TGpSkipList<T>.Create }

destructor TGpSkipList<T>.Destroy;
begin
  Clear;
  FreeAndNil(FSkipList);
  inherited;
end; { TGpSkipList<T>.Destroy }

function TGpSkipList<T>.GetEnumerator: TGpSkipListEnumerator<T>;
begin
  Result := FSkipList.GetEnumerator;
end; { TGpSkipList<T>.GetEnumerator }

procedure TGpSkipList<T>.Clear;
begin
  FSkipList.Clear;
end; { TGpSkipList }

function TGpSkipList<T>.Contains(const key: T): boolean;
begin
  Result := FSkipList.Contains(key);
end; { TGpSkipList<T>.Contains }

class function TGpSkipList<T>.CopyKeyExtractor(const el: T): T;
begin
  Result := el;
end; { TGpSkipList<T>.CopyKeyExtractor }

function TGpSkipList<T>.Count: integer;
begin
  Result := FSkipList.Count;
end; { TGpSkipList }

procedure TGpSkipList<T>.Delete(const element: T);
begin
  FSkipList.Delete(element);
end; { TGpSkipList<T>.Delete }

function TGpSkipList<T>.GetHead: TGpSkipListEl<T>;
begin
  Result := FSkipList.Head;
end; { TGpSkipList }

function TGpSkipList<T>.GetTail: TGpSkipListEl<T>;
begin
  Result := FSkipList.Tail;
end; { TGpSkipList }

function TGpSkipList<T>.Insert(const element: T): boolean;
begin
  Result := FSkipList.Insert(element);
end; { TGpSkipList<T>.Insert }

function TGpSkipList<T>.Insert(const element: T; var position: TGpSkipListEl<T>): boolean;
begin
  Result := FSkipList.Insert(element, position);
end; { TGpSkipList }

function TGpSkipList<T>.IsEmpty: boolean;
begin
  Result := FSkipList.IsEmpty;
end; { TGpSkipList<T>.IsEmpty }

function TGpSkipList<T>.Locate(const key: T; var element: T): boolean;
begin
  Result := FSkipList.Locate(key, element);
end; { TGpSkipList }

function TGpSkipList<T>.Locate(const key: T; var element: TGpSkipListEl<T>): boolean;
begin
  Result := FSkipList.Locate(key, element);
end; { TGpSkipList }

function TGpSkipList<T>.Next(el: TGpSkipListEl<T>): TGpSkipListEl<T>;
begin
  Result := FSkipList.Next(el);
end; { TGpSkipList }

procedure TGpSkipList<T>.Remove(const element: T);
begin
  FSkipList.Delete(element);
end; { TGpSkipList }

function TGpSkipList<T>.Replace(position: TGpSkipListEl<T>; const element: T): T;
begin
  Result := FSkipList.Replace(position, element);
end; { TGpSkipList }

{ TGpSkipObjectList<T> }

constructor TGpSkipObjectList<T>.Create(comparer: TGpSkipListCompare<T>;
  ownsObjects: boolean; levelProbability: real; maxLevels: integer);
begin
  inherited Create(comparer, levelProbability, maxLevels);
  FOwnsObjects := ownsObjects;
end; { TGpSkipObjectList }

procedure TGpSkipObjectList<T>.Clear;
var
  obj: T;
begin
  if FOwnsObjects then
    for obj in Self do
      obj.Free;
  inherited;
end; { TGpSkipObjectList }

procedure TGpSkipObjectList<T>.Delete(const element: T);
begin
  inherited;
  if FOwnsObjects then
    element.Free;
end; { TGpSkipObjectList }

{$IF CompilerVersion >= 24} //XE3 or newer

{ TGpCache<K, V> }

constructor TGpCache<K, V>.Create(ANumElements: integer;
  const AComparer: IEqualityComparer<K>; AOwnsValues: boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  if AOwnsValues and (PTypeInfo(System.TypeInfo(V)).Kind <> tkClass) then
    raise Exception.Create('TGpCache<K, V>.Create: AOwnsValues is set, but V is not a class type');
  FCache := TDictionary<K,integer>.Create(ANumElements, AComparer);
  BuildLinkedList(ANumElements);
end; { TGpCache<K, V>.Create }

destructor TGpCache<K, V>.Destroy;
begin
  if FOwnsValues then
    DestroyOwnedValues;
  FreeAndNil(FCache);
  inherited;
end; { TGpCache<K, V>.Destroy }

function TGpCache<K, V>.IsNil(element: integer): boolean; //inline
begin
  Result := (element = NilPointer);
end; { TGpCache<K, V>.IsNil }

procedure TGpCache<K, V>.DestroyOwnedValues;
begin
  while not IsNil(FHead) do begin
    {$IF CompilerVersion < 25} // DisposeOf was implemented in XE4
    PObject(@FKeys[FHead].Value)^.Free;
    {$ELSE}
    PObject(@FKeys[FHead].Value)^.DisposeOf;
    {$IFEND}
    FHead := FKeys[FHead].Next;
  end;
end; { TGpCache<K, V>.DestroyOwnedValues }

procedure TGpCache<K, V>.BuildLinkedList(numElements: integer);
var
  i: integer;
begin
  SetLength(FKeys, numElements);

  for i := 0 to numElements - 2 do
    FKeys[i].Next := i + 1;
  FKeys[numElements - 1].Next := NilPointer;

  FKeys[0].Prev := NilPointer;
  for i := 1 to numElements - 1 do
    FKeys[i].Prev := i - 1;

  FHead := NilPointer;
  FTail := NilPointer;
  FFreeList := 0;
end; { TGpCache<K, V>.BuildLinkedList }

function TGpCache<K, V>.GetFree: integer;
begin
  if IsNil(FFreeList) then
    raise Exception.Create('TGpCache<K, V>.GetFree: Free list is empty!');
  Result := FFreeList;
  FFreeList := FKeys[FFreeList].Next;
end; { TGpCache<K, V>.GetFree }

procedure TGpCache<K, V>.InsertInFront(elementIdx: integer);
begin
  FKeys[elementIdx].Next := FHead;
  FKeys[elementIdx].Prev := NilPointer;
  if not IsNil(FHead) then
    FKeys[FHead].Prev := elementIdx;
  FHead := elementIdx;
  if IsNil(FTail) then
    FTail := FHead;
end; { TGpCache<K, V>.InsertInFront }

function TGpCache<K, V>.IsFull: boolean;
begin
  Result := IsNil(FFreeList);
end; { TGpCache<K, V>.IsFull }

function TGpCache<K, V>.Remove(const key: K): boolean;
var
  element : integer;
  pElement: PListElement;
begin
  Result := FCache.TryGetValue(key, element);
  if Result then begin
    Unlink(element);
    pElement := @FKeys[element];
    pElement.Next := FFreeList;
    pElement.Prev := NilPointer;
    FFreeList := element;
    FCache.Remove(key);
    if FOwnsValues then
      {$IF CompilerVersion < 25} // DisposeOf was implemented in XE4
      PObject(@pElement.Value)^.Free;
      {$ELSE}
      PObject(@pElement.Value)^.DisposeOf;
      {$IFEND}
  end;
end; { TGpCache<K, V>.Remove }

function TGpCache<K, V>.RemoveOldest: integer;
var
  element: integer;
begin
  if IsNil(FTail) then
    raise Exception.Create('TGpCache<K, V>.RemoveOldest: List is empty!');
  Result := FTail;
  Unlink(FTail);
  FCache.Remove(FKeys[Result].Key);
  if FOwnsValues then
    {$IF CompilerVersion < 25} // DisposeOf was implemented in XE4
    PObject(@FKeys[Result].Value)^.Free;
    {$ELSE}
    PObject(@FKeys[Result].Value)^.DisposeOf;
    {$IFEND}
end; { TGpCache<K, V>.RemoveOldest }

function TGpCache<K, V>.TryGetValue(const key: K; var value: V): boolean;
var
  element: integer;
begin
  Result := FCache.TryGetValue(key, element);
  if Result then begin
    value := FKeys[element].Value;
    Unlink(element);
    InsertInFront(element);
  end;
end; { TGpCache<K, V>.TryGetValue }

procedure TGpCache<K, V>.Unlink(element: integer);
var
  pElement: PListElement;
begin
  pElement := @FKeys[element];
  if not IsNil(pElement.Next) then
    FKeys[pElement.Next].Prev := pElement.Prev
  else begin
    Assert(FTail = element);
    FTail := pElement.Prev;
  end;

  if not IsNil(pElement.Prev) then
    FKeys[pElement.Prev].Next := pElement.Next
  else begin
    Assert(FHead = element);
    FHead := pElement.Next;
  end;
end; { TGpCache<K, V>.Unlink }

procedure TGpCache<K, V>.Update(const key: K; const value: V);
var
  element : integer;
  oldValue: V;
  pElement: PListElement;
begin
  if FCache.TryGetValue(key, element) then begin // update existing element
    pElement := @FKeys[element];
    if not FOwnsValues then
      pElement.Value := value
    else begin
      oldValue := pElement.Value;
      pElement.Value := value;
      if PObject(@oldValue)^ <> PObject(@value)^ then
        {$IF CompilerVersion < 25} // DisposeOf was implemented in XE4
        PObject(@oldValue)^.Free;
        {$ELSE}
        PObject(@oldValue)^.DisposeOf;
        {$IFEND}
    end;
    Unlink(element);
    InsertInFront(element);
  end
  else begin // add new element
    if IsFull then
      element := RemoveOldest
    else
      element := GetFree;
    InsertInFront(element);
    pElement := @FKeys[element];
    pElement.Key := key;
    pElement.Value := value;
    FCache.Add(key, element);
  end;
end; { TGpCache<K, V>.Update }
{$IFEND CompilerVersion >= 24}
{$ENDIF}
{$ENDIF}

end.

