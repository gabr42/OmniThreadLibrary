(*:Various TList descendants, TList-compatible, and TList-similar classes.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2008, Primoz Gabrijelcic
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
   Last modification : 2008-11-10
   Version           : 1.42
</pre>*)(*
   History:
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
  {$IF (CompilerVersion >= 17)} //Delphi 2005 or newer
    {$DEFINE GpLists_Inline}
    {$DEFINE GpLists_TStringListHelper}
    {$DEFINE GpLists_Enumerators}
  {$IFEND}
{$ENDIF}

uses
  Classes,
  Contnrs,
  SyncObjs,
  SpinLock,
  Windows,
  SysUtils;

const
  CUpperListBound = MaxInt; //converted to Self.Count-1 inside Slice and Walk

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
    constructor Create(aValue: string = '');
    property Value: string read sValue write sValue;
  end; { TGpString }

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
  public
    constructor Create(aList: TGpIntegerList; idxFrom, idxTo: integer);
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
  protected
    procedure HandleListChange(list: TObject; idxItem: integer; operation: TGpListOperation);
    property List: TGpIntegerList read ilweList;
  public
    constructor Create(aList: TGpIntegerList; idxFrom, idxTo: integer);
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
  public
    constructor Create(list: TGpIntegerList; idxFrom, idxTo: integer);
    function  GetEnumerator: TGpIntegerListEnumerator;
  end; { TGpIntegerListSliceEnumeratorFactory }

  TGpIntegerListWalkEnumeratorFactory = record
  private
    wefList   : TGpIntegerList;
    wefIdxFrom: integer;
    wefIdxTo  : integer;
  public
    constructor Create(list: TGpIntegerList; idxFrom, idxTo: integer);
    function  GetEnumerator: TGpIntegerListWalkEnumerator;
  end; { TGpIntegerListWalkEnumeratorFactory }
  {$ENDIF GpLists_Enumerators}

  TGpIntegerListSortCompare = function(List: TGpIntegerList;
    Index1, Index2: integer): integer;

  {:List of integers.
    @since   2002-07-04.
  }
  TGpIntegerList = class
  private
    ilDuplicates          : TDuplicates;
    ilList                : TList;
    ilNotificationHandlers: TGpTMethodList;
    ilSorted              : Boolean;
  protected
    function  GetAsDelimitedText(const delimiter: string;
      appendLastDelimiter: boolean): string;
    function  GetCapacity: integer; virtual;
    function  GetCount: integer; virtual;
    function  GetItems(idx: integer): integer; virtual;
    function  GetText: string; virtual;
    procedure InsertItem(idx, item: integer);
    procedure Notify(idxItem: integer; operation: TGpListOperation); {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure QuickSort(L, R: integer; SCompare: TGpIntegerListSortCompare);
    procedure SetCapacity(const value: integer); virtual;
    procedure SetCount(const value: integer); virtual;
    procedure SetItems(idx: integer; const value: integer); virtual; 
    procedure SetSorted(const value: boolean); virtual;
    procedure SetText(const value: string); virtual;
  public
    constructor Create; overload;
    constructor Create(elements: array of integer); overload;
    destructor  Destroy; override;
    function  Add(item: integer): integer; virtual;
    procedure Append(elements: array of integer); overload;
    procedure Append(list: TGpIntegerList); overload; virtual;
    function  AsDelimitedText(const delimiter: string): string; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  AsHexText(const delimiter: string = ''): string;
    procedure Assign(elements: array of integer); overload;
    procedure Assign(list: TGpIntegerList); overload; virtual;
    procedure Clear; virtual;
    function  Contains(item: integer): boolean;     {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure CustomSort(sortMethod: TGpIntegerListSortCompare);
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
    procedure Sort;                                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure UnregisterNotification(notificationHandler: TGpListNotificationEvent);
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpIntegerListEnumerator; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Slice(idxFrom: integer; idxTo: integer = CUpperListBound):
      TGpIntegerListSliceEnumeratorFactory;
    function  Walk(idxFrom: integer = 0; idxTo: integer = CUpperListBound):
      TGpIntegerListWalkEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Duplicates: TDuplicates read ilDuplicates write ilDuplicates;
    property Items[idx: integer]: integer read GetItems write SetItems; default;
    property Sorted: boolean read ilSorted write SetSorted;
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
  public
    constructor Create(aList: TGpInt64List; idxFrom, idxTo: integer);
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
  protected
    procedure HandleListChange(list: TObject; idxItem: integer; operation: TGpListOperation);
    property List: TGpInt64List read ilweList;
  public
    constructor Create(aList: TGpInt64List; idxFrom, idxTo: integer);
    destructor Destroy; override;
    function  GetCurrent: integer;
    function  MoveNext: boolean;
    property Current: integer read GetCurrent;
  end; { TGpInt64ListWalkEnumerator }

  TGpInt64ListSliceEnumeratorFactory = record
  private
    sefList   : TGpInt64List;
    sefIdxFrom: integer;
    sefIdxTo  : integer;
  public
    constructor Create(list: TGpInt64List; idxFrom, idxTo: integer);
    function  GetEnumerator: TGpInt64ListEnumerator;
  end; { TGpInt64ListSliceEnumeratorFactory }

  TGpInt64ListWalkEnumeratorFactory = record
  private
    wefList   : TGpInt64List;
    wefIdxFrom: integer;
    wefIdxTo  : integer;
  public
    constructor Create(list: TGpInt64List; idxFrom, idxTo: integer);
    function  GetEnumerator: TGpInt64ListWalkEnumerator;
  end; { TGpInt64ListWalkEnumeratorFactory }
  {$ENDIF GpLists_Enumerators}

  TGpInt64ListSortCompare = function(List: TGpInt64List; Index1, Index2: integer):
    integer;

  {:List of 64-bit integers.
    @since   2006-09-20
  }
  TGpInt64List = class
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
    function  GetItems(idx: integer): int64; virtual;
    function  GetText: string; virtual;
    procedure InsertItem(idx: integer; item: int64);
    procedure Notify(idxItem: integer; operation: TGpListOperation);
    procedure QuickSort(L, R: integer; SCompare: TGpInt64ListSortCompare);
    procedure SetCapacity(const value: integer); virtual;
    procedure SetCount(const value: integer); virtual;
    procedure SetItems(idx: integer; value: int64); virtual;
    procedure SetSorted(const value: boolean); virtual;
    procedure SetText(const value: string); virtual;
  public
    constructor Create; overload;
    constructor Create(elements: array of int64); overload;
    destructor  Destroy; override;
    function  Add(item: int64): integer; virtual;
    procedure Append(elements: array of int64); overload;
    procedure Append(list: TGpInt64List); overload; virtual;
    procedure Append(list: TGpIntegerList); overload; virtual;
    function  AsDelimitedText(const delimiter: string): string; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  AsHexText(const delimiter: string = ''): string;
    procedure Assign(elements: array of int64); overload;
    procedure Assign(list: TGpInt64List); overload; virtual;
    procedure Assign(list: TGpIntegerList); overload; virtual;
    procedure Clear; virtual;
    function  Contains(item: int64): boolean;       {$IFDEF GpLists_Inline}inline;{$ENDIF}
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
    function  Slice(idxFrom: integer; idxTo: integer = CUpperListBound):
      TGpInt64ListSliceEnumeratorFactory;
    function  Walk(idxFrom: integer = 0; idxTo: integer = CUpperListBound):
      TGpInt64ListWalkEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Duplicates: TDuplicates read ilDuplicates write ilDuplicates;
    property Items[idx: integer]: int64 read GetItems write SetItems; default;
    property Sorted: boolean read ilSorted write SetSorted;
    property Text: string read GetText write SetText;
  end; { TGpInt64List }

  {$IFDEF GpLists_Enumerators}
  TGpIntegerObjectList = class;

  TGpIntegerObjectListWalkKVEnumerator = class
  private
    wkeCurrentKV     : TGpKeyValue;
    wkeListEnumerator: TGpIntegerListWalkEnumerator;
  public
    constructor Create(aList: TGpIntegerObjectList; idxFrom, idxTo: integer);
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
  public
    constructor Create(list: TGpIntegerObjectList; idxFrom, idxTo: integer);
    function  GetEnumerator: TGpIntegerObjectListWalkKVEnumerator;
  end; { TGpIntegerObjectListWalkKVEnumeratorFactory }
  {$ENDIF GpLists_Enumerators}

  {:Integer list where each integer is accompanied with an object.
    @since   2003-06-09
  }
  TGpIntegerObjectList = class(TGpIntegerList)
  private
    iolObjects: TObjectList;
  protected
    function  GetObject(idxObject: integer): TObject; virtual;
    procedure SetObject(idxObject: integer; const value: TObject); virtual;
  public
    constructor Create(ownsObjects: boolean = true);
    destructor  Destroy; override;
    function  Add(item: integer): integer; override;
    function  AddObject(item: integer; obj: TObject): integer; virtual;
    procedure Clear; override;
    procedure Delete(idx: integer); override;
    function  Dump(baseAddr: pointer): pointer; override;
    function  EnsureObject(item: integer; obj: TObject): integer; virtual;
    procedure Exchange(idx1, idx2: integer); override;
    function  ExtractObject(idxObject: integer): TObject;
    function  FetchObject(item: integer): TObject;
    procedure Insert(idx: integer; item: integer); override;
    procedure InsertObject(idx: integer; item: integer; obj: TObject); virtual;
    function  LoadFromStream(stream: TStream): boolean; override;
    procedure Move(curIdx, newIdx: integer); override;
    function  Restore(baseAddr: pointer): pointer; override;
    procedure SaveToStream(stream: TStream); override;
    {$IFDEF GpLists_Enumerators}
    function WalkKV(idxFrom: integer = 0; idxTo: integer = CUpperListBound):
      TGpIntegerObjectListWalkKVEnumeratorFactory;
    {$ENDIF GpLists_Enumerators}
    property Objects[idxObject: integer]: TObject read GetObject write SetObject;
  end; { TGpIntegerObjectList }

  {:A thin layer over TGpIntegerObject list where each item has associated counter (stored
    in the Objects property).
  }
  TGpCountedIntegerList = class(TGpIntegerObjectList)
  protected
    function  GetCounter(idx: integer): integer; virtual;
    function  GetItemCounter(item: integer): integer;
    procedure SetCounter(idx: integer; const value: integer); virtual;
    procedure SetItemCounter(item: integer; const value: integer);
  public
    constructor Create; reintroduce;
    function  Add(item, count: integer): integer; reintroduce;
    function  Ensure(item, count: integer): integer; reintroduce;
    procedure SortByCounter(descending: boolean = true);
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
    constructor Create(aList: TGpInt64ObjectList; idxFrom, idxTo: integer);
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
  public
    constructor Create(list: TGpInt64ObjectList; idxFrom, idxTo: integer);
    function  GetEnumerator: TGpInt64ObjectListWalkKVEnumerator;
  end; { TGpInt64ObjectListWalkKVEnumeratorFactory }
  {$ENDIF GpLists_Enumerators}

  {:Int64 list where each integer is accompanied with an object.
    @since   2006-09-20
  }
  TGpInt64ObjectList = class(TGpInt64List)
  private
    iolObjects: TObjectList;
  protected
    function  GetObject(idxObject: integer): TObject; virtual;
    procedure SetObject(idxObject: integer; const value: TObject); virtual;
  public
    constructor Create(ownsObjects: boolean = true);
    destructor  Destroy; override;
    function  Add(item: int64): integer; override;
    function  AddObject(item: int64; obj: TObject): integer; virtual;
    procedure Clear; override;
    procedure Delete(idx: integer); override;
    function  Dump(baseAddr: pointer): pointer; override;
    function  EnsureObject(item: int64; obj: TObject): integer; virtual;
    procedure Exchange(idx1, idx2: integer); override;
    function  ExtractObject(idxObject: integer): TObject;
    function  FetchObject(item: int64): TObject;
    procedure Insert(idx: integer; item: int64); override;
    procedure InsertObject(idx: integer; item: int64; obj: TObject); virtual;
    function  LoadFromStream(stream: TStream): boolean; override;
    procedure Move(curIdx, newIdx: integer); override;
    function  Restore(baseAddr: pointer): pointer; override;
    procedure SaveToStream(stream: TStream); override;
{$IFDEF GpLists_Enumerators}
    function  WalkKV(idxFrom: integer = 0; idxTo: integer = CUpperListBound):
      TGpInt64ObjectListWalkKVEnumeratorFactory;
{$ENDIF GpLists_Enumerators}
    property Objects[idxObject: integer]: TObject read GetObject write SetObject;
  end; { TGpInt64ObjectList }

  {:A thin layer over TGpInt64Object list where each item has 64-bit associated counter
    (stored in the Objects property).
  }
  TGpCountedInt64List = class(TGpInt64ObjectList)
  protected
    function  GetCounter(idx: integer): int64; virtual;
    function  GetItemCounter(item: int64): int64;
    procedure SetCounter(idx: integer; const value: int64); virtual;
    procedure SetItemCounter(item: int64; const value: int64);
  public
    function  Add(item: int64; count: int64): integer; reintroduce;
    function  Ensure(item: int64; count: int64): integer; reintroduce;
    procedure SortByCounter(descending: boolean = true);
    property Counter[idx: integer]: int64 read GetCounter write SetCounter;
    property ItemCounter[item: int64]: int64 read GetItemCounter write SetItemCounter;
  end; { TGpCountedInt64List }

  {$IFDEF GpLists_TStringListHelper}
  ///<summary>Implements helpers for the TStringList.</summary>
  ///<since>2007-06-28</since>
  TGpStringListHelper = class helper for TStringList
  public
    function  Contains(const s: string): boolean;   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  FetchObject(const s: string): TObject;
    procedure FreeObjects;
    function  Last: string;                         {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Sort;
    procedure Remove(const s: string);
  end; { TGpStringListHelper }
  {$ENDIF GpLists_TStringListHelper}

  {:String list where each item has associated counter (stored in the Objects property).
  }
  TGpCountedStringList = class(TStringList)
  protected
    function  GetItemCount(idx: integer): integer; virtual;
    procedure SetItemCount(idx: integer; const value: integer); virtual;
  public
    function  Add(const s: string; count: integer): integer; reintroduce;
    function  Contains(const s: string): boolean;   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Ensure(const s: string; count: integer): integer;
    procedure SortByCounter(descending: boolean = true);
    property  Counter[idx: integer]: integer read GetItemCount write SetItemCount;
  end; { TGpCountedStringList }

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

  {:List of TMethod records.
    @since   2006-05-15
  }
  TGpTMethodList = class
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
    function  Add(item: TMethod): integer;          {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Assign(list: TGpTMethodList);         {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Clear;                                {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Contains(item: TMethod): boolean;     {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Delete(idx: integer);                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Ensure(item: TMethod): integer;       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpTMethodListEnumerator; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$ENDIF GpLists_Enumerators}
    function  IndexOf(item: TMethod): integer;
    procedure Insert(idx: integer; item: TMethod);  {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Remove(item: TMethod);                {$IFDEF GpLists_Inline}inline;{$ENDIF}
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

  {:TStringList-based list of classes. Class names must be unique. Useful when you need to
    implement a class registry to generate objects from class names.
    @since   2007-10-25
  }
  TGpClassList = class
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
    function  Add(aClass: TClass): integer;
    procedure Clear;                                {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Delete(idx: integer);                 {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  CreateObject(sClass: string): TObject;
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpClassListEnumerator;{$IFDEF GpLists_Inline}inline;{$ENDIF}
    {$ENDIF GpLists_Enumerators}
    function  IndexOf(aClass: TClass): integer; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  IndexOf(sClass: string): integer; overload; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Remove(aClass: TClass); overload;     {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure Remove(sClass: string); overload;     {$IFDEF GpLists_Inline}inline;{$ENDIF}
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount;
    property Items[idx: integer]: TClass read GetItems; default;
  end; { TGpClassList }

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

  {:Fixed-size ring buffer of TObject references. Optionally thread-safe.
    @since   2003-07-25
  }
  TGpObjectRingBuffer = class
  private
    orbBuffer                    : array of TObject;
    orbBufferAlmostEmptyEvent    : THandle;
    orbBufferAlmostEmptyThreshold: integer;
    orbBufferAlmostFullEvent     : THandle;
    orbBufferAlmostFullThreshold : integer;
    orbBufferSize                : integer;
    orbCount                     : integer;
    orbHead                      : integer;
    orbLock                      : TSpinLock;
    orbOwnsObjects               : boolean;
    orbTail                      : integer;
  protected
    function  GetItem(iObject: integer): TObject; virtual;
    function  IncPointer(const ptr: integer; increment: integer = 1): integer;
    function  InternalIsFull: boolean;              {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure SetItem(iObject: integer; const value: TObject); virtual;
  public
    constructor Create(bufferSize: integer; ownsObjects: boolean = true;
      multithreaded: boolean = false);
    destructor  Destroy; override;
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
    property Items[iObject: integer]: TObject read GetItem write SetItem; default;
    property OwnsObjects: boolean read orbOwnsObjects;
  end; { TGpObjectRingBuffer }

  {:Object map comparision function.
    @since   2003-08-02
  }        
  TGpObjectMapCompare = function(userValue, mapValue: TObject): boolean;

  {:List, indexed by objects and containing objects.
    @since   2003-08-02
  }
  TGpObjectMap = class
  private
    omList: TGpIntegerObjectList;
  protected
    function  GetIndexedItem(idxItem: integer): TObject;   {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetIndexedValue(idxValue: integer): TObject; {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  GetItems(item: TObject): TObject; virtual;
    procedure SetItems(item: TObject; const value: TObject); virtual;
  public
    constructor Create(ownsObjects: boolean = true); overload;
    destructor  Destroy; override;
    procedure Clear; virtual;
    function  Count: integer;                       {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  Exists(value: TObject; compareFunc: TGpObjectMapCompare): boolean;
    procedure Find(value: TObject; compareFunc: TGpObjectMapCompare;
      var item: TObject); virtual;
    property Items[idxItem: integer]: TObject read GetIndexedItem;
    property ValuesIdx[idxValue: integer]: TObject read GetIndexedValue;
    property Values[item: TObject]: TObject read GetItems write SetItems; default;
  end; { TGpObjectMap }

  {:Matrix, indexed by two objects and containing objects.
    @since   2003-08-02
  }        
  TGpObjectObjectMap = class
  private
    oomCompareFunc: TGpObjectMapCompare;
    oomFindValue  : TObject;
    oomItem2      : TObject;
    oomMap        : TGpObjectMap;
    oomOwnsObjects: boolean;
  protected
    function  GetItems(item1, item2: TObject): TObject; virtual;
    function  Map(item: TObject): TGpObjectMap; virtual;
    procedure SetItems(item1, item2: TObject; const Value: TObject); virtual;
  public
    constructor Create(ownsObjects: boolean = true); overload;
    destructor  Destroy; override;
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
    function NextUnsafe: TGpDoublyLinkedListObject;
    function PreviousUnsafe: TGpDoublyLinkedListObject;
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
  {$ENDIF GpLists_Enumerators}

  {:A list of doubly-linked TGpDoublyLinkedListObject objects. NOT an owner of linked
    objects. Optionally thread-safe.
    @since   2003-10-27
  }
  TGpDoublyLinkedList = class
  private
    dllCount: integer;
    dllHead : TGpDoublyLinkedListObject;
    dllLock : TSpinLock;
    dllTail : TGpDoublyLinkedListObject;
  protected
    procedure Linking(obj: TGpDoublyLinkedListObject);      {$IFDEF GpLists_Inline}inline;{$ENDIF}
    function  HeadUnsafe: TGpDoublyLinkedListObject;
    function  TailUnsafe: TGpDoublyLinkedListObject;
    procedure Unlinking(obj: TGpDoublyLinkedListObject);    {$IFDEF GpLists_Inline}inline;{$ENDIF}
  public
    constructor Create(multithreaded: boolean = false); overload;
    destructor  Destroy; override;
    function  Count: integer;                               {$IFDEF GpLists_Inline}inline;{$ENDIF}
    procedure FreeAll;
    {$IFDEF GpLists_Enumerators}
    function  GetEnumerator: TGpDoublyLinkedListEnumerator; {$IFDEF GpLists_Inline}inline;{$ENDIF}
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
  Consts,
  Math;

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

function GpCompareInt64(userValue, mapValue: TObject): boolean;
begin
  Result := (TGpInt64(userValue).Value = TGpInt64(mapValue).Value);
end; { GpCompareInt64 }

{ globals }

function IntegerListCompare(List: TGpIntegerList; idx1, idx2: integer): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}
begin
  Result := IntegerCompare(List[idx1], List[idx2]);
end; { IntegerListCompare }

function Int64ListCompare(List: TGpInt64List; idx1, idx2: integer): integer; {$IFDEF GpLists_Inline}inline;{$ENDIF}
begin
  Result := Int64Compare(List[idx1], List[idx2]);
end; { Int64ListCompare }

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

constructor TGpString.Create(aValue: string);
begin
  inherited Create;
  sValue := aValue;
end; { TGpString.Create }

{ TGpReal }

constructor TGpReal.Create(aValue: real);
begin
  inherited Create;
  rValue := aValue;
end; { TGpReal.Create }
{$IFDEF GpLists_Enumerators}

{ TGpIntegerListEnumerator }

constructor TGpIntegerListEnumerator.Create(aList: TGpIntegerList; idxFrom, idxTo:
  integer);
begin
  ileIndex := idxFrom - 1;
  ileList := aList;
  ileIdxTo := idxTo;
end; { TGpIntegerListEnumerator.Create }

function TGpIntegerListEnumerator.GetCurrent: integer;
begin
  Result := ileList[ileIndex];
end; { TGpIntegerListEnumerator.GetCurrent }

function TGpIntegerListEnumerator.MoveNext: boolean;
begin
  Result := ileIndex < ileIdxTo;
  if Result then
    Inc(ileIndex);
end; { TGpIntegerListEnumerator.MoveNext }

{ TGpIntegerListWalkEnumerator }

constructor TGpIntegerListWalkEnumerator.Create(aList: TGpIntegerList; idxFrom, idxTo:
  integer);
begin
  inherited Create;
  ilweList := aList;
  ilweIndex := idxFrom - 1;
  ilweIdxTo := idxTo;
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
      else if idxItem < ilweIdxTo then
        Inc(ilweIdxTo);
    loDelete:
      if idxItem < ilweIndex then begin
        Dec(ilweIndex);
        Dec(ilweIdxTo);
      end
      else if idxItem < ilweIdxTo then 
        Dec(ilweIdxTo);
    else raise Exception.Create('TGpIntegerListWalkEnumerator.HandleListChange: Unexpected list operation');
  end;
end; { TGpIntegerListWalkEnumerator.HandleListChange }

function TGpIntegerListWalkEnumerator.MoveNext: boolean;
begin
  Result := ilweIndex < ilweIdxTo;
  if Result then
    Inc(ilweIndex);
end; { TGpIntegerListWalkEnumerator.MoveNext }

{ TGpIntegerListSliceEnumeratorFactory }

constructor TGpIntegerListSliceEnumeratorFactory.Create(list: TGpIntegerList; idxFrom,
  idxTo: integer);
begin
  sefList := list;
  sefIdxFrom := idxFrom;
  sefIdxTo := idxTo;
end; { TGpIntegerListSliceEnumeratorFactory.Create }

function TGpIntegerListSliceEnumeratorFactory.GetEnumerator: TGpIntegerListEnumerator;
begin
  Result := TGpIntegerListEnumerator.Create(sefList, sefIdxFrom, sefIdxTo);
end; { TGpIntegerListSliceEnumeratorFactory.GetEnumerator }

{ TGpIntegerListWalkEnumeratorFactory }

constructor TGpIntegerListWalkEnumeratorFactory.Create(list: TGpIntegerList; idxFrom,
  idxTo: integer);
begin
  wefList := list;
  wefIdxFrom := idxFrom;
  wefIdxTo := idxTo;
end; { TGpIntegerListWalkEnumeratorFactory.Create }

function TGpIntegerListWalkEnumeratorFactory.GetEnumerator: TGpIntegerListWalkEnumerator;
begin
  Result := TGpIntegerListWalkEnumerator.Create(wefList, wefIdxFrom, wefIdxTo);
end; { TGpIntegerListWalkEnumeratorFactory.GetEnumerator }
{$ENDIF GpLists_Enumerators}

{ TGpIntegerList }

constructor TGpIntegerList.Create;
begin
  inherited;
  ilList := TList.Create;
  ilNotificationHandlers := TGpTMethodList.Create;
end; { TGpIntegerList.Create }

constructor TGpIntegerList.Create(elements: array of integer);
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
  if not Sorted then begin
    Result := ilList.Add(pointer(item));
    Notify(Result, loInsert);
  end
  else begin
    if Find(item, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError : ilList.Error(SDuplicateItem, item);
      end;
    InsertItem(Result, item);
  end;
end; { TGpIntegerList.Add }

procedure TGpIntegerList.Append(elements: array of integer);
var
  iElement: integer;
begin
  SetCapacity(Length(elements));
  for iElement := Low(elements) to High(elements) do
    Add(elements[iElement]);
end; { TGpIntegerList.Append }

procedure TGpIntegerList.Append(list: TGpIntegerList);
var
  iItem: integer;
begin
  SetCapacity(list.Count);
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

procedure TGpIntegerList.Assign(elements: array of integer);
begin
  Clear;
  Append(elements);
end; { TGpIntegerList.Assign }

procedure TGpIntegerList.Assign(list: TGpIntegerList);
begin
  Clear;
  Append(list);
end; { TGpIntegerList.Assign }

procedure TGpIntegerList.Clear;
begin
  ilList.Clear;
end; { TGpIntegerList.Clear }

function TGpIntegerList.Contains(item: integer): boolean;
begin
  Result := (IndexOf(item) >= 0);
end; { TGpIntegerList.Contains }

procedure TGpIntegerList.CustomSort(sortMethod: TGpIntegerListSortCompare);
begin
  if not Sorted and (Count > 1) then
    QuickSort(0, Count - 1, sortMethod);
end; { TGpIntegerList.CustomSort }

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

{$IFDEF GpLists_Enumerators}
function TGpIntegerList.GetEnumerator: TGpIntegerListEnumerator;
begin
  Result := TGpIntegerListEnumerator.Create(Self, 0, Count - 1);
end; { TGpIntegerList.GetEnumerator }
{$ENDIF GpLists_Enumerators}

function TGpIntegerList.GetItems(idx: integer): integer;
begin
  Result := integer(ilList.Items[idx]);
end; { TGpIntegerList.GetItems }

function TGpIntegerList.GetText: string;
begin
  Result := GetAsDelimitedText(#13#10, true);
end; { TGpIntegerList.GetText }

function TGpIntegerList.IndexOf(item: integer): integer;
begin
  if Sorted then begin
    if not Find(item, Result) then
      Result := -1
  end
  else
    Result := ilList.IndexOf(pointer(item));
end; { TGpIntegerList.IndexOf }

procedure TGpIntegerList.Insert(idx, item: integer);
begin
  if Sorted then
    raise Exception.Create('Cannot insert element in sorted list.');
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
  if Sorted then
    raise Exception.Create('Cannot move elements in sorted list.');
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

procedure TGpIntegerList.SetItems(idx: integer; const value: integer);
begin
  ilList.Items[idx] := pointer(value);
end; { TGpIntegerList.SetItems }

procedure TGpIntegerList.SetSorted(const value: boolean);
begin
  if ilSorted <> value then begin
    if value then
      CustomSort(IntegerListCompare);
    ilSorted := value;
  end;
end; { TGpIntegerList.SetSorted }

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
function TGpIntegerList.Slice(idxFrom, idxTo: integer):
  TGpIntegerListSliceEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpIntegerListSliceEnumeratorFactory.Create(Self, idxFrom, idxTo);
end; { TGpIntegerList.Slice }
{$ENDIF GpLists_Enumerators}

procedure TGpIntegerList.Sort;
begin
  Sorted := false;
  Sorted := true;
end; { TGpIntegerList.Sort }

procedure TGpIntegerList.UnregisterNotification(notificationHandler:
  TGpListNotificationEvent);
begin
  ilNotificationHandlers.Remove(TMethod(notificationHandler));
end; { TGpIntegerList.UnregisterNotification }

{$IFDEF GpLists_Enumerators}
function TGpIntegerList.Walk(idxFrom, idxTo: integer):
  TGpIntegerListWalkEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpIntegerListWalkEnumeratorFactory.Create(Self, idxFrom, idxTo);
end; { TGpIntegerList.Walk }
{$ENDIF GpLists_Enumerators}

{$IFDEF GpLists_Enumerators}

{ TGpInt64ListEnumerator }

constructor TGpInt64ListEnumerator.Create(aList: TGpInt64List; idxFrom, idxTo: integer);
begin
  ileIndex := idxFrom - 1;
  ileList := aList;
  ileIdxTo := idxTo;
end; { TGpInt64ListEnumerator.Create }

function TGpInt64ListEnumerator.GetCurrent: int64;
begin
  Result := ileList[ileIndex];
end; { TGpInt64ListEnumerator.GetCurrent }

function TGpInt64ListEnumerator.MoveNext: boolean;
begin
  Result := ileIndex < ileIdxTo;
  if Result then
    Inc(ileIndex);
end; { TGpInt64ListEnumerator.MoveNext }

{ TGpInt64ListWalkEnumerator }

constructor TGpInt64ListWalkEnumerator.Create(aList: TGpInt64List; idxFrom, idxTo:
  integer);
begin
  inherited Create;
  ilweList := aList;
  ilweIndex := idxFrom - 1;
  ilweIdxTo := idxTo;
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
      else if idxItem < ilweIdxTo then
        Inc(ilweIdxTo);
    loDelete:
      if idxItem < ilweIndex then begin
        Dec(ilweIndex);
        Dec(ilweIdxTo);
      end
      else if idxItem < ilweIdxTo then 
        Dec(ilweIdxTo);
    else raise Exception.Create('TGpInt64ListWalkEnumerator.HandleListChange: Unexpected list operation');
  end;
end; { TGpInt64ListWalkEnumerator.HandleListChange }

function TGpInt64ListWalkEnumerator.MoveNext: boolean;
begin
  Result := ilweIndex < ilweIdxTo;
  if Result then
    Inc(ilweIndex);
end; { TGpInt64ListWalkEnumerator.MoveNext }

{ TGpInt64ListSliceEnumeratorFactory }

constructor TGpInt64ListSliceEnumeratorFactory.Create(list: TGpInt64List; idxFrom,
  idxTo: integer);
begin
  sefList := list;
  sefIdxFrom := idxFrom;
  sefIdxTo := idxTo;
end; { TGpInt64ListSliceEnumeratorFactory.Create }

function TGpInt64ListSliceEnumeratorFactory.GetEnumerator: TGpInt64ListEnumerator;
begin
  Result := TGpInt64ListEnumerator.Create(sefList, sefIdxFrom, sefIdxTo);
end; { TGpInt64ListSliceEnumeratorFactory.GetEnumerator }

{ TGpInt64ListWalkEnumeratorFactory }

constructor TGpInt64ListWalkEnumeratorFactory.Create(list: TGpInt64List; idxFrom, idxTo:
  integer);
begin
  wefList := list;
  wefIdxFrom := idxFrom;
  wefIdxTo := idxTo;
end; { TGpInt64ListWalkEnumeratorFactory.Create }

function TGpInt64ListWalkEnumeratorFactory.GetEnumerator: TGpInt64ListWalkEnumerator;
begin
  Result := TGpInt64ListWalkEnumerator.Create(wefList, wefIdxFrom, wefIdxTo);
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

constructor TGpInt64List.Create(elements: array of int64);
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
        dupError : ilList.Error(SDuplicateItem, item);
      end;
    InsertItem(Result, item);
  end;
end; { TGpInt64List.Add }

procedure TGpInt64List.Append(elements: array of int64);
var
  iElement: integer;
begin
  SetCapacity(Length(elements));
  for iElement := Low(elements) to High(elements) do
    Add(elements[iElement]);
end; { TGpInt64List.Append }

procedure TGpInt64List.Append(list: TGpInt64List);
var
  iItem: integer;
begin
  SetCapacity(list.Count);
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpInt64List.Append }

procedure TGpInt64List.Append(list: TGpIntegerList);
var
  iItem: integer;
begin
  SetCapacity(list.Count);
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

procedure TGpInt64List.Assign(elements: array of int64);
begin
  Clear;
  Append(elements);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Assign(list: TGpInt64List);
begin
  Clear;
  Append(list);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Assign(list: TGpIntegerList);
begin
  Clear;
  Append(list);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Clear;
begin
  ilList.Clear;
end; { TGpInt64List.Clear }

function TGpInt64List.Contains(item: int64): boolean;
begin
  Result := (IndexOf(item) >= 0);
end; { TGpInt64List.Contains }

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
  pList: PLargeInteger;
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

{$IFDEF GpLists_Enumerators}
function TGpInt64List.GetEnumerator: TGpInt64ListEnumerator;
begin
  Result := TGpInt64ListEnumerator.Create(Self, 0, Count - 1);
end; { TGpInt64List.GetEnumerator }
{$ENDIF GpLists_Enumerators}

function TGpInt64List.GetItems(idx: integer): int64;
begin
  Int64Rec(Result).Lo := cardinal(ilList.Items[2*idx]);
  Int64Rec(Result).Hi := cardinal(ilList.Items[2*idx+1]);
end; { TGpInt64List.GetItems }

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
  pList   : {$IFDEF GpLists_RequiresD6CompilerHack} PInteger64 {$ELSE} PLargeInteger {$ENDIF};
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
function TGpInt64List.Slice(idxFrom, idxTo: integer): TGpInt64ListSliceEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpInt64ListSliceEnumeratorFactory.Create(Self, idxFrom, idxTo);
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
function TGpInt64List.Walk(idxFrom, idxTo: integer):
  TGpInt64ListWalkEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpInt64ListWalkEnumeratorFactory.Create(Self, idxFrom, idxTo);
end; { TGpInt64List.Walk }
{$ENDIF GpLists_Enumerators}

{$IFDEF GpLists_Enumerators}

{ TGpIntegerObjectListWalkKVEnumerator }

constructor TGpIntegerObjectListWalkKVEnumerator.Create(aList: TGpIntegerObjectList;
  idxFrom, idxTo: integer);
begin
  inherited Create;
  wkeListEnumerator := TGpIntegerListWalkEnumerator.Create(aList, idxFrom, idxTo);
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
  TGpIntegerObjectList; idxFrom, idxTo: integer);
begin
  wkefList := list;
  wkefIdxFrom := idxFrom;
  wkefIdxTo := idxTo;
end; { TGpIntegerObjectListWalkKVEnumeratorFactory.Create }

function TGpIntegerObjectListWalkKVEnumeratorFactory.GetEnumerator:
  TGpIntegerObjectListWalkKVEnumerator;
begin
  Result := TGpIntegerObjectListWalkKVEnumerator.Create(wkefList, wkefIdxFrom, wkefIdxTo);
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
    'TGpIntegerObjectList.AddObject: Number of items and objects differ');
end; { TGpIntegerObjectList.AddObject }                    

procedure TGpIntegerObjectList.Clear;
begin
  inherited;
  iolObjects.Clear;
end; { TGpIntegerObjectList.Clear }

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
function TGpIntegerObjectList.WalkKV(idxFrom, idxTo: integer):
  TGpIntegerObjectListWalkKVEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpIntegerObjectListWalkKVEnumeratorFactory.Create(Self, idxFrom, idxTo);
end; { TGpIntegerObjectList.WalkKV }
{$ENDIF GpLists_Enumerators}

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

function TGpCountedIntegerList.Ensure(item, count: integer): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item, count)
  else
    Counter[Result] := count;
end; { TGpCountedIntegerList.Ensure }

function TGpCountedIntegerList.GetCounter(idx: integer): integer;
begin
  Result := integer(Objects[idx]);
end; { TGpCountedIntegerList.GetCounter }

function TGpCountedIntegerList.GetItemCounter(item: integer): integer;
begin
  Result := Counter[IndexOf(item)];
end; { TGpCountedInt64List.GetItemCounter }

procedure TGpCountedIntegerList.SetCounter(idx: integer; const value: integer);
begin
  Objects[idx] := TObject(value);
end; { TGpCountedIntegerList.SetCounter }

procedure TGpCountedIntegerList.SetItemCounter(item: integer; const value: integer);
begin
  Counter[IndexOf(item)] := value;
end; { TGpCountedInt64List.SetItemCounter }

procedure TGpCountedIntegerList.SortByCounter(descending: boolean);
begin
  Sorted := false;
  if descending then
    CustomSort(CompareDescending_CIL)
  else
    CustomSort(CompareAscending_CIL);
  Sorted := false;
end; { TGpCountedIntegerList.SortByCounter }

{$IFDEF GpLists_Enumerators}

{ TGpInt64ObjectListWalkKVEnumerator }

constructor TGpInt64ObjectListWalkKVEnumerator.Create(aList: TGpInt64ObjectList; idxFrom,
  idxTo: integer);
begin
  inherited Create;
  wkeListEnumerator := TGpInt64ListWalkEnumerator.Create(aList, idxFrom, idxTo);
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
  idxFrom, idxTo: integer);
begin
  wkefList := list;
  wkefIdxFrom := idxFrom;
  wkefIdxTo := idxTo;
end; { TGpInt64ObjectListWalkKVEnumeratorFactory.Create }

function TGpInt64ObjectListWalkKVEnumeratorFactory.GetEnumerator:
  TGpInt64ObjectListWalkKVEnumerator;
begin
  Result := TGpInt64ObjectListWalkKVEnumerator.Create(wkefList, wkefIdxFrom, wkefIdxTo);
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
function TGpInt64ObjectList.WalkKV(idxFrom, idxTo: integer):
  TGpInt64ObjectListWalkKVEnumeratorFactory;
begin
  if idxTo = CUpperListBound then
    idxTo := Count - 1;
  Result := TGpInt64ObjectListWalkKVEnumeratorFactory.Create(Self, idxFrom, idxTo);
end; { TGpInt64ObjectList.WalkKV }
{$ENDIF GpLists_Enumerators}

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

function TGpCountedInt64List.Ensure(item: int64; count: int64): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item, count)
  else
    Counter[Result] := count;
end; { TGpCountedInt64List.Ensure }

function TGpCountedInt64List.GetCounter(idx: integer): int64;
begin
  Result := TGpInt64(Objects[idx]).Value;
end; { TGpCountedInt64List.GetCounter }

function TGpCountedInt64List.GetItemCounter(item: int64): int64;
begin
  Result := Counter[IndexOf(item)];
end; { TGpCountedInt64List.GetItemCounter }

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

{$IFDEF GpLists_TStringListHelper}

{ TGpStringListHelper }

function TGpStringListHelper.Contains(const s: string): boolean;
begin
  Result := (IndexOf(s) >= 0);
end; { TGpStringListHelper.Contains }

function TGpStringListHelper.FetchObject(const s: string): TObject;
var
  idxItem: integer;
begin
  idxItem := IndexOf(s);
  if idxItem >= 0 then
    Result := Objects[idxItem]
  else
    Result := nil;
end; { TGpStringListHelper.FetchObject }

procedure TGpStringListHelper.FreeObjects;
var
  iObject: integer;
begin
  for iObject := 0 to Count - 1 do begin
    Objects[iObject].Free;
    Objects[iObject] := nil;
  end;
end; { TGpStringListHelper.FreeObjects }

function TGpStringListHelper.Last: string;
begin
  Result := Strings[Count - 1];
end; { TGpStringListHelper.Last }

procedure TGpStringListHelper.Remove(const s: string);
var
  idxItem: integer;
begin
  idxItem := IndexOf(s);
  if idxItem >= 0 then
    Delete(idxItem);
end; { TGpStringListHelper.Remove }

procedure TGpStringListHelper.Sort;
begin
  Sorted := false;
  Sorted := true;
end; { TGpStringListHelper.Sort }

{$ENDIF GpLists_TStringListHelper}

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

function TGpCountedStringList.Contains(const s: string): boolean;
begin
  Result := (IndexOf(s) >= 0);
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

function TGpTMethodList.Contains(item: TMethod): boolean;
begin
  Result := (IndexOf(item) >= 0);
end; { TGpTMethodList.Contains }

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

function TGpClassList.CreateObject(sClass: string): TObject;
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

function TGpClassList.IndexOf(sClass: string): integer;
begin
  Result := clClasses.IndexOf(sClass);
end; { TGpClassList.IndexOf }

procedure TGpClassList.Remove(sClass: string);
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

{ TGpObjectRingBuffer }

constructor TGpObjectRingBuffer.Create(bufferSize: integer; ownsObjects,
  multithreaded: boolean);
begin
  if bufferSize = 0 then
    raise Exception.Create('TGpObjectRingBuffer.Create: buffer size is 0');
  if multithreaded then
    orbLock := TSpinLock.Create;
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

{:Removes tail object from the buffer, without destroying it. Returns nil if
  buffer is empty.
  @since   2003-07-26
}
function TGpObjectRingBuffer.Dequeue: TObject;
begin
  Lock;
  try
    if IsEmpty then
      Result := nil
    else begin
      Result := orbBuffer[orbTail];
      orbTail := IncPointer(orbTail);
      Dec(orbCount);
      if (BufferAlmostEmptyEvent <> 0) and (BufferAlmostEmptyThreshold = orbCount) then
        Win32Check(SetEvent(BufferAlmostEmptyEvent));
    end;
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

{ TGpObjectMap }

constructor TGpObjectMap.Create(ownsObjects: boolean);
begin
  inherited Create;
  omList := TGpIntegerObjectList.Create(ownsObjects);
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
  idxItem := omList.IndexOf(integer(item));
  if idxItem >= 0 then
    Result := omList.Objects[idxItem]
  else
    Result := nil;
end; { TGpObjectMap.GetItems }

procedure TGpObjectMap.SetItems(item: TObject; const value: TObject);
var
  idxItem: integer;
begin
  idxItem := omList.IndexOf(integer(item));
  if idxItem >= 0 then begin
    if assigned(value) then
      omList.Objects[idxItem] := value
    else
      omList.Delete(idxItem);
  end
  else
    omList.AddObject(integer(item), value);
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

procedure TGpObjectObjectMap.SetItems(item1, item2: TObject;
  const Value: TObject);
begin
  Map(item1)[item2] := Value;
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

{$ENDIF GpLists_Enumerators}

{ TGpDoublyLinkedList }

constructor TGpDoublyLinkedList.Create(multithreaded: boolean);
begin
  inherited Create;
  if multithreaded then
    dllLock := TSpinLock.Create;
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

{:Destroy all elements of the list.
  @since   2005-06-02
}
procedure TGpDoublyLinkedList.FreeAll;
begin
  Lock;
  try
    while TailUnsafe <> nil do
      TailUnsafe.Free;
  finally Unlock; end;
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
  Lock;
  try
    Result := HeadUnsafe;
  finally Unlock; end;
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
  Lock;
  try
    obj.LinkAfter(Self, existingObject);
  finally Unlock; end;
end; { TGpDoublyLinkedList.InsertAfter }

procedure TGpDoublyLinkedList.InsertAtHead(obj: TGpDoublyLinkedListObject);
begin
  Lock;
  try
    InsertAfter(dllHead, obj);
  finally Unlock; end;
end; { TGpDoublyLinkedList.InsertAtHead }

procedure TGpDoublyLinkedList.InsertAtTail(obj: TGpDoublyLinkedListObject);
begin
  Lock;
  try
    InsertBefore(dllTail, obj);
  finally Unlock; end;
end; { TGpDoublyLinkedList.InsertAtTail }

procedure TGpDoublyLinkedList.InsertBefore(existingObject,
  obj: TGpDoublyLinkedListObject);
begin
  InsertAfter(existingObject.dlloPrevious, obj);
end; { TGpDoublyLinkedList.InsertBefore }

function TGpDoublyLinkedList.IsEmpty: boolean;
begin
  Lock;
  try
    Result := (dllHead.dlloNext = dllTail);
  finally Unlock; end;
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
  Lock;
  try
    Result := HeadUnsafe;
    if assigned(Result) then
      Result.Unlink;
  finally Unlock; end;
end; { TGpDoublyLinkedList.RemoveFromHead }

function TGpDoublyLinkedList.RemoveFromTail: TGpDoublyLinkedListObject;
begin
  Lock;
  try
    Result := TailUnsafe;
    if assigned(Result) then
      Result.Unlink;
  finally Unlock; end;
end; { TGpDoublyLinkedList.RemoveFromTail }

function TGpDoublyLinkedList.Tail: TGpDoublyLinkedListObject;
begin
  Lock;
  try
    Result := TailUnsafe;
  finally Unlock; end;
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
  Lock;
  try
    while TailUnsafe <> nil do
      RemoveFromTail;
  finally Unlock; end;
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

end.

