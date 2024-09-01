//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.VectorLists;

(* Misc. lists of vectors and entities *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.PersistentClasses;

type
  TBaseListOption = (bloExternalMemory, bloSetCountResetsMemory);
  TBaseListOptions = set of TBaseListOption;

  // Base class for lists, introduces common behaviours.
  TgxBaseList = class(TgxPersistentObject)
  private
    FCount: Integer;
    FCapacity: Integer;
    FGrowthDelta: Integer;
    FBufferItem: PByteArray;
    FOptions: TBaseListOptions;
    FRevision: LongWord;
    FTagString: string;
  protected
     // The base list pointer (untyped)
    FBaseList: PByteArray;
    // Must be defined by all subclasses in their constructor(s)
    FItemSize: Integer;
    procedure SetCount(Val: Integer);
    (*Only function where list may be alloc'ed & freed.
    Resizes the array pointed by FBaseList, adjust the subclass's
    typed pointer accordingly if any. *)
    procedure SetCapacity(NewCapacity: Integer); virtual;
    function BufferItem: PByteArray;
    function GetSetCountResetsMemory: Boolean;
    procedure SetSetCountResetsMemory(const Val: Boolean);
    // Borland-style persistency support.
    procedure ReadItemsData(AReader : TReader); virtual;
    procedure WriteItemsData(AWriter : TWriter); virtual;
    procedure DefineProperties(AFiler: TFiler); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Src: TPersistent); override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure AddNulls(nbVals: Cardinal);
    procedure InsertNulls(Index: Integer; nbVals: Cardinal);
    procedure AdjustCapacityToAtLeast(const size: Integer);
    function DataSize: Integer;
    (* Tell the list to use the specified range instead of its own.
     rangeCapacity should be expressed in bytes.
     The allocated memory is NOT managed by the list, current content
     if copied to the location, if the capacity is later changed, regular
     memory will be allocated, and the specified range no longer used. *)
    procedure UseMemory(rangeStart: Pointer; rangeCapacity: Integer);
    // Empties the list without altering capacity.
    procedure Flush;
    // Empties the list and release.
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure DeleteItems(Index: Integer; nbVals: Cardinal);
    procedure Exchange(index1, index2: Integer);
    procedure Move(curIndex, newIndex: Integer);
    procedure Reverse;
    // Nb of items in the list. When assigning a Count, added items are reset to zero.
    property Count: Integer read FCount write SetCount;
    // Current list capacity. Not persistent.
    property Capacity: Integer read FCapacity write SetCapacity;
    // List growth granularity. Not persistent.
    property GrowthDelta: Integer read FGrowthDelta write FGrowthDelta;
    (* If true (default value) adjusting count will reset added values.
     Switching this option to true will turn off this memory reset,
     which can improve performance is that having empty values isn't required. *)
    property SetCountResetsMemory: Boolean read GetSetCountResetsMemory write SetSetCountResetsMemory;
    property TagString: string read FTagString write FTagString;
    // Increase by one after every content changes.
    property Revision: LongWord read FRevision write FRevision;
  end;

  // Base class for vector lists, introduces common behaviours.
  TgxBaseVectorList = class(TgxBaseList)
  protected
    function GetItemAddress(Index: Integer): PFloatArray;
  public
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure GetExtents(out min, max: TAffineVector); virtual;
    function Sum: TAffineVector; virtual;
    procedure Normalize; virtual;
    function MaxSpacing(list2: TgxBaseVectorList): Single; virtual;
    procedure Translate(const delta: TAffineVector); overload; virtual;
    procedure Translate(const delta: TgxBaseVectorList); overload; virtual;
    procedure TranslateInv(const delta: TgxBaseVectorList); overload; virtual;
    (* Replace content of the list with lerp results between the two given lists.
     Note: you can't Lerp with Self!!! *)
    procedure Lerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single); virtual; abstract;
    (* Replace content of the list with angle lerp between the two given lists.
    Note: you can't Lerp with Self!!! *)
    procedure AngleLerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single);
    procedure AngleCombine(const list1: TgxBaseVectorList; intensity: Single);
    (* Linear combination of Self with another list.
     Self[i]:=Self[i]+list2[i]*factor *)
    procedure Combine(const list2: TgxBaseVectorList; factor: Single); virtual;
    property ItemAddress[Index: Integer]: PFloatArray read GetItemAddress;
  end;

  (* A list of TAffineVector.
   Similar to TList, but using TAffineVector as items.
   The list has stack-like push/pop methods. *)
  TgxAffineVectorList = class(TgxBaseVectorList)
  private
    FList: PAffineVectorArray;
  protected
    function Get(Index: Integer): TAffineVector;
    procedure Put(Index: Integer; const item: TAffineVector);
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create; override;
    procedure Assign(Src: TPersistent); override;
    function Add(const item: TAffineVector): Integer; overload;
    function Add(const item: TVector4f): Integer; overload;
    procedure Add(const i1, i2: TAffineVector); overload;
    procedure Add(const i1, i2, i3: TAffineVector); overload;
    function Add(const item: TVector2f): Integer; overload;
    function Add(const item: TTexPoint): Integer; overload;
    function Add(const X, Y: Single): Integer; overload;
    function Add(const X, Y, Z: Single): Integer; overload;
    function Add(const X, Y, Z: Integer): Integer; overload;
    // Add (3 ints, no capacity check)
    function AddNC(const X, Y, Z: Integer): Integer; overload;
    // Add (2 ints in array + 1)
    function Add(const xy: PIntegerArray; const Z: Integer): Integer; overload;
    // AddNC (2 ints in array + 1, no capacity check)
    function AddNC(const xy: PIntegerArray; const Z: Integer): Integer; overload;
    procedure Add(const list: TgxAffineVectorList); overload;
    procedure Push(const Val: TAffineVector);
    function Pop: TAffineVector;
    procedure Insert(Index: Integer; const item: TAffineVector);
    function IndexOf(const item: TAffineVector): Integer;
    function FindOrAdd(const item: TAffineVector): Integer;
    property Items[Index: Integer]: TAffineVector read Get write Put; default;
    property List: PAffineVectorArray read FList;
    procedure Translate(const delta: TAffineVector); overload; override;
    procedure Translate(const delta: TAffineVector; base, nb: Integer); overload;
    // Translates the given item
    procedure TranslateItem(Index: Integer; const delta: TAffineVector);
    // Translates given items
    procedure TranslateItems(Index: Integer; const delta: TAffineVector; nb: Integer);
    // Combines the given item
    procedure CombineItem(Index: Integer; const vector: TAffineVector; const f: Single);
    {Transforms all items by the matrix as if they were points. 
     ie. the translation component of the matrix is honoured. }
    procedure TransformAsPoints(const matrix: TMatrix4f);
    {Transforms all items by the matrix as if they were vectors. 
     ie. the translation component of the matrix is not honoured. }
    procedure TransformAsVectors(const matrix: TMatrix4f); overload;
    procedure TransformAsVectors(const matrix: TAffineMatrix); overload;
    procedure Normalize; override;
    procedure Lerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single); override;
    procedure Scale(factor: Single); overload;
    procedure Scale(const factors: TAffineVector); overload;
  end;

  (* A list of TVector4f.
   Similar to TList, but using TVector4f as items.
   The list has stack-like push/pop methods. *)
  TgxVectorList = class(TgxBaseVectorList)
  private
    FList: PVectorArray;
  protected
    function Get(Index: Integer): TVector4f;
    procedure Put(Index: Integer; const item: TVector4f);
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create; override;
    procedure Assign(Src: TPersistent); override;
    function Add(const item: TVector4f): Integer; overload;
    function Add(const item: TAffineVector; w: Single): Integer; overload;
    function Add(const X, Y, Z, w: Single): Integer; overload;
    procedure Add(const i1, i2, i3: TAffineVector; w: Single); overload;
    function AddVector(const item: TAffineVector): Integer; overload;
    function AddPoint(const item: TAffineVector): Integer; overload;
    function AddPoint(const X, Y: Single; const Z: Single = 0): Integer; overload;
    procedure Push(const Val: TVector4f);
    function Pop: TVector4f;
    function IndexOf(const item: TVector4f): Integer;
    function FindOrAdd(const item: TVector4f): Integer;
    function FindOrAddPoint(const item: TAffineVector): Integer;
    procedure Insert(Index: Integer; const item: TVector4f);
    property Items[Index: Integer]: TVector4f read Get write Put; default;
    property List: PVectorArray read FList;
    procedure Lerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single); override;
  end;

  (* A list of TTexPoint.
   Similar to TList, but using TTexPoint as items.
   The list has stack-like push/pop methods. *)
  TgxTexPointList = class(TgxBaseVectorList)
  private
    FList: PTexPointArray;
  protected
    function Get(Index: Integer): TTexPoint;
    procedure Put(Index: Integer; const item: TTexPoint);
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create; override;
    procedure Assign(Src: TPersistent); override;
    function IndexOf(const item: TTexpoint): Integer;
    function FindOrAdd(const item: TTexpoint): Integer;
    function Add(const item: TTexPoint): Integer; overload;
    function Add(const item: TVector2f): Integer; overload;
    function Add(const texS, Text: Single): Integer; overload;
    function Add(const texS, Text: Integer): Integer; overload;
    function AddNC(const texS, Text: Integer): Integer; overload;
    function Add(const texST: PIntegerArray): Integer; overload;
    function AddNC(const texST: PIntegerArray): Integer; overload;
    procedure Push(const Val: TTexPoint);
    function Pop: TTexPoint;
    procedure Insert(Index: Integer; const item: TTexPoint);
    property Items[Index: Integer]: TTexPoint read Get write Put; default;
    property List: PTexPointArray read FList;
    procedure Translate(const delta: TTexPoint);
    procedure ScaleAndTranslate(const scale, delta: TTexPoint); overload;
    procedure ScaleAndTranslate(const scale, delta: TTexPoint; base, nb: Integer); overload;
    procedure Lerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single); override;
  end;

  (* A list of Integers.
   Similar to TList, but using TTexPoint as items.
   The list has stack-like push/pop methods. *)
  TgxIntegerList = class(TgxBaseList)
  private
    FList: PIntegerArray;
  protected
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; const item: Integer);
    procedure SetCapacity(newCapacity: Integer); override;
  public
    constructor Create; override;
    procedure Assign(src: TPersistent); override;
    function Add(const item: Integer): Integer; overload;
    function AddNC(const item: Integer): Integer; overload;
    procedure Add(const i1, i2: Integer); overload;
    procedure Add(const i1, i2, i3: Integer); overload;
    procedure Add(const AList: TgxIntegerList); overload;
    procedure Push(const Val: Integer);
    function Pop: Integer;
    procedure Insert(Index: Integer; const item: Integer);
    procedure Remove(const item: Integer);
    function IndexOf(item: Integer): Integer;
    property Items[Index: Integer]: Integer read Get write Put; default;
    property List: PIntegerArray read FList;
    (* Adds count items in an arithmetic serie.
      Items are (aBase), (aBase+aDelta) ... (aBase+(aCount-1)*aDelta) *)
    procedure AddSerie(aBase, aDelta, aCount: Integer);
    // Add n integers at the address starting at (and including) first.
    procedure AddIntegers(const First: PInteger; n: Integer); overload;
    // Add all integers from aList into the list.
    procedure AddIntegers(const aList: TgxIntegerList); overload;
    // Add all integers from anArray into the list.
    procedure AddIntegers(const anArray: array of Integer); overload;
    // Returns the minimum integer item, zero if list is empty.
    function MinInteger: Integer;
    // Returns the maximum integer item, zero if list is empty.
    function MaxInteger: Integer;
    // Sort items in ascending order.
    procedure Sort;
    // Sort items in ascending order and remove duplicated integers.
    procedure SortAndRemoveDuplicates;
    // Locate a value in a sorted list.
    function BinarySearch(const Value: Integer): Integer; overload;
    (* Locate a value in a sorted list.
      If ReturnBestFit is set to true, the routine will return the position
      of the largest value that's smaller than the sought value. Found will
      be set to True if the exact value was found, False if a "BestFit" was found. *)
    function BinarySearch(const Value: Integer; returnBestFit: Boolean; var found: Boolean): Integer; overload;
    (* Add integer to a sorted list.
      Maintains the list sorted. If you have to add "a lot" of integers
      at once, use the Add method then Sort the list for better performance. *)
    function AddSorted(const Value: Integer; const ignoreDuplicates: Boolean = False): Integer;
    // Removes an integer from a sorted list.
    procedure RemoveSorted(const Value: Integer);
    // Adds delta to all items in the list.
    procedure Offset(delta: Integer); overload;
    procedure Offset(delta: Integer; const base, nb: Integer); overload;
  end;

  TSingleArrayList = array[0..MaxInt shr 4] of Single;
  PSingleArrayList = ^TSingleArrayList;

  (* A list of Single.
   Similar to TList, but using Single as items.
   The list has stack-like push/pop methods. *)
  TgxSingleList = class(TgxBaseList)
  private
    FList: PSingleArrayList;
  protected
    function Get(Index: Integer): Single;
    procedure Put(Index: Integer; const item: Single);
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create; override;
    procedure Assign(Src: TPersistent); override;
    function Add(const item: Single): Integer; overload;
    procedure Add(const i1, i2: Single); overload;
    procedure AddSingles(const First: PSingle; n: Integer); overload;
    procedure AddSingles(const anArray: array of Single); overload;
    procedure Push(const Val: Single);
    function Pop: Single;
    procedure Insert(Index: Integer; const item: Single);
    property Items[Index: Integer]: Single read Get write Put; default;
    property List: PSingleArrayList read FList;
    procedure AddSerie(aBase, aDelta: Single; aCount: Integer);
    // Adds delta to all items in the list.
    procedure Offset(delta: Single); overload;
    (*  Adds to each item the corresponding item in the delta list.
       Performs 'Items[i]:=Items[i]+delta[i]'.
       If both lists don't have the same item count, an exception is raised. *)
    procedure Offset(const delta: TgxSingleList); overload;
    //  Multiplies all items by factor.
    procedure Scale(factor: Single);
    // Square all items.
    procedure Sqr;
    // SquareRoot all items.
    procedure Sqrt;
    // Computes the sum of all elements.
    function Sum: Single;
    function Min: Single;
    function Max: Single;
  end;

  TDoubleArrayList = array[0..MaxInt shr 4] of Double;
  PDoubleArrayList = ^TDoubleArrayList;
  (*  A list of Double.
    Similar to TList, but using Double as items.
    The list has stack-like push/pop methods. *)
  TgxDoubleList = class(TgxBaseList)
  private
    FList: PDoubleArrayList;
  protected
    function Get(Index: Integer): Double;
    procedure Put(Index: Integer; const item: Double);
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create; override;
    procedure Assign(Src: TPersistent); override;
    function Add(const item: Double): Integer;
    procedure Push(const Val: Double);
    function Pop: Double;
    procedure Insert(Index: Integer; const item: Double);
    property Items[Index: Integer]: Double read Get write Put; default;
    property List: PDoubleArrayList read FList;
    procedure AddSerie(aBase, aDelta: Double; aCount: Integer);
    // Adds delta to all items in the list.
    procedure Offset(delta: Double); overload;
    (* Adds to each item the corresponding item in the delta list.
      Performs 'Items[i]:=Items[i]+delta[i]'.
      If both lists don't have the same item count, an exception is raised. *)
    procedure Offset(const delta: TgxDoubleList); overload;
    // Multiplies all items by factor.
    procedure Scale(factor: Double);
    // Square all items.
    procedure Sqr;
    // SquareRoot all items.
    procedure Sqrt;
    // Computes the sum of all elements.
    function Sum: Double;
    function Min: Single;
    function Max: Single;
  end;

  // A list of bytes. Similar to TList, but using Byte as items.
  TByteList = class(TgxBaseList)
  private
    FList: PByteArray;
  protected
    function Get(Index: Integer): Byte;
    procedure Put(Index: Integer; const item: Byte);
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create; override;
    procedure Assign(Src: TPersistent); override;
    function Add(const item: Byte): Integer;
    procedure Insert(Index: Integer; const item: Byte);
    property Items[Index: Integer]: Byte read Get write Put; default;
    property List: PByteArray read FList;
  end;

  (* A list of TQuaternion.
    Similar to TList, but using TQuaternion as items.
    The list has stack-like push/pop methods. *)
  TQuaternionList = class(TgxBaseVectorList)
  private
    FList: PQuaternionArray;
  protected
    function Get(Index: Integer): TQuaternion;
    procedure Put(Index: Integer; const item: TQuaternion);
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create; override;
    procedure Assign(Src: TPersistent); override;
    function Add(const item: TQuaternion): Integer; overload;
    function Add(const item: TAffineVector; w: Single): Integer; overload;
    function Add(const X, Y, Z, W: Single): Integer; overload;
    procedure Push(const Val: TQuaternion);
    function Pop: TQuaternion;
    function IndexOf(const item: TQuaternion): Integer;
    function FindOrAdd(const item: TQuaternion): Integer;
    procedure Insert(Index: Integer; const item: TQuaternion);
    property Items[Index: Integer]: TQuaternion read Get write Put; default;
    property List: PQuaternionArray read FList;
    // Lerps corresponding quaternions from both lists using QuaternionSlerp.
    procedure Lerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single); override;
    (* Multiplies corresponding quaternions after the second quaternion is
      slerped with the IdentityQuaternion using factor. This allows for weighted
      combining of rotation transforms using quaternions. *)
    procedure Combine(const list2: TgxBaseVectorList; factor: Single); override;
  end;

  // 4 byte union contain access like Integer, Single and four Byte
	T4ByteData = packed record
    case Byte of
    0 : (Bytes : record Value : array[0..3] of Byte; end);
    1 : (Int   : record Value : Integer; end);
    2 : (UInt  : record Value : Cardinal; end);
    3 : (Float : record Value : Single; end);
    4 : (Word  : record Value : array[0..1] of Word; end);
  end;

  T4ByteArrayList = array[0..MaxInt shr 4] of T4ByteData;
  P4ByteArrayList = ^T4ByteArrayList;

  // A list of T4ByteData.
  Tgx4ByteList = class(TgxBaseList)
  private
    FList: P4ByteArrayList;
  protected
    function  Get(Index: Integer): T4ByteData;
    procedure Put(Index: Integer; const item: T4ByteData);
    procedure SetCapacity(NewCapacity: Integer); override;
  public
    constructor Create; override;
    procedure Assign(Src: TPersistent); override;
    function  Add(const item: T4ByteData): Integer; overload;
    procedure Add(const i1: Single); overload;
    procedure Add(const i1, i2: Single); overload;
    procedure Add(const i1, i2, i3: Single); overload;
    procedure Add(const i1, i2, i3, i4: Single); overload;
    procedure Add(const i1: Integer); overload;
    procedure Add(const i1, i2: Integer); overload;
    procedure Add(const i1, i2, i3: Integer); overload;
    procedure Add(const i1, i2, i3, i4: Integer); overload;
    procedure Add(const i1: Cardinal); overload;
    procedure Add(const i1, i2: Cardinal); overload;
    procedure Add(const i1, i2, i3: Cardinal); overload;
    procedure Add(const i1, i2, i3, i4: Cardinal); overload;
    procedure Add(const AList: Tgx4ByteList); overload;
    procedure Push(const Val: T4ByteData);
    function  Pop: T4ByteData;
    procedure Insert(Index: Integer; const item: T4ByteData);
    property Items[Index: Integer]: T4ByteData read Get write Put; default;
    property List: P4ByteArrayList read FList;
  end;

  TgxLongWordList = class(TgxBaseList)
  private
    FList: PLongWordArray;
  protected
    function Get(Index: Integer): LongWord;
    procedure Put(Index: Integer; const item: LongWord);
    procedure SetCapacity(newCapacity: Integer); override;
  public
    constructor Create; override;
    procedure Assign(src: TPersistent); override;
    function Add(const item: LongWord): Integer; overload;
    function AddNC(const item: LongWord): Integer; overload;
    procedure Add(const i1, i2: LongWord); overload;
    procedure Add(const i1, i2, i3: LongWord); overload;
    procedure Add(const AList: TgxLongWordList); overload;
    procedure Push(const Val: LongWord);
    function Pop: LongWord;
    procedure Insert(Index: Integer; const item: LongWord);
    procedure Remove(const item: LongWord);
    function IndexOf(item: Integer): LongWord;
    property Items[Index: Integer]: LongWord read Get write Put; default;
    property List: PLongWordArray read FList;
    // Add n integers at the address starting at (and including) first.
    procedure AddLongWords(const First: PLongWord; n: Integer); overload;
    // Add all integers from aList into the list.
    procedure AddLongWords(const aList: TgxLongWordList); overload;
    // Add all integers from anArray into the list.
    procedure AddLongWords(const anArray: array of LongWord); overload;
  end;

// Sort the refList in ascending order, ordering objList (TList) on the way.
procedure QuickSortLists(startIndex, endIndex: Integer; refList: TgxSingleList; objList: TList); overload;
// Sort the refList in ascending order, ordering objList (TgxBaseList) on the way.
procedure QuickSortLists(startIndex, endIndex: Integer; refList: TgxSingleList; objList: TgxBaseList); overload;
(* Sort the refList in ascending order, ordering objList on the way.
   Use if, and *ONLY* if refList contains only values superior or equal to 1. *)
procedure FastQuickSortLists(startIndex, endIndex: Integer; const refList: TgxSingleList; const objList: TgxPersistentObjectList);

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cDefaultListGrowthDelta = 16;

procedure QuickSortLists(startIndex, endIndex: Integer; refList: TgxSingleList; objList: TList);
var
  I, J: Integer;
  P:    Single;
begin
  if endIndex - startIndex > 1 then
  begin
    repeat
      I := startIndex;
      J := endIndex;
      P := refList.List^[(I + J) shr 1];
      repeat
        while Single(refList.List^[I]) < P do
          Inc(I);
        while Single(refList.List^[J]) > P do
          Dec(J);
        if I <= J then
        begin
          refList.Exchange(I, J);
          objList.Exchange(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if startIndex < J then
        QuickSortLists(startIndex, J, refList, objList);
      startIndex := I;
    until I >= endIndex;
  end
  else
  if endIndex - startIndex > 0 then
  begin
    p := refList.List^[startIndex];
    if refList.List^[endIndex] < p then
    begin
      refList.Exchange(startIndex, endIndex);
      objList.Exchange(startIndex, endIndex);
    end;
  end;
end;

procedure QuickSortLists(startIndex, endIndex: Integer; refList: TgxSingleList; objList: TgxBaseList);
var
  I, J: Integer;
  P:    Single;
begin
  if endIndex - startIndex > 1 then
  begin
    repeat
      I := startIndex;
      J := endIndex;
      P := refList.List^[(I + J) shr 1];
      repeat
        while Single(refList.List^[I]) < P do
          Inc(I);
        while Single(refList.List^[J]) > P do
          Dec(J);
        if I <= J then
        begin
          refList.Exchange(I, J);
          objList.Exchange(I, J);
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if startIndex < J then
        QuickSortLists(startIndex, J, refList, objList);
      startIndex := I;
    until I >= endIndex;
  end
  else
  if endIndex - startIndex > 0 then
  begin
    p := refList.List^[startIndex];
    if refList.List^[endIndex] < p then
    begin
      refList.Exchange(startIndex, endIndex);
      objList.Exchange(startIndex, endIndex);
    end;
  end;
end;

procedure FastQuickSortLists(startIndex, endIndex: Integer; const refList: TgxSingleList; const objList: TgxPersistentObjectList);
var
  I, J:    Integer;
  p, Temp: Integer;
  ppl:     PIntegerArray;
  oTemp    : Pointer;
  oppl     : PPointerArray;
begin
  // All singles are >=1, so IEEE format allows comparing them as if they were integers
  ppl := PIntegerArray(@refList.List[0]);
  oppl := PPointerArray(objList.List);
  if endIndex > startIndex + 1 then
  begin
    repeat
      I := startIndex;
      J := endIndex;
      p := PInteger(@refList.List[(I + J) shr 1])^;
      repeat
        while ppl^[I] < p do
          Inc(I);
        while ppl^[J] > p do
          Dec(J);
        if I <= J then
        begin
          // swap integers
          Temp := ppl^[I];
          ppl^[I] := ppl^[J];
          ppl^[J] := Temp;
          // swap pointers
          oTemp := oppl^[I];
          oppl^[I] := oppl^[J];
          oppl^[J] := oTemp;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if startIndex < J then
        FastQuickSortLists(startIndex, J, refList, objList);
      startIndex := I;
    until I >= endIndex;
  end
  else
  if endIndex > startIndex then
  begin
    if ppl^[endIndex] < ppl^[startIndex] then
    begin
      I := endIndex;
      J := startIndex;
      // swap integers
      Temp := ppl^[I];
      ppl^[I] := ppl^[J];
      ppl^[J] := Temp;
      // swap pointers
      oTemp := oppl^[I];
      oppl^[I] := oppl^[J];
      oppl^[J] := oTemp;
    end;
  end;
end;

// ------------------
// ------------------ TgxBaseList ------------------
// ------------------

constructor TgxBaseList.Create;
begin
  inherited Create;
  FOptions := [bloSetCountResetsMemory];
end;

destructor TgxBaseList.Destroy;
begin
  Clear;
  if Assigned(FBufferItem) then
    FreeMem(FBufferItem);
  inherited;
end;

procedure TgxBaseList.Assign(Src: TPersistent);
begin
  if (Src is TgxBaseList) then
  begin
    SetCapacity(TgxBaseList(Src).Count);
    FGrowthDelta := TgxBaseList(Src).FGrowthDelta;
    FCount := FCapacity;
    FTagString := TgxBaseList(Src).FTagString;
    Inc(FRevision);
  end
  else
    inherited;
end;

procedure TgxBaseList.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineProperty('Items', ReadItemsData, WriteItemsData, True);
end;

procedure TgxBaseList.ReadItemsData(AReader: TReader);
var
  lData: String;
  lOutputText: string;
begin
  lOutputText := AReader.ReadString;
  SetLength(lData, Length(lOutputText) div 2 + 1);
  HexToBin(PChar(lOutputText), PAnsiChar(lData), Length(lData));
  LoadFromString(string(lData));
end;

procedure TgxBaseList.WriteItemsData(AWriter: TWriter);
var
  lData: String;
  lOutputText: String;
begin
  lData := SaveToString;
  SetLength(lOutputText, Length(lData) * 2);
  BinToHex(PAnsiChar(lData), PChar(lOutputText), Length(lData));
  AWriter.WriteString(lOutputText);
end;

procedure TgxBaseList.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(Count);
    WriteInteger(FItemSize);
    if Count > 0 then
      write(FBaseList[0], Count * FItemSize);
  end;
end;

procedure TgxBaseList.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited;
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FCount := ReadInteger;
      FItemSize := ReadInteger;
      SetCapacity(Count);
      if Count > 0 then
        read(FBaseList[0], Count * FItemSize);
    end
  else
    RaiseFilerException(archiveVersion);
  Inc(FRevision);
end;

procedure TgxBaseList.SetCount(Val: Integer);
begin
  Assert(Val >= 0);
  if Val > FCapacity then
    SetCapacity(Val);
  if (Val > FCount) and (bloSetCountResetsMemory in FOptions) then
    FillChar(FBaseList[FItemSize * FCount], (Val - FCount) * FItemSize, 0);
  FCount := Val;
  Inc(FRevision);
end;

procedure TgxBaseList.SetCapacity(newCapacity: Integer);
begin
  if newCapacity <> FCapacity then
  begin
    if bloExternalMemory in FOptions then
    begin
      Exclude(FOptions, bloExternalMemory);
      FBaseList := nil;
    end;
    ReallocMem(FBaseList, newCapacity * FItemSize);
    FCapacity := newCapacity;
    Inc(FRevision);
  end;
end;

procedure TgxBaseList.AddNulls(nbVals: Cardinal);
begin
  if Integer(nbVals) + Count > Capacity then
    SetCapacity(Integer(nbVals) + Count);
  FillChar(FBaseList[FCount * FItemSize], Integer(nbVals) * FItemSize, 0);
  FCount := FCount + Integer(nbVals);
  Inc(FRevision);
end;

procedure TgxBaseList.InsertNulls(Index: Integer; nbVals: Cardinal);
var
  nc: Integer;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if nbVals > 0 then
  begin
    nc := FCount + Integer(nbVals);
    if nc > FCapacity then
      SetCapacity(nc);
    if Index < FCount then
      System.Move(FBaseList[Index * FItemSize],
        FBaseList[(Index + Integer(nbVals)) * FItemSize],
        (FCount - Index) * FItemSize);
    FillChar(FBaseList[Index * FItemSize], Integer(nbVals) * FItemSize, 0);
    FCount := nc;
    Inc(FRevision);
  end;
end;

procedure TgxBaseList.AdjustCapacityToAtLeast(const size: Integer);
begin
  if Capacity < size then
    Capacity := size;
end;

function TgxBaseList.DataSize: Integer;
begin
  Result := FItemSize * FCount;
end;

function TgxBaseList.BufferItem: PByteArray;
begin
  if not Assigned(FBufferItem) then
    GetMem(FBufferItem, FItemSize);
  Result := FBufferItem;
end;

function TgxBaseList.GetSetCountResetsMemory: Boolean;
begin
  Result := (bloSetCountResetsMemory in FOptions);
end;

procedure TgxBaseList.SetSetCountResetsMemory(const Val: Boolean);
begin
  if Val then
    Include(FOptions, bloSetCountResetsMemory)
  else
    Exclude(FOptions, bloSetCountResetsMemory);
end;

procedure TgxBaseList.UseMemory(rangeStart: Pointer; rangeCapacity: Integer);
begin
  rangeCapacity := rangeCapacity div FItemSize;
  if rangeCapacity < FCount then
    Exit;
  // transfer data
  System.Move(FBaseList^, rangeStart^, FCount * FItemSize);
  if not (bloExternalMemory in FOptions) then
  begin
    FreeMem(FBaseList);
    Include(FOptions, bloExternalMemory);
  end;
  FBaseList := rangeStart;
  FCapacity := rangeCapacity;
  SetCapacity(FCapacity); // notify subclasses
end;

procedure TgxBaseList.Flush;
begin
  if Assigned(Self) then
  begin
    SetCount(0);
  end;
end;

procedure TgxBaseList.Clear;
begin
  if Assigned(Self) then
  begin
    SetCount(0);
    SetCapacity(0);
  end;
end;

procedure TgxBaseList.Delete(Index: Integer);
begin
{$IFOPT R+}
    Assert(Cardinal(index) < Cardinal(FCount));
{$ENDIF}
  Dec(FCount);
  if Index < FCount then
    System.Move(FBaseList[(Index + 1) * FItemSize],
      FBaseList[Index * FItemSize],
      (FCount - Index) * FItemSize);
  Inc(FRevision);
end;

procedure TgxBaseList.DeleteItems(Index: Integer; nbVals: Cardinal);
begin
{$IFOPT R+}
    Assert(Cardinal(index) < Cardinal(FCount));
{$ENDIF}
  if nbVals > 0 then
  begin
    if Index + Integer(nbVals) < FCount then
    begin
      System.Move(FBaseList[(Index + Integer(nbVals)) * FItemSize],
        FBaseList[Index * FItemSize],
        (FCount - Index - Integer(nbVals)) * FItemSize);
    end;
    Dec(FCount, nbVals);
    Inc(FRevision);
  end;
end;

procedure TgxBaseList.Exchange(index1, index2: Integer);
var
  buf: Integer;
  p:   PIntegerArray;
begin
{$IFOPT R+}
    Assert((Cardinal(index1) < Cardinal(FCount)) and (Cardinal(index2) < Cardinal(FCount)));
{$ENDIF}
  if FItemSize = 4 then
  begin
    p := PIntegerArray(FBaseList);
    buf := p^[index1];
    p^[index1] := p^[index2];
    p^[index2] := buf;
  end
  else
  begin
    System.Move(FBaseList[index1 * FItemSize], BufferItem[0], FItemSize);
    System.Move(FBaseList[index2 * FItemSize], FBaseList[index1 * FItemSize], FItemSize);
    System.Move(BufferItem[0], FBaseList[index2 * FItemSize], FItemSize);
  end;
  Inc(FRevision);
end;

procedure TgxBaseList.Move(curIndex, newIndex: Integer);
begin
  if curIndex <> newIndex then
  begin
{$IFOPT R+}
        Assert(Cardinal(newIndex) < Cardinal(Count));
        Assert(Cardinal(curIndex) < Cardinal(Count));
{$ENDIF}
    if FItemSize = 4 then
      PInteger(BufferItem)^ := PInteger(@FBaseList[curIndex * FItemSize])^
    else
      System.Move(FBaseList[curIndex * FItemSize], BufferItem[0], FItemSize);
    if curIndex < newIndex then
    begin
      // curIndex+1 necessarily exists since curIndex<newIndex and newIndex<Count
      System.Move(FBaseList[(curIndex + 1) * FItemSize], FBaseList[curIndex * FItemSize],
        (newIndex - curIndex) * FItemSize);
    end
    else
    begin
      // newIndex+1 necessarily exists since newIndex<curIndex and curIndex<Count
      System.Move(FBaseList[newIndex * FItemSize], FBaseList[(newIndex + 1) * FItemSize],
        (curIndex - newIndex) * FItemSize);
    end;
    if FItemSize = 4 then
      PInteger(@FBaseList[newIndex * FItemSize])^ := PInteger(BufferItem)^
    else
      System.Move(BufferItem[0], FBaseList[newIndex * FItemSize], FItemSize);
    Inc(FRevision);
  end;
end;

procedure TgxBaseList.Reverse;
var
  s, e: Integer;
begin
  s := 0;
  e := Count - 1;
  while s < e do
  begin
    Exchange(s, e);
    Inc(s);
    Dec(e);
  end;
  Inc(FRevision);
end;

// ------------------
// ------------------ TgxBaseVectorList ------------------
// ------------------

procedure TgxBaseVectorList.WriteToFiler(writer: TgxVirtualWriter);
begin
  inherited;
  if Self is TgxTexPointList then
    exit;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing
  end;
end;

procedure TgxBaseVectorList.ReadFromFiler(reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited;
  if Self is TgxTexPointList then
    exit;
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TgxBaseVectorList.GetExtents(out min, max: TAffineVector);
var
  I, K: Integer;
  f:    Single;
  ref:  PFloatArray;
const
  cBigValue: Single   = 1E50;
  cSmallValue: Single = -1E50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for I := 0 to Count - 1 do
  begin
    ref := ItemAddress[I];
    for K := 0 to 2 do
    begin
      f := ref^[K];
      if f < min.V[K] then
        min.V[K] := f;
      if f > max.V[K] then
        max.V[K] := f;
    end;
  end;
end;

function TgxBaseVectorList.Sum: TAffineVector;
var
  I: Integer;
begin
  Result := NullVector;
  for I := 0 to Count - 1 do
    AddVector(Result, PAffineVector(ItemAddress[I])^);
end;

procedure TgxBaseVectorList.Normalize;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    NormalizeVector(PAffineVector(ItemAddress[I])^);
  Inc(FRevision);
end;

function TgxBaseVectorList.MaxSpacing(list2: TgxBaseVectorList): Single;
var
  I: Integer;
  s: Single;
begin
  Assert(list2.Count = Count);
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    s := VectorSpacing(PAffineVector(ItemAddress[I])^,
      PAffineVector(list2.ItemAddress[I])^);
    if s > Result then
      Result := s;
  end;
end;

procedure TgxBaseVectorList.Translate(const delta: TAffineVector);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AddVector(PAffineVector(ItemAddress[I])^, delta);
  Inc(FRevision);
end;

procedure TgxBaseVectorList.Translate(const delta: TgxBaseVectorList);
var
  I: Integer;
begin
  Assert(Count <= delta.Count);
  for I := 0 to Count - 1 do
    AddVector(PAffineVector(ItemAddress[I])^, PAffineVector(delta.ItemAddress[I])^);
  Inc(FRevision);
end;

procedure TgxBaseVectorList.TranslateInv(const delta: TgxBaseVectorList);
var
  I: Integer;
begin
  Assert(Count <= delta.Count);
  for I := 0 to Count - 1 do
    SubtractVector(PAffineVector(ItemAddress[I])^, PAffineVector(delta.ItemAddress[I])^);
  Inc(FRevision);
end;

procedure TgxBaseVectorList.AngleLerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single);
var
  I: Integer;
begin
  Assert(list1.Count = list2.Count);
  if list1 <> list2 then
  begin
    if lerpFactor = 0 then
      Assign(list1)
    else
    if lerpFactor = 1 then
      Assign(list2)
    else
    begin
      Capacity := list1.Count;
      FCount := list1.Count;
      for I := 0 to list1.Count - 1 do
        PAffineVector(ItemAddress[I])^ := VectorAngleLerp(PAffineVector(list1.ItemAddress[I])^,
          PAffineVector(list2.ItemAddress[I])^,
          lerpFactor);
    end;
  end
  else
    Assign(list1);
  Inc(FRevision);
end;

procedure TgxBaseVectorList.AngleCombine(const list1: TgxBaseVectorList; intensity: Single);
var
  I: Integer;
begin
  Assert(list1.Count = Count);
  for I := 0 to Count - 1 do
    PAffineVector(ItemAddress[I])^ := VectorAngleCombine(PAffineVector(ItemAddress[I])^,
      PAffineVector(list1.ItemAddress[I])^,
      intensity);
  Inc(FRevision);
end;

procedure TgxBaseVectorList.Combine(const list2: TgxBaseVectorList; factor: Single);
var
  I: Integer;
begin
  Assert(list2.Count >= Count);
  for I := 0 to Count - 1 do
    CombineVector(PAffineVector(ItemAddress[I])^,
      PAffineVector(list2.ItemAddress[I])^,
      factor);
  Inc(FRevision);
end;

function TgxBaseVectorList.GetItemAddress(Index: Integer): PFloatArray;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := PFloatArray(@FBaseList[Index * FItemSize]);
end;

// ------------------
// ------------------ TgxAffineVectorList ------------------
// ------------------

constructor TgxAffineVectorList.Create;
begin
  FItemSize := SizeOf(TAffineVector);
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

procedure TgxAffineVectorList.Assign(Src: TPersistent);
begin
  if Assigned(Src) then
  begin
    inherited;
    if (Src is TgxAffineVectorList) then
      System.Move(TgxAffineVectorList(Src).FList^, FList^, FCount * SizeOf(TAffineVector));
  end
  else
    Clear;
end;

function TgxAffineVectorList.Add(const item: TAffineVector): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
  Inc(FRevision);
end;

function TgxAffineVectorList.Add(const item: TVector4f): Integer;
begin
  Result := Add(PAffineVector(@item)^);
end;

procedure TgxAffineVectorList.Add(const i1, i2: TAffineVector);
begin
  Inc(FCount, 2);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[FCount - 2] := i1;
  FList^[FCount - 1] := i2;
  Inc(FRevision);
end;

procedure TgxAffineVectorList.Add(const i1, i2, i3: TAffineVector);
begin
  Inc(FCount, 3);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[FCount - 3] := i1;
  FList^[FCount - 2] := i2;
  FList^[FCount - 1] := i3;
  Inc(FRevision);
end;

function TgxAffineVectorList.Add(const item: TVector2f): Integer;
begin
  Result := Add(AffineVectorMake(item.X, item.Y, 0));
end;

function TgxAffineVectorList.Add(const item: TTexPoint): Integer;
begin
  Result := Add(AffineVectorMake(item.S, item.T, 0));
end;

function TgxAffineVectorList.Add(const X, Y: Single): Integer;
var
  v: PAffineVector;
begin
  Result := FCount;
  Inc(FCount);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  v := @List[Result];
  v^.X := X;
  v^.Y := Y;
  v^.Z := 0;
  Inc(FRevision);
end;

function TgxAffineVectorList.Add(const X, Y, Z: Single): Integer;
var
  v: PAffineVector;
begin
  Result := FCount;
  Inc(FCount);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  v := @List[Result];
  v^.X := X;
  v^.Y := Y;
  v^.Z := Z;
  Inc(FRevision);
end;

function TgxAffineVectorList.Add(const X, Y, Z: Integer): Integer;
var
  v: PAffineVector;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  v := @List[Result];
  v^.X := X;
  v^.Y := Y;
  v^.Z := Z;
  Inc(FCount);
  Inc(FRevision);
end;

function TgxAffineVectorList.AddNC(const X, Y, Z: Integer): Integer;
var
  v: PAffineVector;
begin
  Result := FCount;
  v := @List[Result];
  v^.X := X;
  v^.Y := Y;
  v^.Z := Z;
  Inc(FCount);
  Inc(FRevision);
end;

function TgxAffineVectorList.Add(const xy: PIntegerArray; const Z: Integer): Integer;
var
  v: PAffineVector;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  v := @List[Result];
  v^.X := xy^[0];
  v^.Y := xy^[1];
  v^.Z := Z;
  Inc(FCount);
  Inc(FRevision);
end;

function TgxAffineVectorList.AddNC(const xy: PIntegerArray; const Z: Integer): Integer;
var
  v: PAffineVector;
begin
  Result := FCount;
  v := @List[Result];
  v^.X := xy^[0];
  v^.Y := xy^[1];
  v^.Z := Z;
  Inc(FCount);
  Inc(FRevision);
end;

procedure TgxAffineVectorList.Add(const list: TgxAffineVectorList);
begin
  if Assigned(list) and (list.Count > 0) then
  begin
    if Count + list.Count > Capacity then
      Capacity := Count + list.Count;
    System.Move(list.FList[0], FList[Count], list.Count * SizeOf(TAffineVector));
    Inc(FCount, list.Count);
  end;
  Inc(FRevision);
end;

function TgxAffineVectorList.Get(Index: Integer): TAffineVector;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := FList^[Index];
end;

procedure TgxAffineVectorList.Insert(Index: Integer; const Item: TAffineVector);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TAffineVector));
  FList^[Index] := Item;
  Inc(FCount);
  Inc(FRevision);
end;

function TgxAffineVectorList.IndexOf(const item: TAffineVector): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if VectorEquals(item, FList^[I]) then
    begin
      Result := I;
      Break;
    end;
end;

function TgxAffineVectorList.FindOrAdd(const item: TAffineVector): Integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
  begin
    Result := Add(item);
    Inc(FRevision);
  end;
end;

procedure TgxAffineVectorList.Put(Index: Integer; const Item: TAffineVector);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  FList^[Index] := Item;
  Inc(FRevision);
end;

procedure TgxAffineVectorList.SetCapacity(NewCapacity: Integer);
begin
  inherited;
  FList := PAffineVectorArray(FBaseList);
end;

procedure TgxAffineVectorList.Push(const Val: TAffineVector);
begin
  Add(Val);
end;

function TgxAffineVectorList.Pop: TAffineVector;
begin
  if FCount > 0 then
  begin
    Result := Get(FCount - 1);
    Delete(FCount - 1);
    Inc(FRevision);
  end
  else
    Result := NullVector;
end;

procedure TgxAffineVectorList.Translate(const delta: TAffineVector);
begin
  VectorArrayAdd(FList, delta, Count, FList);
  Inc(FRevision);
end;

procedure TgxAffineVectorList.Translate(const delta: TAffineVector; base, nb: Integer);
begin
  VectorArrayAdd(@FList[base], delta, nb, @FList[base]);
  Inc(FRevision);
end;

procedure TgxAffineVectorList.TranslateItem(Index: Integer; const delta: TAffineVector);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  AddVector(FList^[Index], delta);
  Inc(FRevision);
end;

procedure TgxAffineVectorList.TranslateItems(Index: Integer; const delta: TAffineVector; nb: Integer);
begin
  nb := Index + nb;
{$IFOPT R+}
    Assert(Cardinal(index) < Cardinal(FCount));
    if nb > FCount then
        nb := FCount;
{$ENDIF}
  VectorArrayAdd(@FList[Index], delta, nb - Index, @FList[Index]);
  Inc(FRevision);
end;

procedure TgxAffineVectorList.CombineItem(Index: Integer; const vector: TAffineVector; const f: Single);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  CombineVector(FList^[Index], vector, @f);
  Inc(FRevision);
end;

procedure TgxAffineVectorList.TransformAsPoints(const matrix: TMatrix4f);
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FList^[I] := VectorTransform(FList^[I], matrix);
  Inc(FRevision);
end;

procedure TgxAffineVectorList.TransformAsVectors(const matrix: TMatrix4f);
var
  m: TAffineMatrix;
begin
  if FCount > 0 then
  begin
    SetMatrix(m, matrix);
    TransformAsVectors(m);
  end;
end;

procedure TgxAffineVectorList.TransformAsVectors(const matrix: TAffineMatrix);
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FList^[I] := VectorTransform(FList^[I], matrix);
  Inc(FRevision);
end;

procedure TgxAffineVectorList.Normalize;
begin
  NormalizeVectorArray(List, Count);
  Inc(FRevision);
end;

procedure TgxAffineVectorList.Lerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single);
begin
  if (list1 is TgxAffineVectorList) and (list2 is TgxAffineVectorList) then
  begin
    Assert(list1.Count = list2.Count);
    Capacity := list1.Count;
    FCount := list1.Count;
    VectorArrayLerp(TgxAffineVectorList(list1).List, TgxAffineVectorList(list2).List,
      lerpFactor, FCount, List);
    Inc(FRevision);
  end;
end;

procedure TgxAffineVectorList.Scale(factor: Single);
begin
  if (Count > 0) and (factor <> 1) then
  begin
    ScaleFloatArray(@FList[0].X, Count * 3, factor);
    Inc(FRevision);
  end;
end;

procedure TgxAffineVectorList.Scale(const factors: TAffineVector);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ScaleVector(FList^[I], factors);
  Inc(FRevision);
end;

// ------------------
// ------------------ TgxVectorList ------------------
// ------------------

constructor TgxVectorList.Create;
begin
  FItemSize := SizeOf(TVector4f);
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

procedure TgxVectorList.Assign(Src: TPersistent);
begin
  if Assigned(Src) then
  begin
    inherited;
    if (Src is TgxVectorList) then
      System.Move(TgxVectorList(Src).FList^, FList^, FCount * SizeOf(TVector4f));
  end
  else
    Clear;
end;

function TgxVectorList.Add(const item: TVector4f): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

function TgxVectorList.Add(const item: TAffineVector; w: Single): Integer;
begin
  Result := Add(VectorMake(item, w));
end;

function TgxVectorList.Add(const X, Y, Z, w: Single): Integer;
begin
  Result := Add(VectorMake(X, Y, Z, w));
end;

procedure TgxVectorList.Add(const i1, i2, i3: TAffineVector; w: Single);
begin
  Inc(FCount, 3);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  PAffineVector(@FList[FCount - 3])^ := i1;
  FList^[FCount - 3].W := w;
  PAffineVector(@FList[FCount - 2])^ := i2;
  FList^[FCount - 2].W := w;
  PAffineVector(@FList[FCount - 1])^ := i3;
  FList^[FCount - 1].W := w;
end;

function TgxVectorList.AddVector(const item: TAffineVector): Integer;
begin
  Result := Add(VectorMake(item));
end;

function TgxVectorList.AddPoint(const item: TAffineVector): Integer;
begin
  Result := Add(PointMake(item));
end;

function TgxVectorList.AddPoint(const X, Y: Single; const Z: Single = 0): Integer;
begin
  Result := Add(PointMake(X, Y, Z));
end;

function TgxVectorList.Get(Index: Integer): TVector4f;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := FList^[Index];
end;

procedure TgxVectorList.Insert(Index: Integer; const Item: TVector4f);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TVector4f));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TgxVectorList.Put(Index: Integer; const Item: TVector4f);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  FList^[Index] := Item;
end;

procedure TgxVectorList.SetCapacity(NewCapacity: Integer);
begin
  inherited;
  FList := PVectorArray(FBaseList);
end;

procedure TgxVectorList.Push(const Val: TVector4f);
begin
  Add(Val);
end;

function TgxVectorList.Pop: TVector4f;
begin
  if FCount > 0 then
  begin
    Result := Get(FCount - 1);
    Delete(FCount - 1);
  end
  else
    Result := NullHmgVector;
end;

function TgxVectorList.IndexOf(const item: TVector4f): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if VectorEquals(item, FList^[I]) then
    begin
      Result := I;
      Break;
    end;
end;

function TgxVectorList.FindOrAdd(const item: TVector4f): Integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end;

function TgxVectorList.FindOrAddPoint(const item: TAffineVector): Integer;
var
  ptItem: TVector4f;
begin
  MakePoint(ptItem, item);
  Result := IndexOf(ptItem);
  if Result < 0 then
    Result := Add(ptItem);
end;

procedure TgxVectorList.Lerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single);
begin
  if (list1 is TgxVectorList) and (list2 is TgxVectorList) then
  begin
    Assert(list1.Count = list2.Count);
    Capacity := list1.Count;
    FCount := list1.Count;
    VectorArrayLerp(TgxVectorList(list1).List, TgxVectorList(list2).List,
      lerpFactor, FCount, List);
  end;
end;

// ------------------
// ------------------ TgxTexPointList ------------------
// ------------------

constructor TgxTexPointList.Create;
begin
  FItemSize := SizeOf(TTexPoint);
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

procedure TgxTexPointList.Assign(Src: TPersistent);
begin
  if Assigned(Src) then
  begin
    inherited;
    if (Src is TgxTexPointList) then
      System.Move(TgxTexPointList(Src).FList^, FList^, FCount * SizeOf(TTexPoint));
  end
  else
    Clear;
end;

function TgxTexPointList.IndexOf(const item: TTexpoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if TexpointEquals(FList^[I], item) then
    begin
      Result := I;
      Break;
    end;
end;

function TgxTexPointList.FindOrAdd(const item: TTexPoint): Integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end;

function TgxTexPointList.Add(const item: TTexPoint): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

function TgxTexPointList.Add(const item: TVector2f): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := PTexPoint(@Item)^;
  Inc(FCount);
end;

function TgxTexPointList.Add(const texS, Text: Single): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  with FList^[Result] do
  begin
    s := texS;
    t := Text;
  end;
  Inc(FCount);
end;

function TgxTexPointList.Add(const texS, Text: Integer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  with FList^[Result] do
  begin
    s := texS;
    t := Text;
  end;
  Inc(FCount);
end;

function TgxTexPointList.AddNC(const texS, Text: Integer): Integer;
begin
  Result := FCount;
  with FList^[Result] do
  begin
    s := texS;
    t := Text;
  end;
  Inc(FCount);
end;

function TgxTexPointList.Add(const texST: PIntegerArray): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  with FList^[Result] do
  begin
    s := texST^[0];
    t := texST^[1];
  end;
  Inc(FCount);
end;

function TgxTexPointList.AddNC(const texST: PIntegerArray): Integer;
begin
  Result := FCount;
  with FList^[Result] do
  begin
    s := texST^[0];
    t := texST^[1];
  end;
  Inc(FCount);
end;

function TgxTexPointList.Get(Index: Integer): TTexPoint;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := FList^[Index];
end;

procedure TgxTexPointList.Insert(Index: Integer; const Item: TTexPoint);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TTexPoint));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TgxTexPointList.Put(Index: Integer; const Item: TTexPoint);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  FList^[Index] := Item;
end;

procedure TgxTexPointList.SetCapacity(NewCapacity: Integer);
begin
  inherited;
  FList := PTexPointArray(FBaseList);
end;

procedure TgxTexPointList.Push(const Val: TTexPoint);
begin
  Add(Val);
end;

function TgxTexPointList.Pop: TTexPoint;
begin
  if FCount > 0 then
  begin
    Result := Get(FCount - 1);
    Delete(FCount - 1);
  end
  else
    Result := NullTexPoint;
end;

procedure TgxTexPointList.Translate(const delta: TTexPoint);
begin
  TexPointArrayAdd(List, delta, FCount, FList);
end;

procedure TgxTexPointList.ScaleAndTranslate(const scale, delta: TTexPoint);
begin
  TexPointArrayScaleAndAdd(FList, delta, FCount, scale, FList);
end;

procedure TgxTexPointList.ScaleAndTranslate(const scale, delta: TTexPoint; base, nb: Integer);
var
  p: PTexPointArray;
begin
  p := @FList[base];
  TexPointArrayScaleAndAdd(p, delta, nb, scale, p);
end;

procedure TgxTexPointList.Lerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single);
begin
  if (list1 is TgxTexPointList) and (list2 is TgxTexPointList) then
  begin
    Assert(list1.Count = list2.Count);
    Capacity := list1.Count;
    FCount := list1.Count;
    VectorArrayLerp(TgxTexPointList(list1).List, TgxTexPointList(list2).List,
      lerpFactor, FCount, List);
  end;
end;

// ------------------
// ------------------ TgxIntegerList ------------------
// ------------------

constructor TgxIntegerList.Create;
begin
  FItemSize := SizeOf(Integer);
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

procedure TgxIntegerList.Assign(Src: TPersistent);
begin
  if Assigned(Src) then
  begin
    inherited;
    if (Src is TgxIntegerList) then
      System.Move(TgxIntegerList(Src).FList^, FList^, FCount * SizeOf(Integer));
  end
  else
    Clear;
end;

function TgxIntegerList.Add(const item: Integer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

function TgxIntegerList.AddNC(const item: Integer): Integer;
begin
  Result := FCount;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TgxIntegerList.Add(const i1, i2: Integer);
var
  tmpList : PIntegerArray;
begin
  Inc(FCount, 2);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 2];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
end;

procedure TgxIntegerList.Add(const i1, i2, i3: Integer);
var
  tmpList : PIntegerArray;
begin
  Inc(FCount, 3);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 3];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  tmpList^[2] := i3;
end;

procedure TgxIntegerList.Add(const AList: TgxIntegerList);
begin
  if Assigned(AList) and (AList.Count > 0) then
  begin
    if Count + AList.Count > Capacity then
      Capacity := Count + AList.Count;
    System.Move(AList.FList[0], FList[Count], AList.Count * SizeOf(Integer));
    Inc(FCount, AList.Count);
  end;
end;

function TgxIntegerList.Get(Index: Integer): Integer;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := FList^[Index];
end;

procedure TgxIntegerList.Insert(Index: Integer; const Item: Integer);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(Integer));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TgxIntegerList.Remove(const item: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if FList^[I] = item then
    begin
      System.Move(FList[I + 1], FList[I], (FCount - 1 - I) * SizeOf(Integer));
      Dec(FCount);
      Break;
    end;
  end;
end;

procedure TgxIntegerList.Put(Index: Integer; const Item: Integer);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  FList^[Index] := Item;
end;

procedure TgxIntegerList.SetCapacity(NewCapacity: Integer);
begin
  inherited;
  FList := PIntegerArray(FBaseList);
end;

procedure TgxIntegerList.Push(const Val: Integer);
begin
  Add(Val);
end;

function TgxIntegerList.Pop: Integer;
begin
  if FCount > 0 then
  begin
    Result := FList^[FCount - 1];
    Delete(FCount - 1);
  end
  else
    Result := 0;
end;

procedure TgxIntegerList.AddSerie(aBase, aDelta, aCount: Integer);
var
  tmpList : PInteger;
  I:    Integer;
begin
  if aCount <= 0 then
    Exit;
  AdjustCapacityToAtLeast(Count + aCount);
  tmpList := @FList[Count];
  for I := Count to Count + aCount - 1 do
  begin
    tmpList^ := aBase;
    Inc(tmpList);
    aBase := aBase + aDelta;
  end;
  FCount := Count + aCount;
end;

procedure TgxIntegerList.AddIntegers(const First: PInteger; n: Integer);
begin
  if n < 1 then
    Exit;
  AdjustCapacityToAtLeast(Count + n);
  System.Move(First^, FList[FCount], n * SizeOf(Integer));
  FCount := FCount + n;
end;

procedure TgxIntegerList.AddIntegers(const aList: TgxIntegerList);
begin
  if not Assigned(aList) then
    Exit;
  AddIntegers(@aList.List[0], aList.Count);
end;

procedure TgxIntegerList.AddIntegers(const anArray: array of Integer);
var
  n: Integer;
begin
  n := Length(anArray);
  if n > 0 then
    AddIntegers(@anArray[0], n);
end;

function IntegerSearch(item: Integer; list: PIntegerVector; Count: Integer): Integer; register; 
var i : integer;
begin
  result:=-1;
  for i := 0 to Count-1 do begin
    if list^[i]=item then begin
      result:=i;
      break;
    end;
  end;
end;

function TgxIntegerList.IndexOf(item: Integer): Integer; register;
begin
  Result := IntegerSearch(item, FList, FCount);
end;

function TgxIntegerList.MinInteger: Integer;
var
  I: Integer;
  locList: PIntegerVector;
begin
  if FCount > 0 then
  begin
    locList := FList;
    Result := locList^[0];
    for I := 1 to FCount - 1 do
      if locList^[I] < Result then
        Result := locList^[I];
  end
  else
    Result := 0;
end;

function TgxIntegerList.MaxInteger: Integer;
var
  I: Integer;
  locList: PIntegerVector;
begin
  if FCount > 0 then
  begin
    locList := FList;
    Result := locList^[0];
    for I := 1 to FCount - 1 do
      if locList^[I] > Result then
        Result := locList^[I];
  end
  else
    Result := 0;
end;

procedure IntegerQuickSort(sortList: PIntegerArray; left, right: Integer);
var
  I, J: Integer;
  p, t: Integer;
begin
  repeat
    I := left;
    J := right;
    p := sortList^[(left + right) shr 1];
    repeat
      while sortList^[I] < p do
        Inc(I);
      while sortList^[J] > p do
        Dec(J);
      if I <= J then
      begin
        t := sortList^[I];
        sortList^[I] := sortList^[J];
        sortList^[J] := t;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if left < J then
      IntegerQuickSort(sortList, left, J);
    left := I;
  until I >= right;
end;

procedure TgxIntegerList.Sort;
begin
  if (FList <> nil) and (Count > 1) then
    IntegerQuickSort(FList, 0, Count - 1);
end;

procedure TgxIntegerList.SortAndRemoveDuplicates;
var
  I, J, lastVal: Integer;
  localList:     PIntegerArray;
begin
  if (FList <> nil) and (Count > 1) then
  begin
    IntegerQuickSort(FList, 0, Count - 1);
    J := 0;
    localList := FList;
    lastVal := localList^[J];
    for I := 1 to Count - 1 do
    begin
      if localList^[I] <> lastVal then
      begin
        lastVal := localList^[I];
        Inc(J);
        localList^[J] := lastVal;
      end;
    end;
    FCount := J + 1;
  end;
end;

function TgxIntegerList.BinarySearch(const Value: Integer): Integer;
var
  found: Boolean;
begin
  Result := BinarySearch(Value, False, found);
end;

function TgxIntegerList.BinarySearch(const Value: Integer; returnBestFit: Boolean; var found: Boolean): Integer;
var
  Index:   Integer;
  min, max, mid: Integer;
  intList: PIntegerArray;
begin
  // Assume we won't find it
  found := False;
  // If the list is empty, we won't find the sought value!
  if Count = 0 then
  begin
    Result := -1;
    Exit;
  end;

  min := -1; // ONE OFF!
  max := Count; // ONE OFF!

  // We now know that Min and Max AREN'T the values!
  Index := -1;
  intList := List;
  repeat
    // Find the middle of the current scope
    mid := (min + max) shr 1;
    // Reduce the search scope by half
    if intList^[mid] <= Value then
    begin
      // Is this the one?
      if intList^[mid] = Value then
      begin
        Index := mid;
        found := True;
        Break;
      end
      else
        min := mid;
    end
    else
      max := mid;
  until min + 1 = max;

  if returnBestFit then
  begin
    if Index >= 0 then
      Result := Index
    else
      Result := min;
  end
  else
    Result := Index;
end;

function TgxIntegerList.AddSorted(const Value: Integer; const ignoreDuplicates: Boolean = False): Integer;
var
  Index: Integer;
  found: Boolean;
begin
  Index := BinarySearch(Value, True, found);
  if ignoreDuplicates and Found then
    Result := -1
  else
  begin
    Insert(Index + 1, Value);
    Result := Index + 1;
  end;
end;

procedure TgxIntegerList.RemoveSorted(const Value: Integer);
var
  Index: Integer;
begin
  Index := BinarySearch(Value);
  if Index >= 0 then
    Delete(Index);
end;

procedure TgxIntegerList.Offset(delta: Integer);
var
  I: Integer;
  locList: PIntegerArray;
begin
  locList := FList;
  for I := 0 to FCount - 1 do
    locList^[I] := locList^[I] + delta;
end;

procedure TgxIntegerList.Offset(delta: Integer; const base, nb: Integer);
var
  I: Integer;
  locList: PIntegerArray;
begin
  locList := FList;
  for I := base to base + nb - 1 do
    locList^[I] := locList^[I] + delta;
end;

// ------------------
// ------------------ TgxSingleList ------------------
// ------------------

constructor TgxSingleList.Create;
begin
  FItemSize := SizeOf(Single);
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

procedure TgxSingleList.Assign(Src: TPersistent);
begin
  if Assigned(Src) then
  begin
    inherited;
    if (Src is TgxSingleList) then
      System.Move(TgxSingleList(Src).FList^, FList^, FCount * SizeOf(Single));
  end
  else
    Clear;
end;

function TgxSingleList.Add(const item: Single): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TgxSingleList.Add(const i1, i2: Single);
var
  tmpList : PSingleArray;
begin
  Inc(FCount, 2);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 2];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
end;

procedure TgxSingleList.AddSingles(const First: PSingle; n: Integer);
begin
  if n < 1 then
    Exit;
  AdjustCapacityToAtLeast(Count + n);
  System.Move(First^, FList[FCount], n * SizeOf(Single));
  FCount := FCount + n;
end;

procedure TgxSingleList.AddSingles(const anArray: array of Single);
var
  n: Integer;
begin
  n := Length(anArray);
  if n > 0 then
    AddSingles(@anArray[0], n);
end;

function TgxSingleList.Get(Index: Integer): Single;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := FList^[Index];
end;

procedure TgxSingleList.Insert(Index: Integer; const Item: Single);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Single));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TgxSingleList.Put(Index: Integer; const Item: Single);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  FList^[Index] := Item;
end;

procedure TgxSingleList.SetCapacity(NewCapacity: Integer);
begin
  inherited;
  FList := PSingleArrayList(FBaseList);
end;

procedure TgxSingleList.Push(const Val: Single);
begin
  Add(Val);
end;

function TgxSingleList.Pop: Single;
begin
  if FCount > 0 then
  begin
    Result := Get(FCount - 1);
    Delete(FCount - 1);
  end
  else
    Result := 0;
end;

procedure TgxSingleList.AddSerie(aBase, aDelta: Single; aCount: Integer);
var
  tmpList : PSingle;
  I:    Integer;
begin
  if aCount <= 0 then
    Exit;
  AdjustCapacityToAtLeast(Count + aCount);
  tmpList := @FList[Count];
  for I := Count to Count + aCount - 1 do
  begin
    tmpList^ := aBase;
    Inc(tmpList);
    aBase := aBase + aDelta;
  end;
  FCount := Count + aCount;
end;

procedure TgxSingleList.Offset(delta: Single);
begin
  OffsetFloatArray(PFloatVector(FList), FCount, delta);
end;

procedure TgxSingleList.Offset(const delta: TgxSingleList);
begin
  if FCount = delta.FCount then
    OffsetFloatArray(PFloatVector(FList), PFloatVector(delta.FList), FCount)
  else
    raise Exception.Create('SingleList count do not match');
end;

procedure TgxSingleList.Scale(factor: Single);
begin
  ScaleFloatArray(PFloatVector(FList), FCount, factor);
end;

procedure TgxSingleList.Sqr;
var
  I: Integer;
  locList: PSingleArrayList;
begin
  locList := FList;
  for I := 0 to Count - 1 do
    locList^[I] := locList^[I] * locList^[I];
end;

procedure TgxSingleList.Sqrt;
var
  I: Integer;
  locList: PSingleArrayList;
begin
  locList := FList;
  for I := 0 to Count - 1 do
    locList^[I] := System.Sqrt(locList^[I]);
end;

function TgxSingleList.Sum: Single;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FCount-1 do
    Result := Result + FList^[i];
end;

function TgxSingleList.Min: Single;
var
  I: Integer;
  locList: PSingleArrayList;
begin
  if FCount > 0 then
  begin
    locList := FList;
    Result := locList^[0];
    for I := 1 to FCount - 1 do
      if locList^[I] < Result then
        Result := locList^[I];
  end
  else
    Result := 0;
end;

function TgxSingleList.Max: Single;
var
  I: Integer;
  locList: PSingleArrayList;
begin
  if FCount > 0 then
  begin
    locList := FList;
    Result := locList^[0];
    for I := 1 to FCount - 1 do
      if locList^[I] > Result then
        Result := locList^[I];
  end
  else
    Result := 0;
end;

// ------------------
// ------------------ TByteList ------------------
// ------------------

constructor TByteList.Create;
begin
  FItemSize := SizeOf(Byte);
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

procedure TByteList.Assign(Src: TPersistent);
begin
  if Assigned(Src) then
  begin
    inherited;
    if (Src is TByteList) then
      System.Move(TByteList(Src).FList^, FList^, FCount * SizeOf(Byte));
  end
  else
    Clear;
end;

function TByteList.Add(const item: Byte): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

function TByteList.Get(Index: Integer): Byte;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := FList^[Index];
end;

procedure TByteList.Insert(Index: Integer; const Item: Byte);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Byte));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TByteList.Put(Index: Integer; const Item: Byte);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  FList^[Index] := Item;
end;

procedure TByteList.SetCapacity(NewCapacity: Integer);
begin
  inherited;
  FList := PByteArray(FBaseList);
end;

// ------------------
// ------------------ TgxDoubleList ------------------
// ------------------

constructor TgxDoubleList.Create;
begin
  FItemSize := SizeOf(Double);
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

procedure TgxDoubleList.Assign(Src: TPersistent);
begin
  if Assigned(Src) then
  begin
    inherited;
    if (Src is TgxDoubleList) then
      System.Move(TgxDoubleList(Src).FList^, FList^, FCount * SizeOf(Double));
  end
  else
    Clear;
end;

function TgxDoubleList.Add(const item: Double): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

function TgxDoubleList.Get(Index: Integer): Double;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := FList^[Index];
end;

procedure TgxDoubleList.Insert(Index: Integer; const Item: Double);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Double));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TgxDoubleList.Put(Index: Integer; const Item: Double);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  FList^[Index] := Item;
end;

procedure TgxDoubleList.SetCapacity(NewCapacity: Integer);
begin
  inherited;
  FList := PDoubleArrayList(FBaseList);
end;

procedure TgxDoubleList.Push(const Val: Double);
begin
  Add(Val);
end;

function TgxDoubleList.Pop: Double;
begin
  if FCount > 0 then
  begin
    Result := Get(FCount - 1);
    Delete(FCount - 1);
  end
  else
    Result := 0;
end;

procedure TgxDoubleList.AddSerie(aBase, aDelta: Double; aCount: Integer);
var
  tmpList: PDouble;
  I:    Integer;
begin
  if aCount <= 0 then
    Exit;
  AdjustCapacityToAtLeast(Count + aCount);
  tmpList := @FList[Count];
  for I := Count to Count + aCount - 1 do
  begin
    tmpList^ := aBase;
    Inc(tmpList);
    aBase := aBase + aDelta;
  end;
  FCount := Count + aCount;
end;

procedure TgxDoubleList.Offset(delta: Double);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FList^[I] := FList^[I] + delta;
end;

procedure TgxDoubleList.Offset(const delta: TgxDoubleList);
var
  I: Integer;
begin
  if FCount = delta.FCount then
    for I := 0 to Count - 1 do
      FList^[I] := FList^[I] + delta[I]
  else
    raise Exception.Create('DoubleList count do not match');
end;

procedure TgxDoubleList.Scale(factor: Double);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FList^[I] := FList^[I] * factor;
end;

procedure TgxDoubleList.Sqr;
var
  I: Integer;
  locList: PDoubleArrayList;
begin
  locList := FList;
  for I := 0 to Count - 1 do
    locList^[I] := locList^[I] * locList^[I];
end;

procedure TgxDoubleList.Sqrt;
var
  I: Integer;
  locList: PDoubleArrayList;
begin
  locList := FList;
  for I := 0 to Count - 1 do
    locList^[I] := System.Sqrt(locList^[I]);
end;

function TgxDoubleList.Sum: Double;
var
  i: Integer;
begin
    Result := 0;
    for i := 0 to FCount-1 do
    Result := Result + FList^[i];
end;

function TgxDoubleList.Min: Single;
var
  I: Integer;
  locList: PDoubleArrayList;
begin
  if FCount > 0 then
  begin
    locList := FList;
    Result := locList^[0];
    for I := 1 to FCount - 1 do
      if locList^[I] < Result then
        Result := locList^[I];
  end
  else
    Result := 0;
end;

function TgxDoubleList.Max: Single;
var
  I: Integer;
  locList: PDoubleArrayList;
begin
  if FCount > 0 then
  begin
    locList := FList;
    Result := locList^[0];
    for I := 1 to FCount - 1 do
      if locList^[I] > Result then
        Result := locList^[I];
  end
  else
    Result := 0;
end;

// ------------------
// ------------------ TQuaternionList ------------------
// ------------------

constructor TQuaternionList.Create;
begin
  FItemSize := SizeOf(TQuaternion);
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

procedure TQuaternionList.Assign(Src: TPersistent);
begin
  if Assigned(Src) then
  begin
    inherited;
    if (Src is TQuaternionList) then
      System.Move(TQuaternionList(Src).FList^, FList^, FCount * SizeOf(TQuaternion));
  end
  else
    Clear;
end;

function TQuaternionList.Add(const item: TQuaternion): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

function TQuaternionList.Add(const item: TAffineVector; w: Single): Integer;
begin
  Result := Add(QuaternionMake(item.V, w));
end;

function TQuaternionList.Add(const X, Y, Z, w: Single): Integer;
begin
  Result := Add(QuaternionMake([X, Y, Z], w));
end;

function TQuaternionList.Get(Index: Integer): TQuaternion;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := FList^[Index];
end;

procedure TQuaternionList.Insert(Index: Integer; const Item: TQuaternion);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TQuaternion));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TQuaternionList.Put(Index: Integer; const Item: TQuaternion);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  FList^[Index] := Item;
end;

procedure TQuaternionList.SetCapacity(NewCapacity: Integer);
begin
  inherited;
  FList := PQuaternionArray(FBaseList);
end;

procedure TQuaternionList.Push(const Val: TQuaternion);
begin
  Add(Val);
end;

function TQuaternionList.Pop: TQuaternion;
begin
  if FCount > 0 then
  begin
    Result := Get(FCount - 1);
    Delete(FCount - 1);
  end
  else
    Result := IdentityQuaternion;
end;

function TQuaternionList.IndexOf(const item: TQuaternion): Integer;
var
  I: Integer;
  curItem: PQuaternion;
begin
  for I := 0 to Count - 1 do
  begin
    curItem := @FList[I];
    if (item.RealPart = curItem^.RealPart) and VectorEquals(item.ImagPart, curItem^.ImagPart) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TQuaternionList.FindOrAdd(const item: TQuaternion): Integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end;

procedure TQuaternionList.Lerp(const list1, list2: TgxBaseVectorList; lerpFactor: Single);
var
  I: Integer;
begin
  if (list1 is TQuaternionList) and (list2 is TQuaternionList) then
  begin
    Assert(list1.Count = list2.Count);
    Capacity := list1.Count;
    FCount := list1.Count;
    for I := 0 to FCount - 1 do
      Put(I, QuaternionSlerp(TQuaternionList(list1)[I], TQuaternionList(list2)[I], lerpFactor));
  end;
end;

procedure TQuaternionList.Combine(const list2: TgxBaseVectorList; factor: Single);

  procedure CombineQuaternion(var q1: TQuaternion; const q2: TQuaternion; factor: Single);
  begin
    q1 := QuaternionMultiply(q1, QuaternionSlerp(IdentityQuaternion, q2, factor));
  end;

var
  I: Integer;
begin
  Assert(list2.Count >= Count);
  if list2 is TQuaternionList then
  begin
    for I := 0 to Count - 1 do
    begin
      CombineQuaternion(PQuaternion(ItemAddress[I])^,
        PQuaternion(list2.ItemAddress[I])^,
        factor);
    end;
  end
  else
    inherited;
end;

// ------------------
// ------------------ Tgx4ByteList ------------------
// ------------------

constructor Tgx4ByteList.Create;
begin
  FItemSize := SizeOf(Tgx4ByteList);
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

procedure Tgx4ByteList.Assign(Src: TPersistent);
begin
  if Assigned(Src) then
  begin
    inherited;
    if (Src is Tgx4ByteList) then
      System.Move(Tgx4ByteList(Src).FList^, FList^, FCount * SizeOf(T4ByteData));
  end
  else
    Clear;
end;

function Tgx4ByteList.Add(const item: T4ByteData): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
  Inc(FRevision);
end;

procedure Tgx4ByteList.Add(const i1: Single);
var
  tmpList: PSingle;
begin
  Inc(FCount);
  if FCount >= FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 1];
  tmpList^ := i1;
  Inc(FRevision);
end;

procedure Tgx4ByteList.Add(const i1, i2: Single);
var
  tmpList: PSingleArray;
begin
  Inc(FCount, 2);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 2];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  Inc(FRevision);
end;


procedure Tgx4ByteList.Add(const i1, i2, i3: Single);
var
  tmpList: PSingleArray;
begin
  Inc(FCount, 3);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 3];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  tmpList^[2] := i3;
  Inc(FRevision);
end;


procedure Tgx4ByteList.Add(const i1, i2, i3, i4: Single);
var
  tmpList: PSingleArray;
begin
  Inc(FCount, 4);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 4];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  tmpList^[2] := i3;
  tmpList^[3] := i4;
  Inc(FRevision);
end;

procedure Tgx4ByteList.Add(const i1: Integer);
var
  tmpList: PInteger;
begin
  Inc(FCount);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 1];
  tmpList^ := i1;
  Inc(FRevision);
end;

procedure Tgx4ByteList.Add(const i1, i2: Integer);
var
  tmpList: PIntegerArray;
begin
  Inc(FCount, 2);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 2];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  Inc(FRevision);
end;


procedure Tgx4ByteList.Add(const i1, i2, i3: Integer);
var
  tmpList: PIntegerArray;
begin
  Inc(FCount, 3);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 3];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  tmpList^[2] := i3;
  Inc(FRevision);
end;


procedure Tgx4ByteList.Add(const i1, i2, i3, i4: Integer);
var
  tmpList: PIntegerArray;
begin
  Inc(FCount, 4);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 4];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  tmpList^[2] := i3;
  tmpList^[3] := i4;
  Inc(FRevision);
end;

procedure Tgx4ByteList.Add(const i1: Cardinal);
var
  tmpList: PLongWord;
begin
  Inc(FCount);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 1];
  tmpList^ := i1;
  Inc(FRevision);
end;

procedure Tgx4ByteList.Add(const i1, i2: Cardinal);
var
  tmpList: PLongWordArray;
begin
  Inc(FCount, 2);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 2];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  Inc(FRevision);
end;


procedure Tgx4ByteList.Add(const i1, i2, i3: Cardinal);
var
  tmpList: PLongWordArray;
begin
  Inc(FCount, 3);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 3];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  tmpList^[2] := i3;
  Inc(FRevision);
end;


procedure Tgx4ByteList.Add(const i1, i2, i3, i4: Cardinal);
var
  tmpList: PLongWordArray;
begin
  Inc(FCount, 4);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 4];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  tmpList^[2] := i3;
  tmpList^[3] := i4;
  Inc(FRevision);
end;

procedure Tgx4ByteList.Add(const AList: Tgx4ByteList);
begin
  if Assigned(AList) and (AList.Count > 0) then
  begin
    if Count + AList.Count > Capacity then
      Capacity := Count + AList.Count;
    System.Move(AList.FList[0], FList[Count], AList.Count * SizeOf(T4ByteData));
    Inc(FCount, AList.Count);
    Inc(FRevision);
  end;
end;

function Tgx4ByteList.Get(Index: Integer): T4ByteData;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := FList^[Index];
end;

procedure Tgx4ByteList.Insert(Index: Integer; const Item: T4ByteData);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(T4ByteData));
  FList^[Index] := Item;
  Inc(FCount);
  Inc(FRevision);
end;

procedure Tgx4ByteList.Put(Index: Integer; const Item: T4ByteData);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  FList^[Index] := Item;
  INc(FRevision);
end;

procedure Tgx4ByteList.SetCapacity(NewCapacity: Integer);
begin
  inherited;
  FList := P4ByteArrayList(FBaseList);
end;

procedure Tgx4ByteList.Push(const Val: T4ByteData);
begin
  Add(Val);
end;

function Tgx4ByteList.Pop: T4ByteData;
const
  Zero : T4ByteData = ( Int: (Value:0) );
begin
  if FCount > 0 then
  begin
    Result := Get(FCount - 1);
    Delete(FCount - 1);
  end
  else
    Result := Zero;
end;

// ------------------
// ------------------ TgxLongWordList ------------------
// ------------------

constructor TgxLongWordList.Create;
begin
  FItemSize := SizeOf(LongWord);
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

procedure TgxLongWordList.Assign(Src: TPersistent);
begin
  if Assigned(Src) then
  begin
    inherited;
    if (Src is TgxLongWordList) then
      System.Move(TgxLongWordList(Src).FList^, FList^, FCount * SizeOf(LongWord));
  end
  else
    Clear;
end;

function TgxLongWordList.Add(const item: LongWord): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

function TgxLongWordList.AddNC(const item: LongWord): Integer;
begin
  Result := FCount;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TgxLongWordList.Add(const i1, i2: LongWord);
var
  tmpList : PLongWordArray;
begin
  Inc(FCount, 2);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 2];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
end;

procedure TgxLongWordList.Add(const i1, i2, i3: LongWord);
var
  tmpList : PLongWordArray;
begin
  Inc(FCount, 3);
  while FCount > FCapacity do
    SetCapacity(FCapacity + FGrowthDelta);
  tmpList := @FList[FCount - 3];
  tmpList^[0] := i1;
  tmpList^[1] := i2;
  tmpList^[2] := i3;
end;

procedure TgxLongWordList.Add(const AList: TgxLongWordList);
begin
  if Assigned(AList) and (AList.Count > 0) then
  begin
    if Count + AList.Count > Capacity then
      Capacity := Count + AList.Count;
    System.Move(AList.FList[0], FList[Count], AList.Count * SizeOf(LongWord));
    Inc(FCount, AList.Count);
  end;
end;

function TgxLongWordList.Get(Index: Integer): LongWord;
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  Result := FList^[Index];
end;

procedure TgxLongWordList.Insert(Index: Integer; const Item: LongWord);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(LongWord));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TgxLongWordList.Remove(const item: LongWord);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if FList^[I] = item then
    begin
      System.Move(FList[I + 1], FList[I], (FCount - 1 - I) * SizeOf(LongWord));
      Dec(FCount);
      Break;
    end;
  end;
end;

procedure TgxLongWordList.Put(Index: Integer; const Item: LongWord);
begin
{$IFOPT R+}
    Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
  FList^[Index] := Item;
end;

procedure TgxLongWordList.SetCapacity(NewCapacity: Integer);
begin
  inherited;
  FList := PLongWordArray(FBaseList);
end;

procedure TgxLongWordList.Push(const Val: LongWord);
begin
  Add(Val);
end;

function TgxLongWordList.Pop: LongWord;
begin
  if FCount > 0 then
  begin
    Result := FList^[FCount - 1];
    Delete(FCount - 1);
  end
  else
    Result := 0;
end;

procedure TgxLongWordList.AddLongWords(const First: PLongWord; n: Integer);
begin
  if n < 1 then
    Exit;
  AdjustCapacityToAtLeast(Count + n);
  System.Move(First^, FList[FCount], n * SizeOf(LongWord));
  FCount := FCount + n;
end;

procedure TgxLongWordList.AddLongWords(const aList: TgxLongWordList);
begin
  if not Assigned(aList) then
    Exit;
  AddLongWords(@aList.List[0], aList.Count);
end;

procedure TgxLongWordList.AddLongWords(const anArray: array of LongWord);
var
  n: Integer;
begin
  n := Length(anArray);
  if n > 0 then
    AddLongWords(@anArray[0], n);
end;

function LongWordSearch(item: LongWord; list: PLongWordVector; Count: Integer): Integer; register;
var i : integer;
begin
  result:=-1;
  for i := 0 to Count-1 do begin
    if list^[i]=item then begin
      result:=i;
      break;
    end;
  end;
end;

function TgxLongWordList.IndexOf(item: Integer): LongWord; register;
begin
  Result := LongWordSearch(item, FList, FCount);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TgxAffineVectorList, TgxVectorList, TgxTexPointList, TgxSingleList,
                   TgxDoubleList, Tgx4ByteList, TgxLongWordList]);

end.
