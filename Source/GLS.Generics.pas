//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.Generics;

(* Cross IDE generic classes collection. *)

interface

{$I GLScene.inc}

uses
  System.SysUtils,
  System.Classes,
  SyncObjs;

const
  MaxListSize = Maxint div 16;

type

{$IFDEF USE_GENERIC_PREFIX}
  generic
{$ENDIF}
  GList<T> = class(TObject)
  public
    type
    TListChangeEvent = procedure(Sender: TObject; const Item: T;
      Action: TListNotification) of object;
//    var
  private
    FItems: array of T;
    FCount: Integer;
    FCapacity: Integer;
//    FOnChange: TListChangeEvent;
    FOnChange: TNotifyEvent;
  protected
    procedure SetCapacity(Value: Integer);
    procedure SetCount(Value: Integer);
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
    function GetItemAddress(Index: Integer): Pointer;
    procedure Grow;
  protected
    procedure Notify(const Item: T; Action: TListNotification); virtual;
  public
    destructor Destroy; override;
    procedure Clear;
    function Add(AItem: T): Integer;
    procedure Delete(Index: Integer);
    procedure Extract(AItem: T);
    function Remove(AItem: T): Integer;
    function IndexOf(AItem: T): Integer;
    procedure Insert(Index: Integer; AItem: T);
    procedure Exchange(Index1, Index2: Integer);
    function First: T;
    function Last: T;
    property Items[Index: Integer]: T read GetItem write SetItem; default;
    property ItemAddress[Index: Integer]: Pointer read GetItemAddress;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
//    property OnChange: TListChangeEvent read FOnChange write FOnChange;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{$IFDEF USE_GENERIC_PREFIX}
  generic
{$ENDIF}
  GThreadList<T> = class
  public
    type
      TLockableList = {$IFDEF USE_GENERIC_PREFIX} specialize {$ENDIF} GList<T>;
    var
  private
    FList: TLockableList;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AItem: T);
    procedure Clear;
    function  LockList: TLockableList;
    procedure Remove(AItem: T);
    procedure UnlockList;
  end;

{$IFDEF USE_GENERIC_PREFIX}
  generic
{$ENDIF}
  GOrderedList<T> = class(TObject)
  private
    type
      TOrderedList = {$IFDEF USE_GENERIC_PREFIX} specialize {$ENDIF} GList<T>;
    var
      FList: TOrderedList;
  protected
    procedure PushItem(AItem: T); virtual; abstract;
    function PopItem: T; virtual;
    function PeekItem: T; virtual;
    property List: TOrderedList read FList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Count: Integer;
    function AtLeast(ACount: Integer): Boolean;
    function Push(const AItem: T): T;
    function Pop: T;
    function Peek: T;
  end;

  {$IFDEF USE_GENERIC_PREFIX}
    generic
  {$ENDIF}
  GStack<T> = class({$IFDEF USE_GENERIC_PREFIX} specialize {$ENDIF} GOrderedList<T>)
  protected
    procedure PushItem(AItem: T); override;
  end;

  {$IFDEF USE_GENERIC_PREFIX}
  generic
  {$ENDIF}
  GQueue<T> = class(GOrderedList<T>)
  protected
    procedure PushItem(AItem: T); override;
  end;

implementation


destructor GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Destroy;
begin
  Clear;
end;

procedure GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.SetCapacity(Value: Integer);
begin
{$IFOPT R+}
  Assert(not (Value < FCount) or (Value > MaxListSize));
{$ENDIF}
  if Value <> FCapacity then
  begin
    SetLength(FItems, Value);
    FCapacity := Value;
  end;
end;

procedure GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.SetCount(Value: Integer);
var
  I: Integer;
begin
{$IFOPT R+}
  Assert(not (Value < 0) or (Value > MaxListSize));
{$ENDIF}
  if Value > FCapacity then
    SetCapacity(Value);
  if Value <= FCount then
    for I := FCount - 1 downto Value do
      Delete(I);
  FCount := Value;
end;

function GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Add(AItem: T): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FItems[Result] := AItem;
  Inc(FCount);
  Notify(AItem, lnAdded);
end;

procedure GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Delete(Index: Integer);
var
  Temp: T;
begin
{$IFOPT R+}
  Assert(Index < FCount);
{$ENDIF}
  Temp := FItems[Index];
  Dec(FCount);
  if Index < FCount then
    Move(FItems[Index + 1], FItems[Index],
      (FCount - Index) * SizeOf(T));
  Notify(Temp, lnDeleted);
end;


procedure GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Extract(AItem: T);
var
  I: Integer;
begin
  I := IndexOf(AItem);
  if I >= 0 then
  begin
    Delete(I);
    Notify(AItem, lnExtracted);
  end;
end;

function GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.First: T;
begin
  Result := GetItem(0);
end;

function GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.GetItem(Index: Integer): T;
begin
{$IFOPT R+}
  Assert(Index < FCount);
{$ENDIF}
  Result := FItems[Index];
end;

function GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.GetItemAddress(Index: Integer): Pointer;
begin
{$IFOPT R+}
  Assert(Index < FCount);
{$ENDIF}
  Result := @FItems[Index];
end;

function GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.IndexOf(AItem: T): Integer;
begin
  for Result := 0 to FCount - 1 do
    if CompareMem(@FItems[Result], @AItem, SizeOf(T)) then
      exit;
  Result := -1;
end;

procedure GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Insert(Index: Integer; AItem: T);
begin
{$IFOPT R+}
  Assert(Index < FCount);
{$ENDIF}
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    Move(FItems[Index], FItems[Index + 1],
      (FCount - Index) * SizeOf(T));
  FItems[Index] := AItem;
  Inc(FCount);
  Notify(AItem, lnAdded);
end;

procedure GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Exchange(Index1, Index2: Integer);
var
  Item: T;
begin
{$IFOPT R+}
  Assert(Index1 < FCount);
  Assert(Index2 < FCount);
{$ENDIF}
  Item := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := Item;
end;

function GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Last: T;
begin
  if FCount > 0 then
    Result := FItems[FCount-1];
end;

procedure GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Notify(const Item: T; Action: TListNotification);
begin
//  if Assigned(FOnChange) then FOnChange(Self, Item, Action);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Remove(AItem: T): Integer;
begin
  Result := IndexOf(AItem);
  if Result >= 0 then
    Delete(Result);
end;

procedure GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.SetItem(Index: Integer; const Value: T);
begin
  FItems[Index] := Value;
end;

procedure GList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

constructor GThreadList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FList := TLockableList.Create;
end;

destructor GThreadList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Destroy;
begin
  LockList;
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
end;

procedure GThreadList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Add(AItem: T);
begin
  LockList;
  try
    FList.Add(AItem)
  finally
    UnlockList;
  end;
end;

procedure GThreadList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function GThreadList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.LockList: TLockableList;
begin
  FLock.Enter;
  Result := FList;
end;

procedure GThreadList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Remove(AItem: T);
begin
  LockList;
  try
    FList.Remove(AItem);
  finally
    UnlockList;
  end;
end;

procedure GThreadList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.UnlockList;
begin
  FLock.Leave;
end;

constructor GOrderedList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Create;
begin
  FList := TOrderedList.Create;
end;

destructor GOrderedList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Destroy;
begin
  FList.Free;
end;

function GOrderedList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.AtLeast(ACount: Integer): Boolean;
begin
  Result := List.Count >= ACount;
end;

function GOrderedList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.PeekItem: T;
begin
  Result := List[List.Count-1];
end;

function GOrderedList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.PopItem: T;
begin
  Result := PeekItem;
  List.Delete(List.Count-1);
end;

function GOrderedList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Peek: T;
begin
  Result := PeekItem;
end;

function GOrderedList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Pop: T;
begin
  Result := PopItem;
end;

function GOrderedList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Push(const AItem: T): T;
begin
  PushItem(AItem);
  Result := AItem;
end;

function GOrderedList{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.Count: Integer;
begin
  Result := List.Count;
end;

procedure GStack{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.PushItem(AItem: T);
begin
  List.Add(AItem);
end;



procedure GQueue{$IFNDEF USE_GENERIC_PREFIX}<T>{$ENDIF}.PushItem(AItem: T);
begin
  List.Insert(0, AItem);
end;

end.
