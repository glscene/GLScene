//
// The graphics engine GXScene https://github.com/glscene
//
unit Stage.RedBlackTree;

(*
  USAGE
  The TRedBlackTree generic class behaves somewhat like a TList:
  it stores _Value_ by _Key_
  and uses the same comparison function as TList.Sort (TListSortCompare).
  Functions Clear, Add, Delete, First and Last are equivalent,
  except that First and Last return a _Key_ so they
  can be used for comparisons in loops.

  All _Key_ occur only once in the tree if DuplicateKeys is False:
  when the same value is added twice, the second one is not stored.

  When DuplicateKeys is enabled the second comparison function is used
  for sort _Value_ and it duplicates not allowed.

  To be able to manage the tree, the Create constructor has an argument
  specifying the comparison function that should be used.

  The function Find can be used to find a _Value_ that was put in the tree,
  it searches for the given _Key_ using the comparison function given
  at time of object creation.

  The functions NextKey and PrevKey can be used to "walk" through the tree:
  given a _Key_, NextKey replace it with the smallest key that
  is larger than _Key_, PrevKey returns the largest key that is
  smaller than _Key_. For Last and First key result not returned.
*)

interface

{$I Stage.Defines.inc}

uses
  System.Classes;

type

  TRBColor = (clRed, clBlack);

{$IFDEF GENERIC_PREFIX}
generic
{$ENDIF}

GRedBlackTree<TKey, TValue> = class
  public 
  type 
  TKeyCompareFunc = function(const Item1, Item2: TKey): Integer;
  TValueCompareFunc = function(const Item1, Item2: TValue): Boolean;
  TForEachProc = procedure(AKey: TKey; AValue: TValue; out AContinue: Boolean);
  TRBNode = class Key: TKey;
    Left, Right, Parent, Twin: TRBNode;
    Color:
    TRBColor;
    Value:
    TValue;
  end;

  var
    FRoot: TRBNode;
    FLeftmost: TRBNode;
    FRightmost: TRBNode;
    FLastFound: TRBNode;
    FLastNode: TRBNode;
    FCount: Integer;
    FKeyCompareFunc: TKeyCompareFunc;
    FDuplicateKeys: Boolean;
    FValueCompareFunc: TValueCompareFunc;
    FOnChange: TNotifyEvent;

  function FindNode(const Key: TKey): TRBNode;
  procedure RotateLeft(var x: TRBNode);
  procedure RotateRight(var x: TRBNode);
  function Minimum(var x: TRBNode): TRBNode;
  function Maximum(var x: TRBNode): TRBNode;
  function GetFirst: TKey;
  function GetLast: TKey;
  procedure SetDuplicateKeys(Value: Boolean);
  class procedure FastErase(x: TRBNode);
  public
    constructor Create(KeyCompare: TKeyCompareFunc;
      ValueCompare: TValueCompareFunc);
    destructor Destroy; override;
    procedure Clear;
    // Find value by key.
    function Find(const Key: TKey; out Value: TValue): Boolean;
    function NextKey(var Key: TKey; out Value: TValue): Boolean;
    function PrevKey(var Key: TKey; out Value: TValue): Boolean;
    function NextDublicate(out Value: TValue): Boolean;
    procedure Add(const Key: TKey; const Value: TValue);
    procedure Delete(const Key: TKey);
    procedure ForEach(AProc: TForEachProc);
    property Count: Integer read FCount;
    property First: TKey read GetFirst;
    property Last: TKey read GetLast;
    property DuplicateKeys: Boolean read FDuplicateKeys
      write SetDuplicateKeys;
    property OnChange: TNotifyEvent read FOnChange
      write FOnChange;
end;

function CompareInteger(const Item1, Item2: Integer): Integer;

implementation // -------------------------------------------------------------

function CompareInteger(const Item1, Item2: Integer): Integer;
begin
  if Item1 < Item2 then
  begin
    Result := -1;
  end
  else if (Item1 = Item2) then
  begin
    Result := 0;
  end
  else
  begin
    Result := 1;
  end
end;

constructor GRedBlackTree<TKey, TValue>.Create(KeyCompare: TKeyCompareFunc;
  ValueCompare: TValueCompareFunc);
begin
  inherited Create;
  Assert(Assigned(KeyCompare));
  FKeyCompareFunc := KeyCompare;
  FValueCompareFunc := ValueCompare;
  FRoot := nil;
  FLeftmost := nil;
  FRightmost := nil;
  FDuplicateKeys := Assigned(ValueCompare);
end;

destructor GRedBlackTree<TKey, TValue>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

class procedure GRedBlackTree<TKey, TValue>.FastErase(x: TRBNode);
var
  y: TRBNode;
begin
  if (x.Left <> nil) then
    FastErase(x.Left);
  if (x.Right <> nil) then
    FastErase(x.Right);
  repeat
    y := x;
    x := x.Twin;
    y.Destroy;
  until x = nil;
end;

procedure GRedBlackTree<TKey, TValue>.Clear;
begin
  if (FRoot <> nil) then
    FastErase(FRoot);
  FRoot := nil;
  FLeftmost := nil;
  FRightmost := nil;
  FCount := 0;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function GRedBlackTree<TKey, TValue>.Find(const Key: TKey;
  out Value: TValue): Boolean;
begin
  FLastFound := FindNode(Key);
  Result := Assigned(FLastFound);
  if Result then
    Value := FLastFound.Value;
end;

function GRedBlackTree<TKey, TValue>.FindNode(const Key: TKey): TRBNode;
var
  cmp: Integer;
begin
  Result := FRoot;
  while (Result <> nil) do
  begin
    cmp := FKeyCompareFunc(Result.Key, Key);
    if cmp < 0 then
    begin
      Result := Result.Right;
    end
    else if cmp > 0 then
    begin
      Result := Result.Left;
    end
    else
    begin
      break;
    end;
  end;
end;

function GRedBlackTree<TKey, TValue>.NextDublicate(out Value: TValue): Boolean;
begin
  if Assigned(FLastFound) then
  begin
    if Assigned(FLastFound.Twin) then
    begin
      FLastFound := FLastFound.Twin;
      Value := FLastFound.Value;
      exit(True);
    end;
  end;
  Result := False;
end;

procedure GRedBlackTree<TKey, TValue>.RotateLeft(var x: TRBNode);
var
  y: TRBNode;
begin
  y := x.Right;
  x.Right := y.Left;
  if (y.Left <> nil) then
  begin
    y.Left.Parent := x;
  end;
  y.Parent := x.Parent;
  if (x = FRoot) then
  begin
    FRoot := y;
  end
  else if (x = x.Parent.Left) then
  begin
    x.Parent.Left := y;
  end
  else
  begin
    x.Parent.Right := y;
  end;
  y.Left := x;
  x.Parent := y;
end;

procedure GRedBlackTree<TKey, TValue>.RotateRight(var x: TRBNode);
var
  y: TRBNode;
begin
  y := x.Left;
  x.Left := y.Right;
  if (y.Right <> nil) then
  begin
    y.Right.Parent := x;
  end;
  y.Parent := x.Parent;
  if (x = FRoot) then
  begin
    FRoot := y;
  end
  else if (x = x.Parent.Right) then
  begin
    x.Parent.Right := y;
  end
  else
  begin
    x.Parent.Left := y;
  end;
  y.Right := x;
  x.Parent := y;
end;

function GRedBlackTree<TKey, TValue>.Minimum(var x: TRBNode): TRBNode;
begin
  Result := x;
  while (Result.Left <> nil) do
    Result := Result.Left;
end;

function GRedBlackTree<TKey, TValue>.Maximum(var x: TRBNode): TRBNode;
begin
  Result := x;
  while (Result.Right <> nil) do
    Result := Result.Right;
end;

procedure GRedBlackTree<TKey, TValue>.Add(const Key: TKey; const Value: TValue);
var
  x, y, z, zpp: TRBNode;
  cmp: Integer;
begin
  z := TRBNode.Create;

  { Initialize fields in new node z }
  z.Key := Key;
  z.Left := nil;
  z.Right := nil;
  z.Color := clRed;
  z.Value := Value;
  z.Twin := nil;

  { Maintain FLeftmost and FRightmost nodes }
  if ((FLeftmost = nil) or (FKeyCompareFunc(Key, FLeftmost.Key) < 0)) then
  begin
    FLeftmost := z;
  end;
  if ((FRightmost = nil) or (FKeyCompareFunc(FRightmost.Key, Key) < 0)) then
  begin
    FRightmost := z;
  end;

  { Insert node z }
  y := nil;
  x := FRoot;
  while (x <> nil) do
  begin
    y := x;
    cmp := FKeyCompareFunc(Key, x.Key);
    if cmp < 0 then
      x := x.Left
    else if cmp > 0 then
      x := x.Right
    else
    begin
      { Key already exists in tree. }
      if FDuplicateKeys then
      begin
        { Check twins chain for value dublicate. }
        repeat
          if FValueCompareFunc(Value, x.Value) then
          begin
            y := nil;
            break;
          end;
          y := x;
          x := x.Twin;
        until x = nil;
        if Assigned(y) then
        begin
          { Add dublicate key to end of twins chain. }
          y.Twin := z;
          Inc(FCount);
          if Assigned(FOnChange) then
            FOnChange(Self);
          exit;
        end;
        // Value already exists in tree.
      end;
      z.Destroy;
      // a jzombi: memory leak: if we don't put it in the tree, we shouldn't hold it in the memory
      exit;
    end;
  end;
  z.Parent := y;
  if (y = nil) then
  begin
    FRoot := z;
  end
  else if (FKeyCompareFunc(Key, y.Key) < 0) then
  begin
    y.Left := z;
  end
  else
  begin
    y.Right := z;
  end;

  { Rebalance tree }
  while ((z <> FRoot) and (z.Parent.Color = clRed)) do
  begin
    zpp := z.Parent.Parent;
    if (z.Parent = zpp.Left) then
    begin
      y := zpp.Right;
      if ((y <> nil) and (y.Color = clRed)) then
      begin
        z.Parent.Color := clBlack;
        y.Color := clBlack;
        zpp.Color := clRed;
        z := zpp;
      end
      else
      begin
        if (z = z.Parent.Right) then
        begin
          z := z.Parent;
          RotateLeft(z);
        end;
        z.Parent.Color := clBlack;
        zpp.Color := clRed;
        RotateRight(zpp);
      end;
    end
    else
    begin
      y := zpp.Left;
      if ((y <> nil) and (y.Color = clRed)) then
      begin
        z.Parent.Color := clBlack;
        y.Color := clBlack;
        zpp.Color := clRed; // c jzombi: zpp.color := clRed;
        z := zpp;
      end
      else
      begin
        if (z = z.Parent.Left) then
        begin
          z := z.Parent;
          RotateRight(z);
        end;
        z.Parent.Color := clBlack;
        zpp.Color := clRed; // c jzombi: zpp.color := clRed;
        RotateLeft(zpp);
      end;
    end;
  end;
  FRoot.Color := clBlack;
  Inc(FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure GRedBlackTree<TKey, TValue>.Delete(const Key: TKey);
var
  w, x, y, z, x_parent: TRBNode;
  tmpcol: TRBColor;
begin
  z := FindNode(Key);
  if z = nil then
    exit;

  y := z;
  x := nil;
  x_parent := nil;

  if (y.Left = nil) then
  begin // z has at most one non-null child. y = z.
    x := y.Right; // x might be null.
  end
  else
  begin
    if (y.Right = nil) then
    begin // z has exactly one non-null child. y = z.
      x := y.Left; // x is not null.
    end
    else
    begin
      // z has two non-null children.  Set y to
      y := y.Right; // z's successor.  x might be null.
      while (y.Left <> nil) do
      begin
        y := y.Left;
      end;
      x := y.Right;
    end;
  end;

  if (y <> z) then
  begin
    (* "copy y's sattelite data into z"
      relink y in place of z.  y is z's successor *)
    z.Left.Parent := y;
    y.Left := z.Left;
    if (y <> z.Right) then
    begin
      x_parent := y.Parent;
      if (x <> nil) then
      begin
        x.Parent := y.Parent;
      end;
      y.Parent.Left := x; // y must be a child of left
      y.Right := z.Right;
      z.Right.Parent := y;
    end
    else
    begin
      x_parent := y;
    end;
    if (FRoot = z) then
    begin
      FRoot := y;
    end
    else if (z.Parent.Left = z) then
    begin
      z.Parent.Left := y;
    end
    else
    begin
      z.Parent.Right := y;
    end;
    y.Parent := z.Parent;
    tmpcol := y.Color;
    y.Color := z.Color;
    z.Color := tmpcol;
    y := z;
    // y now points to node to be actually deleted
  end
  else
  begin // y = z
    x_parent := y.Parent;
    if (x <> nil) then
    begin
      x.Parent := y.Parent;
    end;
    if (FRoot = z) then
    begin
      FRoot := x;
    end
    else
    begin
      if (z.Parent.Left = z) then
      begin
        z.Parent.Left := x;
      end
      else
      begin
        z.Parent.Right := x;
      end;
    end;
    if (FLeftmost = z) then
    begin
      if (z.Right = nil) then
      begin // z.left must be null also
        FLeftmost := z.Parent;
      end
      else
      begin
        FLeftmost := Minimum(x);
      end;
    end;
    if (FRightmost = z) then
    begin
      if (z.Left = nil) then
      begin // z.right must be null also
        FRightmost := z.Parent;
      end
      else
      begin // x == z.left
        FRightmost := Maximum(x);
      end;
    end;
  end;

  // Rebalance tree
  if (y.Color = clBlack) then
  begin
    while ((x <> FRoot) and ((x = nil) or (x.Color = clBlack))) do
    begin
      if (x = x_parent.Left) then
      begin
        w := x_parent.Right;
        if (w.Color = clRed) then
        begin
          w.Color := clBlack;
          x_parent.Color := clRed;
          RotateLeft(x_parent);
          w := x_parent.Right;
        end;
        if (((w.Left = nil) or (w.Left.Color = clBlack)) and
          ((w.Right = nil) or (w.Right.Color = clBlack))) then
        begin
          w.Color := clRed;
          x := x_parent;
          x_parent := x_parent.Parent;
        end
        else
        begin
          if ((w.Right = nil) or (w.Right.Color = clBlack)) then
          begin
            w.Left.Color := clBlack;
            w.Color := clRed;
            RotateRight(w);
            w := x_parent.Right;
          end;
          w.Color := x_parent.Color;
          x_parent.Color := clBlack;
          if (w.Right <> nil) then
          begin
            w.Right.Color := clBlack;
          end;
          RotateLeft(x_parent);
          x := FRoot; { break; }
        end
      end
      else
      begin
        { same as above, with right <. left. }
        w := x_parent.Left;
        if (w.Color = clRed) then
        begin
          w.Color := clBlack;
          x_parent.Color := clRed;
          RotateRight(x_parent);
          w := x_parent.Left;
        end;
        if (((w.Right = nil) or (w.Right.Color = clBlack)) and
          ((w.Left = nil) or (w.Left.Color = clBlack))) then
        begin
          w.Color := clRed;
          x := x_parent;
          x_parent := x_parent.Parent;
        end
        else
        begin
          if ((w.Left = nil) or (w.Left.Color = clBlack)) then
          begin
            w.Right.Color := clBlack;
            w.Color := clRed;
            RotateLeft(w);
            w := x_parent.Left;
          end;
          w.Color := x_parent.Color;
          x_parent.Color := clBlack;
          if (w.Left <> nil) then
          begin
            w.Left.Color := clBlack;
          end;
          RotateRight(x_parent);
          x := FRoot; // break;
        end;
      end;
    end;
    if (x <> nil) then
    begin
      x.Color := clBlack;
    end;
  end;
  while Assigned(y.Twin) do
  begin
    z := y;
    y := y.Twin;
    z.Destroy;
  end;
  y.Destroy;
  Dec(FCount);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function GRedBlackTree<TKey, TValue>.NextKey(var Key: TKey;
  out Value: TValue): Boolean;
var
  x, y: TRBNode;
begin
  if Assigned(FLastNode) and (FKeyCompareFunc(FLastNode.Key, Key) = 0) then
    x := FLastNode
  else
    x := FindNode(Key);
  if x = nil then
    exit;
  if (x.Right <> nil) then
  begin
    x := x.Right;
    while (x.Left <> nil) do
    begin
      x := x.Left;
    end;
  end
  else if (x.Parent <> nil) then
  begin
    y := x.Parent;
    while Assigned(y) and (x = y.Right) do
    begin
      x := y;
      y := y.Parent;
    end;
    if (x.Right <> y) then
      x := y;
  end
  else
    x := FRoot;
  if x = nil then
    exit(False);
  Key := x.Key;
  FLastNode := x;
  Value := x.Value;
  Result := True;
end;

function GRedBlackTree<TKey, TValue>.PrevKey(var Key: TKey;
  out Value: TValue): Boolean;
var
  x, y: TRBNode;
begin
  if Assigned(FLastNode) and (FKeyCompareFunc(FLastNode.Key, Key) = 0) then
    x := FLastNode
  else
    x := FindNode(Key);
  if x = nil then
    exit(False);
  if (x.Left <> nil) then
  begin
    y := x.Left;
    while (y.Right <> nil) do
    begin
      y := y.Right;
    end;
    x := y;
  end
  else if (x.Parent <> nil) then
  begin
    y := x.Parent;
    while (x = y.Left) do
    begin
      x := y;
      y := y.Parent;
    end;
    x := y;
  end
  else
    x := FRoot;
  if x = nil then
    exit(False);
  Key := x.Key;
  FLastNode := x;
  Value := x.Value;
  Result := True;
end;

function GRedBlackTree<TKey, TValue>.GetFirst: TKey;
begin
  Result := FLeftmost.Key;
end;

function GRedBlackTree<TKey, TValue>.GetLast: TKey;
begin
  Result := FRightmost.Key;
end;

procedure GRedBlackTree<TKey, TValue>.ForEach(AProc: TForEachProc);
var
  x, y, z: TRBNode;
  cont: Boolean;
begin
  if Assigned(FLeftmost) then
  begin
    x := FLeftmost;
    repeat
      z := x;
      repeat
        AProc(z.Key, z.Value, cont);
        if not cont then
          exit;
        z := z.Twin;
      until z = nil;
      // Next node
      if (x.Right <> nil) then
      begin
        x := x.Right;
        while (x.Left <> nil) do
        begin
          x := x.Left;
        end;
      end
      else if (x.Parent <> nil) then
      begin
        y := x.Parent;
        while (x = y.Right) do
        begin
          x := y;
          y := y.Parent;
        end;
        if (x.Right <> y) then
          x := y;
      end
      else
        x := FRoot;
    until x = FRightmost;
    if cont and (FLeftmost <> FRightmost) then
      AProc(FRightmost.Key, FRightmost.Value, cont);
  end;
end;

procedure GRedBlackTree<TKey, TValue>.SetDuplicateKeys(Value: Boolean);
begin
  if Value and Assigned(FValueCompareFunc) then
    FDuplicateKeys := True
  else
    FDuplicateKeys := False;
end;

end.
