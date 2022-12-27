//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.XCollection;

(* A polymorphism-enabled TCollection-like set of classes *)

{ .$DEFINE DEBUG_XCOLLECTION }  // on define the most apps will not work

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  GLS.Strings,
  GLS.PersistentClasses
{$IFDEF DEBUG_XCOLLECTION}, System.TypInfo {$ENDIF};

type
  TXCollection = class;

  EFilerException = class(Exception)
  end;

  (* Base class for implementing a XCollection item.
    NOTES :
    Don't forget to override the ReadFromFiler/WriteToFiler persistence
    methods if you add data in a subclass !
    Subclasses must be registered using the RegisterXCollectionItemClass
    function for proper operation *)
  TXCollectionItem = class(TGLInterfacedPersistent)
  private
    FOwner: TXCollection;
    FName: string;
  protected
    function GetName: string; virtual;
    procedure SetName(const val: String); virtual;
    function GetOwner: TPersistent; override;
    // Override this function to write subclass data
    procedure WriteToFiler(writer: TWriter); virtual;
    // Override this function to read subclass data
    procedure ReadFromFiler(reader: TReader); virtual;
    // Override to perform things when owner form has been loaded
    procedure Loaded; dynamic;
    // Triggers an EFilerException with appropriate version message
    procedure RaiseFilerException(const archiveVersion: integer);
  public
    constructor Create(aOwner: TXCollection); virtual;
    destructor Destroy; override;
    function GetNamePath: string; override;
    property Owner: TXCollection read FOwner;
    // Default implementation uses WriteToFiler/ReadFromFiler
    procedure Assign(Source: TPersistent); override;
    procedure MoveUp;
    procedure MoveDown;
    function Index: integer;
    (* Returns a user-friendly denomination for the class.
      This denomination is used for picking a texture image class
      in the IDE expert *)
    class function FriendlyName: String; virtual; abstract;
    (* Returns a user-friendly description for the class.
      This denomination is used for helping the user when picking a
      texture image class in the IDE expert. If it's not overriden,
      takes its value from FriendlyName. *)
    class function FriendlyDescription: String; virtual;
    (* Category of the item class.
      This is a free string, it will used by the XCollectionEditor to
      regroup collectionitems and menu items *)
    class function ItemCategory: string; virtual;
    (* If true only one such XCollectionItem is allowed per BaseSceneObject.
      Inheritance is accounted for UniqueXCollectionItem resolution, ie.
      if TClassA is unique, and TClassB is a subclass of TClassA,
      whatever the unicity of TClassB, TClassA and TClassB won't be allowed
      to mix (since TClassB is a TClassA, and TClassA is unique).
      Attempting to break the unicity rules will not be possible at
      design-time (in Delphi IDE) and will trigger an exception at run-time. *)
    class function UniqueItem: Boolean; virtual;
    (* Allows the XCollectionItem class to determine if it should be allowed
      to be added to the given collection. *)
    class function CanAddTo(collection: TXCollection): Boolean; virtual;
  published
    property Name: string read FName write SetName;
  end;

  TXCollectionItemClass = class of TXCollectionItem;

  (* Holds a list of TXCollectionItem objects.
    This class looks a lot like a polymorphic-enabled TCollection, it is
    a much stripped down version of a proprietary TObjectList,
    if the copyrights are ever partially lifted
    on the originals, I'll base this code on them since they are way faster
    than VCL's lists and persistence mechanisms with polymorphism-support 
	and full backward compatibility *)
  TXCollection = class(TPersistent)
  private
    FOwner: TPersistent;
    FList: TList;
    FCount: integer;
    // Archive Version is used to update the way data items is loaded
    FArchiveVersion: integer;
  protected
    function GetItems(Index: integer): TXCollectionItem;
    function GetOwner: TPersistent; override;
    procedure ReadFromFiler(reader: TReader);
    (* Here, we write all listed XCollection through their WriteToFiler methods,
       but to be able to restore them, we also write their classname, and to
       avoid wasting space if the same class appears multiple times we build up
       a lookup table while writing them, if the class is anew, the name is
       written, otherwise, only the index in the table is written.
       Using a global lookup table (instead of a "per-WriteData" one) could save
       more space, but would also increase dependencies, and this I don't want 8)*)
    procedure WriteToFiler(writer: TWriter);
  public
    constructor Create(aOwner: TPersistent); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
    property Owner: TPersistent read FOwner write FOwner;
    function GetNamePath: string; override;
    (* Class of the items.
      Unlike TCollection, items can be of ItemsClass OR ANY of its
      subclasses, ie. this function is used only for asserting your adding
      objects of the right class, and not for persistence. *)
    class function ItemsClass: TXCollectionItemClass; virtual;
    property Items[index: integer]: TXCollectionItem read GetItems; default;
    property Count: integer read FCount;
    function Add(anItem: TXCollectionItem): integer;
    function GetOrCreate(anItem: TXCollectionItemClass): TXCollectionItem;
    procedure Delete(Index: integer);
    procedure Remove(anItem: TXCollectionItem);
    procedure Clear;
    function IndexOf(anItem: TXCollectionItem): integer;
    // Returns the index of the first XCollectionItem of the given class (or -1)
    function IndexOfClass(aClass: TXCollectionItemClass): integer;
    // Returns the first XCollection of the given class (or nil)
    function GetByClass(aClass: TXCollectionItemClass): TXCollectionItem;
    // Returns the index of the first XCollectionItem of the given name (or -1)
    function IndexOfName(const aName: string): integer;
    (* Indicates if an object of the given class can be added.
      This function is used to enforce Unique XCollection. *)
    function CanAdd(aClass: TXCollectionItemClass): Boolean; virtual;
    property ArchiveVersion: integer read FArchiveVersion;
  end;

// Registers an event to be called when an XCollection is destroyed
procedure RegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);
// DeRegisters event
procedure DeRegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);
// Registers a TXCollectionItem subclass for persistence requirements
procedure RegisterXCollectionItemClass(aClass: TXCollectionItemClass);
// Removes a TXCollectionItem subclass from the list
procedure UnregisterXCollectionItemClass(aClass: TXCollectionItemClass);
// Retrieves a registered TXCollectionItemClass from its classname
function FindXCollectionItemClass(const ClassName: string): TXCollectionItemClass;
(* Creates and returns a copy of internal list of TXCollectionItem classes.
  Returned list should be freed by caller, the parameter defines an ancestor
  class filter. If baseClass is left nil, TXCollectionItem is used as ancestor. *)
function GetXCollectionItemClassesList(baseClass: TXCollectionItemClass = nil): TList;
procedure GetXCollectionClassesList(var ClassesList: TList;
  baseClass: TXCollectionItemClass = nil);

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  (* Magic is a workaround that will allow us to know
     if the archive version is 0 (equivalent to:
	 there is no ArchiveVersion stored in the DFM file) *)
  MAGIC: array [0 .. 3] of AnsiChar = 'XCOL';

var
  vXCollectionItemClasses: TList;
  vXCollectionDestroyEvent: TNotifyEvent;

// ---------- internal global routines (used by xcollection editor) -------------

procedure RegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);
begin
  vXCollectionDestroyEvent := notifyEvent;
end;

procedure DeRegisterXCollectionDestroyEvent(notifyEvent: TNotifyEvent);
begin
  vXCollectionDestroyEvent := nil;
end;

// ------------------------------------------------------------------------------

procedure RegisterXCollectionItemClass(aClass: TXCollectionItemClass);
begin
  if not Assigned(vXCollectionItemClasses) then
    vXCollectionItemClasses := TList.Create;
  if vXCollectionItemClasses.IndexOf(aClass) < 0 then
    vXCollectionItemClasses.Add(aClass);
end;

procedure UnregisterXCollectionItemClass(aClass: TXCollectionItemClass);
begin
  if not Assigned(vXCollectionItemClasses) then
    exit;
  if vXCollectionItemClasses.IndexOf(aClass) >= 0 then
    vXCollectionItemClasses.Remove(aClass);
end;

function FindXCollectionItemClass(const ClassName: String): TXCollectionItemClass;
var
  i: integer;
begin
  Result := nil;
  if Assigned(vXCollectionItemClasses) then
    for i := 0 to vXCollectionItemClasses.Count - 1 do
      if TXCollectionItemClass(vXCollectionItemClasses[i]).ClassName = ClassName
      then
      begin
        Result := TXCollectionItemClass(vXCollectionItemClasses[i]);
        Break;
      end;
end;

function GetXCollectionItemClassesList(baseClass: TXCollectionItemClass = nil): TList;
begin
  Result := TList.Create;
  GetXCollectionClassesList(Result, baseClass);
end;

procedure GetXCollectionClassesList(var ClassesList: TList;
  baseClass: TXCollectionItemClass = nil);
var
  i: integer;
begin
  if not Assigned(baseClass) then
    baseClass := TXCollectionItem;
  if Assigned(vXCollectionItemClasses) then
    for i := 0 to vXCollectionItemClasses.Count - 1 do
      if TXCollectionItemClass(vXCollectionItemClasses[i]).InheritsFrom(baseClass) then
        ClassesList.Add(vXCollectionItemClasses[i]);
end;

// ------------------
// ------------------ TXCollectionItem ------------------
// ------------------

constructor TXCollectionItem.Create(aOwner: TXCollection);
begin
  inherited Create;
  FOwner := aOwner;
  if Assigned(aOwner) then
  begin
    Assert(aOwner.CanAdd(TXCollectionItemClass(Self.ClassType)),
      'Addition of ' + Self.ClassName + ' to ' + aOwner.ClassName +
      ' rejected.');
    aOwner.FList.Add(Self);
    aOwner.FCount := aOwner.FList.Count;
  end;
  FName := FriendlyName;
end;

destructor TXCollectionItem.Destroy;
begin
  if Assigned(FOwner) then
  begin
    FOwner.FList.Remove(Self);
    FOwner.FCount := FOwner.FList.Count;
  end;
  inherited Destroy;
end;

procedure TXCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TXCollectionItem then
  begin
    FName := TXCollectionItem(Source).Name;
  end
  else
    inherited Assign(Source);
end;

procedure TXCollectionItem.SetName(const val: string);
begin
  FName := val;
end;

function TXCollectionItem.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TXCollectionItem.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteString(FName);
  end;
end;

procedure TXCollectionItem.ReadFromFiler(reader: TReader);
{$IFOPT C+}
var
  ver: integer;
{$ENDIF}
begin
  with reader do
  begin
{$IFOPT C+}
    ver := ReadInteger;
    Assert(ver = 0);
{$ENDIF}
    FName := ReadString;
  end;
end;

procedure TXCollectionItem.Loaded;
begin
  // does nothing by default
end;

function TXCollectionItem.GetName: string;
begin
  Result := FName;
end;

function TXCollectionItem.GetNamePath: string;
begin
  if FOwner <> nil then
    Result := Format('%s[%d]', [FOwner.GetNamePath, Index])
  else
    Result := inherited GetNamePath;
end;

procedure TXCollectionItem.MoveUp;
var
  i: integer;
begin
  if Assigned(Owner) then
  begin
    i := Owner.FList.IndexOf(Self);
    if i > 0 then
      Owner.FList.Exchange(i, i - 1);
  end;
end;

procedure TXCollectionItem.MoveDown;
var
  i: integer;
begin
  if Assigned(Owner) then
  begin
    i := Owner.FList.IndexOf(Self);
    if Cardinal(i) < Cardinal(Owner.FList.Count - 1) then
      Owner.FList.Exchange(i, i + 1);
  end;
end;

function TXCollectionItem.Index: integer;
begin
  if Assigned(Owner) then
    Result := Owner.FList.IndexOf(Self)
  else
    Result := -1;
end;

procedure TXCollectionItem.RaiseFilerException(const archiveVersion: integer);
begin
  raise EFilerException.Create(ClassName + strUnknownArchiveVersion +
    IntToStr(archiveVersion));
end;

class function TXCollectionItem.FriendlyDescription: string;
begin
  Result := FriendlyName;
end;

class function TXCollectionItem.ItemCategory: string;
begin
  Result := '';
end;

class function TXCollectionItem.UniqueItem: Boolean;
begin
  Result := False;
end;

class function TXCollectionItem.CanAddTo(collection: TXCollection): Boolean;
begin
  Result := True;
end;

// ------------------
// ------------------ TXCollection ------------------
// ------------------

constructor TXCollection.Create(aOwner: TPersistent);
begin
  inherited Create;
  FOwner := aOwner;
  FList := TList.Create;
end;

destructor TXCollection.Destroy;
begin
  if Assigned(vXCollectionDestroyEvent) then
    vXCollectionDestroyEvent(Self);
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TXCollection.Assign(Source: TPersistent);
var
  i: integer;
  srcItem, newItem: TXCollectionItem;
begin
  if not Assigned(Source) then
  begin
    Clear;
  end
  else if Source.ClassType = Self.ClassType then
  begin
    Clear;
    FList.Capacity := TXCollection(Source).FList.Count;
    for i := 0 to TXCollection(Source).Count - 1 do
    begin
      srcItem := TXCollectionItem(TXCollection(Source).FList[i]);
      newItem := TXCollectionItemClass(srcItem.ClassType).Create(Self);
      newItem.Assign(srcItem);
    end;
  end
  else
    inherited Assign(Source);
  FCount := FList.Count;
end;

procedure TXCollection.Loaded;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    TXCollectionItem(FList[i]).Loaded;
end;

procedure TXCollection.WriteToFiler(writer: TWriter);
var
  i, n: integer;
  classList: TList;
  XCollectionItem: TXCollectionItem;
begin
  FArchiveVersion := 1;
  classList := TList.Create;
  try
    with writer do
    begin
      // Magic header and archive version are always written now
      WriteInteger(PInteger(@MAGIC[0])^);
      WriteInteger(FArchiveVersion);

      WriteInteger(FList.Count);
      for i := 0 to FList.Count - 1 do
      begin
        XCollectionItem := TXCollectionItem(FList[i]);
        n := classList.IndexOf(XCollectionItem.ClassType);
        if n < 0 then
        begin
          WriteString(XCollectionItem.ClassName);
          classList.Add(XCollectionItem.ClassType);
        end
        else
          WriteInteger(n);
        XCollectionItem.WriteToFiler(writer);
      end;
    end;
  finally
    classList.Free;
  end;
end;

procedure TXCollection.ReadFromFiler(reader: TReader);
var
  vt: TValueType;
  Header: array [0 .. 3] of AnsiChar;
  n, lc, lcnum: integer;
  classList: TList;
  cName: string;
  XCollectionItemClass: TXCollectionItemClass;
  XCollectionItem: TXCollectionItem;
begin
  // see WriteData for a description of what is going on here
  Clear;
  classList := TList.Create;
  try
    with reader do
    begin
      // save current reader position, it will be used to rewind the reader if the DFM is too old
      try
        vt := NextValue;
        if vt in [vaInt32, vaInt16, vaInt8] then
          PInteger(@Header[0])^ := ReadInteger
        else
        begin
          Read(Header[0], Length(Header));
        end;
      except
        Header[0] := #0;
        Header[1] := #0;
        Header[2] := #0;
        Header[3] := #0;
      end;

      // after reading the header, we need to compare it with the MAGIC reference
      if (Header[0] = MAGIC[0]) and (Header[1] = MAGIC[1]) and
        (Header[2] = MAGIC[2]) and (Header[3] = MAGIC[3]) then
      begin
        // if its ok we can just read the archive version
        FArchiveVersion := ReadInteger;
        lc := ReadInteger;
      end
      else
      begin
        // if the header is invalid (old DFM) just assume archive version is 0 and rewind reader
        FArchiveVersion := 0;
        lc := PInteger(@Header[0])^;
      end;

      for n := 1 to lc do
      begin
        if NextValue in [vaString, vaLString] then
        begin
          cName := ReadString;
{$IFDEF DEBUG_XCOLLECTION}
          writeln('TXCollection.ReadFromFiler create class entry: ', cName);
{$ENDIF}
          XCollectionItemClass := FindXCollectionItemClass(cName);
          Assert(Assigned(XCollectionItemClass),
            'Class ' + cName +
            ' unknown. Add the relevant unit to your "uses".');
          classList.Add(XCollectionItemClass);
        end
        else
        begin
{$IFDEF DEBUG_XCOLLECTION}
          Assert(NextValue in [vaInt8, vaInt16, vaInt32],
            'Non-Integer ValueType: ' + GetEnumName(TypeInfo(TValueType),
            Ord(NextValue)));
{$ENDIF}
          lcnum := ReadInteger;
          Assert((lcnum >= 0) and (lcnum < classList.Count),
            'Invalid classlistIndex: ' + IntToStr(lcnum));
          XCollectionItemClass := TXCollectionItemClass(classList[lcnum]);
{$IFDEF DEBUG_XCOLLECTION}
          writeln('TXCollection.ReadFromFiler create by number: ', lcnum,
            ' -> ', XCollectionItemClass.ClassName);
{$ENDIF}
        end;

        if Assigned(XCollectionItemClass) then
        begin
          XCollectionItem := XCollectionItemClass.Create(Self);
          XCollectionItem.ReadFromFiler(reader);
        end;
      end;
    end;
  finally
    classList.Free;
  end;
  FCount := FList.Count;
end;

class function TXCollection.ItemsClass: TXCollectionItemClass;
begin
  Result := TXCollectionItem;
end;

function TXCollection.GetItems(Index: integer): TXCollectionItem;
begin
  Result := TXCollectionItem(FList[index]);
end;

function TXCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TXCollection.GetNamePath: String;
var
  s: String;
begin
  Result := ClassName;
  if GetOwner = nil then
    exit;
  s := GetOwner.GetNamePath;
  if s = '' then
    exit;
  Result := s + '.XCollection';
end;

function TXCollection.Add(anItem: TXCollectionItem): integer;
begin
  Assert(anItem.InheritsFrom(ItemsClass));
  Assert(CanAdd(TXCollectionItemClass(anItem.ClassType)));
  if Assigned(anItem.FOwner) then
  begin
    anItem.FOwner.FList.Remove(anItem);
    anItem.FOwner.FCount := anItem.FOwner.FList.Count;
  end;
  anItem.FOwner := Self;
  Result := FList.Add(anItem);
  FCount := FList.Count;
end;

function TXCollection.GetOrCreate(anItem: TXCollectionItemClass)
  : TXCollectionItem;
var
  i: integer;
begin
  Assert(anItem.InheritsFrom(ItemsClass));
  i := Self.IndexOfClass(anItem);
  if i >= 0 then
    Result := TXCollectionItem(Self[i])
  else
    Result := anItem.Create(Self);
end;

procedure TXCollection.Delete(Index: integer);
begin
  Assert(Cardinal(index) < Cardinal(FList.Count));
  // doin' it the fast way
  with TXCollectionItem(FList[index]) do
  begin
    FOwner := nil;
    Free;
  end;
  FList.Delete(index);
  FCount := FList.Count;
end;

procedure TXCollection.Remove(anItem: TXCollectionItem);
var
  i: integer;
begin
  i := IndexOf(anItem);
  if i >= 0 then
    Delete(i);
end;

procedure TXCollection.Clear;
var
  i: integer;
begin
  // Fast kill of owned XCollection
  for i := 0 to FList.Count - 1 do
    with TXCollectionItem(FList[i]) do
    begin
      FOwner := nil;
      Free;
    end;
  FList.Clear;
  FCount := 0;
end;

function TXCollection.IndexOf(anItem: TXCollectionItem): integer;
begin
  Result := FList.IndexOf(anItem);
end;

function TXCollection.IndexOfClass(aClass: TXCollectionItemClass): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FList.Count - 1 do
    if TXCollectionItem(FList[i]) is aClass then
    begin
      Result := i;
      Break;
    end;
end;

function TXCollection.GetByClass(aClass: TXCollectionItemClass): TXCollectionItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
    if TXCollectionItem(FList[i]) is aClass then
    begin
      Result := TXCollectionItem(FList[i]);
      Break;
    end;
end;

function TXCollection.IndexOfName(const aName: string): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FList.Count - 1 do
    if TXCollectionItem(FList[i]).Name = aName then
    begin
      Result := i;
      Break;
    end;
end;

function TXCollection.CanAdd(aClass: TXCollectionItemClass): Boolean;
var
  i: integer;
  XCollectionItemClass: TXCollectionItemClass;
begin
  Result := True;
  // Test if the class allows itself to be added to this collection
  if not aClass.CanAddTo(Self) then
  begin
    Result := False;
    Exit;
  end;
  // is the given class compatible with owned ones ?
  if aClass.UniqueItem then
    for i := 0 to Count - 1 do
    begin
      if Items[i] is aClass then
      begin
        Result := False;
        Break;
      end;
    end;
  // are the owned classes compatible with the given one ?
  if Result then
    for i := 0 to Count - 1 do
    begin
      XCollectionItemClass := TXCollectionItemClass(Items[i].ClassType);
      if (XCollectionItemClass.UniqueItem) and
        aClass.InheritsFrom(XCollectionItemClass) then
      begin
        Result := False;
        Break;
      end;
    end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

finalization

vXCollectionItemClasses.Free;

end.
