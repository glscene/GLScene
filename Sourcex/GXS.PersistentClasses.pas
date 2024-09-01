//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.PersistentClasses;

(*
   Base persistence classes.

   These classes are used in GXScene, but are designed for generic purpose.
   They implement a slightly different persistence mechanism than that of the FMX,
   allowing for object-level versioning (100% backward compatibility) and full
   polymorphic persistence.

   Internal Note: stripped down versions of XClasses & XLists.
*)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.Strings;

type
  PObject = ^TObject;

  // Virtual layer similar to VCL's TReader (but reusable) }
  TgxVirtualReader = class
  private
    FStream: TStream;
  public
    constructor Create(Stream: TStream); virtual;
    property Stream: TStream read FStream;
    procedure ReadTypeError;
    procedure Read(var Buf; Count: Longint); virtual; abstract;
    function NextValue: TValueType; virtual; abstract;
    function ReadInteger: Integer; virtual; abstract;
    function ReadBoolean: Boolean; virtual; abstract;
    function ReadString: string; virtual; abstract;
    function ReadFloat: Extended; virtual; abstract;
    procedure ReadListBegin; virtual; abstract;
    procedure ReadListEnd; virtual; abstract;
    function EndOfList: Boolean; virtual; abstract;
    procedure ReadTStrings(aStrings: TStrings);
  end;

  // Virtual layer similar to VCL's TWriter (but reusable)
  TgxVirtualWriter = class
  private
    FStream: TStream;
  public
    constructor Create(Stream: TStream); virtual;
    property Stream: TStream read FStream;
    procedure Write(const Buf; Count: Longint); virtual; abstract;
    procedure WriteInteger(anInteger: Integer); virtual; abstract;
    procedure WriteBoolean(aBoolean: Boolean); virtual; abstract;
    procedure WriteString(const aString: string); virtual; abstract;
    procedure WriteFloat(const aFloat: Extended); virtual; abstract;
    procedure WriteListBegin; virtual; abstract;
    procedure WriteListEnd; virtual; abstract;
    procedure WriteTStrings(const aStrings: TStrings; storeObjects: Boolean = True);
  end;

  TgxVirtualReaderClass = class of TgxVirtualReader;
  TgxVirtualWriterClass = class of TgxVirtualWriter;

  (* Interface for persistent objects.
     This interface does not really allow polymorphic persistence,
     but is rather intended as a way to unify persistence calls for iterators. *)
  IgxPersistentObject = interface(IInterface)
  ['{A9A0198A-F11B-4325-A92C-2F24DB41652B}']
    procedure WriteToFiler(writer: TgxVirtualWriter);
    procedure ReadFromFiler(reader: TgxVirtualReader);
  end;

    (* Base class for persistent objects.
       The base requirement is implementation of ReadFromFiler & WriteToFiler
       in sub-classes, the immediate benefits are support of streaming (to stream,
       file or string), assignment and cloning.
       The other requirement being the use of a virtual constructor, which allows
       polymorphic construction (don't forget to register your subclasses).
       Note that TgxPersistentObject implements IUnknown, but does *not* implement
       reference counting. *)
  TgxPersistentObject = class(TPersistent, IgxPersistentObject)
  protected
    procedure RaiseFilerException(const archiveVersion: Integer);
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create; virtual;
    constructor CreateFromFiler(reader: TgxVirtualReader);
    destructor Destroy; override;
    procedure Assign(source: TPersistent); override;
    function CreateClone: TgxPersistentObject; virtual;
    class function FileSignature: string; virtual;
    class function FileVirtualWriter: TgxVirtualWriterClass; virtual;
    class function FileVirtualReader: TgxVirtualReaderClass; virtual;
    procedure WriteToFiler(writer: TgxVirtualWriter); virtual;
    procedure ReadFromFiler(reader: TgxVirtualReader); virtual;
    procedure SaveToStream(stream: TStream; writerClass: TgxVirtualWriterClass = nil); virtual;
    procedure LoadFromStream(stream: TStream; readerClass: TgxVirtualReaderClass = nil); virtual;
    procedure SaveToFile(const fileName: string; writerClass: TgxVirtualWriterClass = nil); virtual;
    procedure LoadFromFile(const fileName: string; readerClass: TgxVirtualReaderClass = nil); virtual;
    function SaveToString(writerClass: TgxVirtualWriterClass = nil): string; virtual;
    procedure LoadFromString(const data: string; readerClass: TgxVirtualReaderClass = nil); virtual;
  end;

  TGLPersistentObjectClass = class of TgxPersistentObject;
  TgxPointerObjectList = array[0..MaxInt div (2*SizeOf(Pointer))] of TObject;
  PgxPointerObjectList = ^TgxPointerObjectList;
  TObjectListSortCompare = function(item1, item2: TObject): Integer;

  (* A persistent Object list.
     Similar to TList but works on TObject items and has facilities for
     persistence of contained data. Unlike the VCL's TObjectList, this one
     does NOT free its objects upon destruction or Clear, use Clean and CleanFree
     for that, and as such can be used for object referral lists too.
     But only TgxPersistentObject items will be streamed appropriately.
     The list can be used in a stack-like fashion with Push & Pop, and can
     perform basic boolean set operations.
     Note: the IndexOf implementation is up to 3 times faster than that of TList *)
  TgxPersistentObjectList = class(TgxPersistentObject)
  private
    FList: PgxPointerObjectList;
    FCount: Integer;
    FCapacity: Integer;
    FGrowthDelta: Integer;
  protected
    procedure Error; virtual;
    function Get(Index: Integer): TObject;
    procedure Put(Index: Integer; Item: TObject);
    procedure SetCapacity(newCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function GetFirst: TObject;
    procedure SetFirst(item: TObject);
    function GetLast: TObject;
    procedure SetLast(item: TObject);
    // Default event for ReadFromFiler
    procedure AfterObjectCreatedByReader(Sender: TObject); virtual;
    procedure DoClean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(reader: TgxVirtualReader); override;
    procedure ReadFromFilerWithEvent(reader: TgxVirtualReader;
      afterSenderObjectCreated: TNotifyEvent);
    function Add(const item: TObject): Integer;
    procedure AddNils(nbVals: Cardinal);
    procedure Delete(index: Integer);
    procedure DeleteItems(index: Integer; nbVals: Cardinal);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; Item: TObject);
    procedure InsertNils(index: Integer; nbVals: Cardinal);
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: TObject): Integer;
    procedure DeleteAndFree(index: Integer);
    procedure DeleteAndFreeItems(index: Integer; nbVals: Cardinal);
    function RemoveAndFree(item: TObject): Integer;
    property GrowthDelta: integer read FGrowthDelta write FGrowthDelta;
    function Expand: TgxPersistentObjectList;
    property Items[Index: Integer]: TObject read Get write Put; default;
    property Count: Integer read FCount write SetCount;
    property List: PgxPointerObjectList read FList;
    property Capacity: Integer read FCapacity write SetCapacity;
    // Makes sure capacity is at least aCapacity.
    procedure RequiredCapacity(aCapacity: Integer);
    (* Removes all "nil" from the list.
       Note: Capacity is unchanged, no memory us freed, the list is just
       made shorter. This functions is orders of magnitude faster than
       its TList eponymous. *)
    procedure Pack;
    // Empty the list without freeing the objects.
    procedure Clear; virtual;
    // Empty the list and free the objects.
    procedure Clean; virtual;
    // Empty the list, free the objects and Free self.
    procedure CleanFree;
    function IndexOf(Item: TObject): Integer;
    property First: TObject read GetFirst write SetFirst;
    property Last: TObject read GetLast write SetLast;
    procedure Push(item: TObject);
    function Pop: TObject;
    procedure PopAndFree;
    function AddObjects(const objectList: TgxPersistentObjectList): Integer;
    procedure RemoveObjects(const objectList: TgxPersistentObjectList);
    procedure Sort(compareFunc: TObjectListSortCompare);
  end;

  // Wraps a TReader-compatible reader.
  TgxBinaryReader = class(TgxVirtualReader)
  protected
    function ReadValue: TValueType;
    function ReadWideString(vType: TValueType): WideString;
  public
    procedure Read(var Buf; Count: Longint); override;
    function NextValue: TValueType; override;
    function ReadInteger: Integer; override;
    function ReadBoolean: Boolean; override;
    function ReadString: string; override;
    function ReadFloat: Extended; override;
    procedure ReadListBegin; override;
    procedure ReadListEnd; override;
    function EndOfList: Boolean; override;
  end;

  // Wraps a TWriter-compatible writer.
  TgxBinaryWriter = class(TgxVirtualWriter)
  protected
    procedure WriteAnsiString(const aString: AnsiString); virtual;
    procedure WriteWideString(const aString: WideString); virtual;
  public
    procedure Write(const Buf; Count: Longint); override;
    procedure WriteInteger(anInteger: Integer); override;
    procedure WriteBoolean(aBoolean: Boolean); override;
    procedure WriteString(const aString: string); override;
    procedure WriteFloat(const aFloat: Extended); override;
    procedure WriteListBegin; override;
    procedure WriteListEnd; override;
  end;

  // Reads object persistence in Text format.
  TgxTextReader = class(TgxVirtualReader)
  private
    FValueType: string;
    FData: string;
  protected
    procedure ReadLine(const requestedType: string = '');
  public
    procedure Read(var Buf; Count: Longint); override;
    function NextValue: TValueType; override;
    function ReadInteger: Integer; override;
    function ReadBoolean: Boolean; override;
    function ReadString: string; override;
    function ReadFloat: Extended; override;
    procedure ReadListBegin; override;
    procedure ReadListEnd; override;
    function EndOfList: Boolean; override;
  end;

  // Writes object persistence in Text format.
  TgxTextWriter = class(TgxVirtualWriter)
  private
    FIndentLevel: Integer;
  protected
    procedure WriteLine(const valueType, data: string);
  public
    constructor Create(aStream: TStream); override;
    destructor Destroy; override;
    procedure Write(const Buf; Count: Longint); override;
    procedure WriteInteger(anInteger: Integer); override;
    procedure WriteBoolean(aBoolean: Boolean); override;
    procedure WriteString(const aString: string); override;
    procedure WriteFloat(const aFloat: Extended); override;
    procedure WriteListBegin; override;
    procedure WriteListEnd; override;
  end;

  // TPersistent which has knowledge of its owner.
  TgxOwnedPersistent = class(TPersistent)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
  end;

  // TPersistent thet inplements IInterface.
  TgxInterfacedPersistent = class(TPersistent, IInterface)
  protected
    // Implementing IInterface.
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  // TCollectionItem thet inplements IInterface.
  TgxInterfacedCollectionItem = class(TCollectionItem, IInterface)
  protected
    // Implementing IInterface.
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  end;

  // Triggered when file signature does not match.
  EInvalidFileSignature = class(Exception)
  end;

  // Usually triggered when a filing error is detected.
  EFilerException = class(Exception)
  end;

procedure RaiseFilerException(aClass: TClass; archiveVersion: Integer);
function UTF8ToWideString(const s: AnsiString): WideString;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

const
  cDefaultListGrowthDelta = 16;

const
  cVTInteger = 'Int';
  cVTFloat = 'Float';
  cVTString = 'Str';
  cVTBoolean = 'Bool';
  cVTRaw = 'Raw';
  cVTListBegin = '{';
  cVTListEnd = '}';

  cTrue = 'True';
  cFalse = 'False';

procedure RaiseFilerException(aClass: TClass; archiveVersion: Integer);
begin
  raise EFilerException.Create(aClass.ClassName + 
    strUnknownArchiveVersion + IntToStr(archiveVersion));
end;

function UTF8ToWideString(const s: AnsiString): WideString;
const
  bytesFromUTF8: packed array[0..255] of Byte = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5);
  offsetsFromUTF8: array[0..5] of Cardinal = (
    $00000000, $00003080, $000E2080, $03C82080, $FA082080, $82082080);
  MaximumUCS2: Cardinal = $0000FFFF;
  MaximumUCS4: Cardinal = $7FFFFFFF;
  ReplacementCharacter: Cardinal = $0000FFFD;
  halfShift: Integer = 10;
  halfBase: Cardinal = $0010000;
  halfMask: Cardinal = $3FF;
  SurrogateHighStart: Cardinal = $D800;
  SurrogateLowStart: Cardinal = $DC00;
var
  sLength, L, J, T: Cardinal;
  ch: Cardinal;
  extraBytesToWrite: Word;
begin
  sLength := Length(s);
  if sLength = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, sLength); // create enough room

  L := 1;
  T := 1;
  while L <= Cardinal(sLength) do
  begin
    ch := 0;
    extraBytesToWrite := bytesFromUTF8[Ord(S[L])];
    for J := extraBytesToWrite downto 1 do
    begin
      ch := ch + Ord(S[L]);
      Inc(L);
      ch := ch shl 6;
    end;
    ch := ch + Ord(S[L]);
    Inc(L);
    ch := ch - offsetsFromUTF8[extraBytesToWrite];

    if ch <= MaximumUCS2 then
    begin
      Result[T] := WideChar(ch);
      Inc(T);
    end
    else if ch > MaximumUCS4 then
    begin
      Result[T] := WideChar(ReplacementCharacter);
      Inc(T);
    end
    else
    begin
      ch := ch - halfBase;
      Result[T] := WideChar((ch shr halfShift) + SurrogateHighStart);
      Inc(T);
      Result[T] := WideChar((ch and halfMask) + SurrogateLowStart);
      Inc(T);
    end;
  end;
  SetLength(Result, T - 1); // now fix up length
end;

// ------------------
// ------------------ TgxVirtualReader ------------------
// ------------------

constructor TgxVirtualReader.Create(Stream: TStream);
begin
  FStream := Stream;
end;

procedure TgxVirtualReader.ReadTypeError;
begin
  raise EReadError.CreateFmt('%s, read type error', [ClassName]);
end;

procedure TgxVirtualReader.ReadTStrings(aStrings: TStrings);
var
  i: Integer;
  objectsStored: Boolean;
begin
  aStrings.BeginUpdate;
  aStrings.Clear;
  objectsStored := ReadBoolean;
  i := ReadInteger;
  if objectsStored then
    while i > 0 do
    begin
      aStrings.AddObject(ReadString, TObject(Cardinal(ReadInteger)));
      Dec(i);
    end
  else
    while i > 0 do
    begin
      aStrings.Add(ReadString);
      Dec(i);
    end;
  aStrings.EndUpdate;
end;

// ------------------
// ------------------ TgxVirtualWriter ------------------
// ------------------

constructor TgxVirtualWriter.Create(Stream: TStream);
begin
  FStream := Stream;
end;

procedure TgxVirtualWriter.WriteTStrings(const aStrings: TStrings;
  storeObjects: Boolean = True);
var
  i: Integer;
begin
  writeBoolean(storeObjects);
  if Assigned(aStrings) then
  begin
    WriteInteger(aStrings.Count);
    if storeObjects then
      for i := 0 to aStrings.Count - 1 do
      begin
        WriteString(aStrings[i]);
        WriteInteger(Integer(aStrings.Objects[i]));
      end
    else
      for i := 0 to aStrings.Count - 1 do
        WriteString(aStrings[i]);
  end
  else
    WriteInteger(0);
end;

// ------------------
// ------------------ TgxPersistentObject ------------------
// ------------------

constructor TgxPersistentObject.Create;
begin
  inherited Create;
end;

constructor TgxPersistentObject.CreateFromFiler(reader: TgxVirtualReader);
begin
  Create;
  ReadFromFiler(reader);
end;

destructor TgxPersistentObject.Destroy;
begin
  inherited Destroy;
end;

procedure TgxPersistentObject.Assign(source: TPersistent);
var
  ms: TStringStream; // faster than a TMemoryStream...
begin
  if source.ClassType = Self.ClassType then
  begin
    ms := TStringStream.Create('');
    try
      TgxPersistentObject(source).SaveToStream(ms);
      ms.Position := 0;
      LoadFromStream(ms);
    finally
      ms.Free;
    end;
  end
  else
    inherited;
end;

function TgxPersistentObject.CreateClone: TgxPersistentObject;
begin
  Result := TGLPersistentObjectClass(Self.ClassType).Create;
  Result.Assign(Self);
end;

class function TgxPersistentObject.FileSignature: string;
begin
  Result := '';
end;

class function TgxPersistentObject.FileVirtualWriter: TgxVirtualWriterClass;
begin
  Result := TgxBinaryWriter;
end;

class function TgxPersistentObject.FileVirtualReader: TgxVirtualReaderClass;
begin
  Result := TgxBinaryReader;
end;

procedure TgxPersistentObject.WriteToFiler(writer: TgxVirtualWriter);
begin
  // nothing
  Assert(Assigned(writer));
end;

procedure TgxPersistentObject.ReadFromFiler(reader: TgxVirtualReader);
begin
  // nothing
  Assert(Assigned(reader));
end;

procedure TgxPersistentObject.RaiseFilerException(const archiveVersion: Integer);
begin
  raise EFilerException.Create(ClassName + strUnknownArchiveVersion + IntToStr(archiveVersion)); //IGNORE
end;

function TgxPersistentObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TgxPersistentObject._AddRef: Integer; stdcall;
begin
  // ignore
  Result := 1;
end;

function TgxPersistentObject._Release: Integer; stdcall;
begin
  // ignore
  Result := 0;
end;

procedure TgxPersistentObject.SaveToStream(stream: TStream; writerClass: TgxVirtualWriterClass = nil);
var
  wr: TgxVirtualWriter;
  fileSig: AnsiString;
begin
  if writerClass = nil then
    writerClass := TgxBinaryWriter;
  wr := writerClass.Create(stream);
  try
    if FileSignature <> '' then
    begin
      fileSig := AnsiString(FileSignature);
      wr.Write(fileSig[1], Length(fileSig));
    end;
    WriteToFiler(wr);
  finally
    wr.Free;
  end;
end;

procedure TgxPersistentObject.LoadFromStream(stream: TStream; readerClass: TgxVirtualReaderClass = nil);
var
  rd: TgxVirtualReader;
  sig: AnsiString;
begin
  if readerClass = nil then
    readerClass := TgxBinaryReader;
  rd := readerClass.Create(stream);
  try
    if FileSignature <> '' then
    begin
      SetLength(sig, Length(FileSignature));
      rd.Read(sig[1], Length(FileSignature));
      if sig <> AnsiString(FileSignature) then
        raise EInvalidFileSignature.Create(strInvalidFileSignature);
    end;
    ReadFromFiler(rd);
  finally
    rd.Free;
  end;
end;

procedure TgxPersistentObject.SaveToFile(const fileName: string; writerClass: TgxVirtualWriterClass = nil);
var
  fs: TStream;
begin
  if writerClass = nil then
    writerClass := FileVirtualWriter;
  fs := TFileStream.Create(fileName, fmCreate);
  try
    SaveToStream(fs, writerClass);
  finally
    fs.Free;
  end;
end;

procedure TgxPersistentObject.LoadFromFile(const fileName: string; readerClass: TgxVirtualReaderClass = nil);
var
  fs: TStream;
begin
  if readerClass = nil then
    readerClass := FileVirtualReader;
  fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(fs, readerClass);
  finally
    fs.Free;
  end;
end;

function TgxPersistentObject.SaveToString(writerClass: TgxVirtualWriterClass = nil): string;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('');
  try
    SaveToStream(ss, writerClass);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

procedure TgxPersistentObject.LoadFromString(const data: string; readerClass: TgxVirtualReaderClass = nil);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(data);
  try
    LoadFromStream(ss, readerClass);
  finally
    ss.Free;
  end;
end;

// ------------------
// ------------------ TgxPersistentObjectList ------------------
// ------------------

constructor TgxPersistentObjectList.Create;
begin
  inherited Create;
  FGrowthDelta := cDefaultListGrowthDelta;
end;

destructor TgxPersistentObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TgxPersistentObjectList.Add(const item: TObject): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TgxPersistentObjectList.AddNils(nbVals: Cardinal);
begin
  if Integer(nbVals) + Count > Capacity then
    SetCapacity(Integer(nbVals) + Count);
  FillChar(FList[FCount], Integer(nbVals) * SizeOf(TObject), 0);
  FCount := FCount + Integer(nbVals);
end;

function TgxPersistentObjectList.AddObjects(const objectList: TgxPersistentObjectList): Integer;
begin
  if Assigned(objectList) then
  begin
    Result := FCount;
    SetCount(Result + objectList.Count);
    System.Move(objectList.FList^[0], FList^[Result],
      objectList.FCount * SizeOf(TObject));
  end
  else
    Result := 0;
end;

procedure TgxPersistentObjectList.RemoveObjects(const objectList: TgxPersistentObjectList);
var
  i: Integer;
begin
  for i := 0 to objectList.Count - 1 do
    Remove(objectList[i]);
end;

procedure TgxPersistentObjectList.Clear;
begin
  if Assigned(Self) and Assigned(FList) then
  begin
    SetCount(0);
    SetCapacity(0);
  end;
end;

procedure TgxPersistentObjectList.Delete(index: Integer);
begin
{$IFOPT R+}
  if Cardinal(Index) >= Cardinal(FCount) then
    Error;
{$ENDIF}
  Dec(FCount);
  if index < FCount then
    System.Move(FList[index + 1], FList[index], (FCount - index) * SizeOf(TObject));
end;

procedure TgxPersistentObjectList.DeleteItems(index: Integer; nbVals: Cardinal);
begin
{$IFOPT R+}
  Assert(Cardinal(index) < Cardinal(FCount));
{$ENDIF}
  if nbVals > 0 then
  begin
    if index + Integer(nbVals) < FCount then
    begin
      System.Move(FList[index + Integer(nbVals)],
        FList[index],
        (FCount - index - Integer(nbVals)) * SizeOf(TObject));
    end;
    Dec(FCount, nbVals);
  end;
end;

procedure TgxPersistentObjectList.Exchange(index1, index2: Integer);
var
  item: TObject;
  locList: PgxPointerObjectList;
begin
{$IFOPT R+}
  if (Cardinal(Index1) >= Cardinal(FCount)) or
    (Cardinal(Index2) >= Cardinal(FCount)) then
    Error;
{$ENDIF}
  locList := FList;
  item := locList^[index1];
  locList^[index1] := locList^[index2];
  locList^[index2] := item;
end;

function TgxPersistentObjectList.Expand: TgxPersistentObjectList;
begin
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  Result := Self;
end;

function TgxPersistentObjectList.GetFirst: TObject;
begin
{$IFOPT R+}
  if Cardinal(FCount) = 0 then
    Error;
{$ENDIF}
  Result := FList^[0];
end;

procedure TgxPersistentObjectList.SetFirst(item: TObject);
begin
{$IFOPT R+}
  if Cardinal(FCount) = 0 then
    Error;
{$ENDIF}
  FList^[0] := item;
end;

procedure TgxPersistentObjectList.Error;
begin
  raise EListError.Create(strListIndexError);
end;

function TgxPersistentObjectList.Get(Index: Integer): TObject;
begin
{$IFOPT R+}
  if Cardinal(Index) >= Cardinal(FCount) then
    Error;
{$ENDIF}
  Result := FList^[Index];
end;

function TgxPersistentObjectList.IndexOf(Item: TObject): Integer;
var
  I: Integer;
begin
  if FCount <= 0 then
    Result := -1
  else
  begin
    Result := -1;
    for I := 0 to FCount - 1 do
      if FList^[I] = Item then
      begin
        Result := I;
        Exit;
      end;
  end;
end;

procedure TgxPersistentObjectList.Insert(index: Integer; item: TObject);
begin
{$IFOPT R+}
  if Cardinal(index) > Cardinal(FCount) then
    Error;
{$ENDIF}
  if FCount = FCapacity then
    SetCapacity(FCapacity + FGrowthDelta);
  if Index < FCount then
    System.Move(FList[index], FList[index + 1],
      (FCount - index) * SizeOf(TObject));
  FList^[index] := item;
  Inc(FCount);
end;

procedure TgxPersistentObjectList.InsertNils(index: Integer; nbVals: Cardinal);
var
  nc: Integer;
begin
{$IFOPT R+}
  Assert(Cardinal(Index) <= Cardinal(FCount));
{$ENDIF}
  if nbVals > 0 then
  begin
    nc := FCount + Integer(nbVals);
    if nc > FCapacity then
      SetCapacity(nc);
    if Index < FCount then
      System.Move(FList[Index], FList[Index + Integer(nbVals)],
        (FCount - Index) * SizeOf(TObject));
    FillChar(FList[Index], Integer(nbVals) * SizeOf(TObject), 0);
    FCount := nc;
  end;
end;

function TgxPersistentObjectList.GetLast: TObject;
begin
{$IFOPT R+}
  if Cardinal(FCount) = 0 then
    Error;
{$ENDIF}
  Result := FList^[FCount - 1];
end;

procedure TgxPersistentObjectList.SetLast(item: TObject);
begin
{$IFOPT R+}
  if Cardinal(FCount) = 0 then
    Error;
{$ENDIF}
  FList^[FCount - 1] := item;
end;

procedure TgxPersistentObjectList.Move(CurIndex, NewIndex: Integer);
var
  item: Pointer;
begin
  if curIndex <> newIndex then
  begin
{$IFOPT R+}
    if Cardinal(newIndex) >= Cardinal(Count) then
      Error;
    if Cardinal(curIndex) >= Cardinal(Count) then
      Error;
{$ENDIF}
    item := FList^[curIndex];
    if curIndex < newIndex then
    begin
      // curIndex+1 necessarily exists since curIndex<newIndex and newIndex<Count
      System.Move(List[curIndex + 1], List[curIndex], (NewIndex - CurIndex) * SizeOf(TObject));
    end
    else
    begin
      // newIndex+1 necessarily exists since newIndex<curIndex and curIndex<Count
      System.Move(List[newIndex], List[newIndex + 1], (CurIndex - NewIndex) * SizeOf(TObject));
    end;
    FList^[newIndex] := TObject(item);
  end;
end;

procedure TgxPersistentObjectList.Put(Index: Integer; Item: TObject);
begin
{$IFOPT R+}
  if Cardinal(Index) >= Cardinal(FCount) then
    Error;
{$ENDIF}
  FList^[Index] := Item;
end;

function TgxPersistentObjectList.Remove(item: TObject): Integer;
begin
  Result := IndexOf(item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TgxPersistentObjectList.Pack;
var
  i, j, n: Integer;
  p: PgxPointerObjectList;
  pk: PObject;
begin
  p := List;
  n := Count - 1;
  while (n >= 0) and (p^[n] = nil) do
    Dec(n);
  for i := 0 to n do
  begin
    if p^[i] = nil then
    begin
      pk := @(p^[i]);
      for j := i + 1 to n do
      begin
        if p^[j] <> nil then
        begin
          pk^ := p^[j];
          Inc(pk);
        end;
      end;
      SetCount((Cardinal(pk) - Cardinal(p)) div SizeOf(TObject));
      Exit;
    end;
  end;
  SetCount(n + 1);
end;

procedure TgxPersistentObjectList.SetCapacity(newCapacity: Integer);
begin
  if newCapacity <> FCapacity then
  begin
    if newCapacity < FCount then
      FCount := newCapacity;
    ReallocMem(FList, newCapacity * SizeOf(TObject));
    FCapacity := newCapacity;
  end;
end;

procedure TgxPersistentObjectList.RequiredCapacity(aCapacity: Integer);
begin
  if FCapacity < aCapacity then
    SetCapacity(aCapacity);
end;

procedure TgxPersistentObjectList.SetCount(newCount: Integer);
begin
  if newCount > FCapacity then
    SetCapacity(newCount);
  if newCount > FCount then
    FillChar(FList[FCount], (newCount - FCount) * SizeOf(TObject), 0);
  FCount := NewCount;
end;

procedure TgxPersistentObjectList.DeleteAndFree(index: Integer);
var
  obj: TObject;
begin
  obj := Get(index);
  Delete(index);
  obj.Free;
end;

procedure TgxPersistentObjectList.DeleteAndFreeItems(index: Integer; nbVals: Cardinal);
var
  i, n: Integer;
begin
{$IFOPT R+}
  Assert(Cardinal(index) < Cardinal(FCount));
{$ENDIF}
  n := index + Integer(nbVals);
  if n >= FCount then
    n := FCount - 1;
  for i := index to n do
    FList^[i].Free;
  DeleteItems(index, nbVals);
end;

function TgxPersistentObjectList.RemoveAndFree(item: TObject): Integer;
begin
  Result := IndexOf(item);
  if Result >= 0 then
  begin
    Delete(Result);
    item.Free;
  end;
end;

procedure TgxPersistentObjectList.DoClean;
var
  i: Integer;
begin
  // a 'for' loop could crash if freeing an item removes other items form the list
  i := FCount - 1;
  while i >= 0 do
  begin
    if i < FCount then
      FList^[i].Free;
    Dec(i);
  end;
end;

procedure TgxPersistentObjectList.Clean;
begin
  DoClean;
  Clear;
end;

procedure TgxPersistentObjectList.CleanFree;
begin
  if Self <> nil then
  begin
    Clean;
    Destroy;
  end;
end;

procedure TgxPersistentObjectList.WriteToFiler(writer: TgxVirtualWriter);
(*
   Object List Filer Format :

      Integer (Version)
      ListBegin
         ...[Object]...[Object]...
      ListEnd

   with [Object] being either (read vertically)

      Boolean (unused)        String (ClassName)        Integer (reference)
      Integer                 Object Data               Object Data
*)
var
  i, objId: integer;
  objTypes: TList;
  aType: TClass;
begin
  objTypes := TList.Create;
  try
    with writer do
    begin
      WriteInteger(0); // Archive Version 0 (uh... not exactly... but...)
      WriteListBegin;
      for i := 0 to FCount - 1 do
      begin
        if FList^[i] = nil then
        begin
          // store nil as... nil
          WriteBoolean(False);
          WriteInteger(0);
        end
        else if (FList^[i] is TgxPersistentObject) then
        begin
          // yeah, a TgxPersistentObject
          aType := FList^[i].ClassType;
          objId := objTypes.IndexOf(aType);
          if objId < 0 then
          begin
            // class is unknown
            objTypes.Add(aType);
            WriteString(aType.ClassName);
          end
          else
          begin
            // class already registered
            WriteInteger(objId);
          end;
          TgxPersistentObject(FList^[i]).WriteToFiler(writer);
        end
        else
        begin
          // Dunno that stuff here, store as is
          WriteBoolean(False);
          WriteInteger(Integer(FList^[i]));
        end;
      end;
      WriteListEnd;
    end;
  finally
    objTypes.Free;
  end;
end;

procedure TgxPersistentObjectList.ReadFromFilerWithEvent(reader: TgxVirtualReader; afterSenderObjectCreated: TNotifyEvent);
var
  obj: TgxPersistentObject;
  m: TGLPersistentObjectClass;
  version: integer;
  objTypes: TList;
begin
  objTypes := TList.Create;
  try
    Clean;
    with reader do
    begin
      version := ReadInteger;
      if version = 0 then
      begin
        ReadListBegin;
        while not EndOfList do
          case Cardinal(NextValue) of
            Cardinal(vaFalse), Cardinal(vaTrue):
              begin
                // stored 'as was' value
                ReadBoolean; // ignored
                Add(TObject(Cardinal(ReadInteger)));
              end;
            Cardinal(vaString), Cardinal(vaLString), Cardinal(vaWString),
              Cardinal(vaInt64) + 1 { vaUTF8String }:
              begin
                // Unknown class, to be registered
                m := TGLPersistentObjectClass(FindClass(ReadString));
                objTypes.Add(m);
                obj := m.Create;
                if Assigned(afterSenderObjectCreated) then
                  afterSenderObjectCreated(obj);
                obj.ReadFromFiler(reader);
                Add(obj);
              end;
            Cardinal(vaInt8), Cardinal(vaInt16), Cardinal(vaInt32):
              begin
                // known class, direct retrieve
                m := TGLPersistentObjectClass(objTypes[ReadInteger]);
                obj := m.Create;
                if Assigned(afterSenderObjectCreated) then
                  afterSenderObjectCreated(obj);
                obj.ReadFromFiler(reader);
                Add(obj);
              end;
          else
            raise Exception.Create(strBrokenObjectListArchive);
          end;
        ReadListEnd;
      end
      else
        RaiseFilerException(version);
    end;
  finally
    objTypes.Free;
  end;
end;

procedure TgxPersistentObjectList.ReadFromFiler(reader: TgxVirtualReader);
begin
  ReadFromFilerWithEvent(reader, AfterObjectCreatedByReader);
end;

procedure TgxPersistentObjectList.AfterObjectCreatedByReader(Sender: TObject);
begin
  // nothing
end;

procedure TgxPersistentObjectList.Push(item: TObject);
begin
  Add(item);
end;

function TgxPersistentObjectList.Pop: TObject;
begin
  if FCount > 0 then
  begin
    Result := FList^[FCount - 1];
    Dec(FCount);
  end
  else
    Result := nil;
end;

procedure TgxPersistentObjectList.PopAndFree;
begin
  Pop.Free;
end;

procedure POListQuickSort(SortList: PgxPointerObjectList; L, R: Integer;
  compareFunc: TObjectListSortCompare);
var
  I, J: Integer;
  P, T: TObject;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while compareFunc(SortList^[I], P) < 0 do
        Inc(I);
      while compareFunc(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      POListQuickSort(SortList, L, J, compareFunc);
    L := I;
  until I >= R;
end;

procedure TgxPersistentObjectList.Sort(compareFunc: TObjectListSortCompare);
begin
  if Count > 1 then
    POListQuickSort(FList, 0, Count - 1, compareFunc);
end;

// ------------------
// ------------------ TgxBinaryReader ------------------
// ------------------

procedure TgxBinaryReader.Read(var Buf; Count: Longint);
begin
  FStream.Read(Buf, Count);
end;

function TgxBinaryReader.ReadValue: TValueType;
var
  b: byte;
begin
  Read(b, 1);
  Result := TValueType(b);
end;

function TgxBinaryReader.NextValue: TValueType;
var
  pos: Int64;
begin
  pos := FStream.Position;
  Result := ReadValue;
  FStream.Position := pos;
end;

function TgxBinaryReader.ReadInteger: Integer;
var
  tempShort: ShortInt;
  tempSmallInt: SmallInt;
begin
  case ReadValue of
    vaInt8:
      begin
        Read(tempShort, 1);
        Result := tempShort;
      end;
    vaInt16:
      begin
        Read(tempSmallInt, 2);
        Result := tempSmallInt;
      end;
    vaInt32: Read(Result, 4);
  else
    Result := 0;
    ReadTypeError;
  end;
end;

function TgxBinaryReader.ReadBoolean: Boolean;
begin
  case ReadValue of
    vaTrue: Result := True;
    vaFalse: Result := False;
  else
    ReadTypeError;
    Result := False;
  end;
end;

function TgxBinaryReader.ReadString: string;
var
  n: Cardinal;
  vType: TValueType;
  tempString: AnsiString;
begin
  n := 0;
  vType := ReadValue;
  case Cardinal(vType) of
    Cardinal(vaWString),
      Cardinal(vaInt64) + 1:
      begin // vaUTF8String
        Result := ReadWideString(vType);
        Exit;
      end;
    Cardinal(vaString): Read(n, 1);
    Cardinal(vaLString): Read(n, 4);
  else
    ReadTypeError;
  end;
  SetLength(tempString, n);
  if n > 0 then
    Read(tempString[1], n);
  Result := string(tempString);
end;

function TgxBinaryReader.ReadWideString(vType: TValueType): WideString;
var
  n: Cardinal;
  utf8buf: AnsiString;
begin
  Read(n, 4);
  case Cardinal(vType) of
    Cardinal(vaWString):
      begin
        SetLength(Result, n);
        if n > 0 then
          Read(Result[1], n * 2);
      end;
    Cardinal(vaInt64) + 1:
      begin // vaUTF8String
        SetLength(utf8buf, n);
        if n > 0 then
        begin
          Read(utf8buf[1], n);
          Result := UTF8ToWideString(utf8buf);
        end;
      end;
  else
    ReadTypeError;
  end;
end;

function TgxBinaryReader.ReadFloat: Extended;
{$IFDEF WIN64}
var
   C  :TExtended80Rec; // Temporary variable to store 10 bytes floating point number in a Win64 application
{$ENDIF}
begin
  {$IFDEF WIN64}
  if ReadValue = vaExtended then
  begin
    Read(C, SizeOf(C));     // Load value into the temp variable
    Result := Extended(C); // Typecast into an Extended: in a win64 application is a Double
  end
  else
    ReadTypeError;
  {$ELSE}
  if ReadValue = vaExtended then
    Read(Result, SizeOf(Result))
  else
    ReadTypeError;
  {$ENDIF}
end;

procedure TgxBinaryReader.ReadListBegin;
begin
  if ReadValue <> vaList then
    ReadTypeError;
end;

procedure TgxBinaryReader.ReadListEnd;
begin
  if ReadValue <> vaNull then
    ReadTypeError;
end;

function TgxBinaryReader.EndOfList: Boolean;
begin
  Result := (NextValue = vaNull);
end;

// ------------------
// ------------------ TgxBinaryWriter ------------------
// ------------------

procedure TgxBinaryWriter.Write(const Buf; Count: Longint);
begin
  FStream.Write(Buf, Count);
end;

procedure TgxBinaryWriter.WriteInteger(anInteger: Integer);
type
  TIntStruct = packed record
    typ: byte;
    val: Integer;
  end;
var
  ins: TIntStruct;
begin
  ins.val := anInteger;
  if (anInteger >= Low(ShortInt)) and (anInteger <= High(ShortInt)) then
  begin
    ins.typ := byte(vaInt8);
    Write(ins, 2);
  end
  else if (anInteger >= Low(SmallInt)) and (anInteger <= High(SmallInt)) then
  begin
    ins.typ := byte(vaInt16);
    Write(ins, 3);
  end
  else
  begin
    ins.typ := byte(vaInt32);
    Write(ins, 5);
  end;
end;

procedure TgxBinaryWriter.WriteBoolean(aBoolean: Boolean);
const
  cBoolToType: array[False..True] of byte = (byte(vaFalse), byte(vaTrue));
begin
  Write(cBoolToType[aBoolean], 1);
end;

procedure TgxBinaryWriter.WriteAnsiString(const aString: AnsiString);
type
  TStringHeader = packed record
    typ: Byte;
    length: Integer;
  end;
var
  sh: TStringHeader;
begin
  sh.Length := Length(aString);
  if sh.Length <= 255 then
  begin
    sh.typ := byte(vaString);
    Write(sh, 2);
    if sh.Length > 0 then
      Write(aString[1], sh.Length);
  end
  else
  begin
    sh.typ := byte(vaLString);
    Write(sh, 5);
    Write(aString[1], sh.Length);
  end;
end;

procedure TgxBinaryWriter.WriteWideString(const aString: WideString);
type
  TStringHeader = packed record
    typ: Byte;
    length: Integer;
  end;
var
  sh: TStringHeader;
begin
  sh.Length := Length(aString);
  sh.typ := byte(vaWString);
  Write(sh, 5);
  if sh.Length > 0 then
    Write(aString[1], sh.length * SizeOf(WideChar));
end;

procedure TgxBinaryWriter.WriteString(const aString: string);
begin
{$IFDEF UNICODE}
  // TODO: should really check if the string can be simplified to: vaString / vaLString / vaUTF8String
  WriteWideString(aString);
{$ELSE}
  WriteAnsiString(aString);
{$ENDIF}
end;

procedure TgxBinaryWriter.WriteFloat(const aFloat: Extended);
type
  TExtendedStruct = packed record
    typ: Byte;
    {$IFDEF WIN64}
    val  :TExtended80Rec;  // Structure to handle a 10 bytes floating point value
    {$ELSE}
    val  :Extended;
    {$ENDIF}
  end;
var
  str: TExtendedStruct;
begin
  {$IFDEF WIN64}
  str.typ := byte(vaExtended);
  str.val := TExtended80Rec(aFloat);   // Typecast the float value (in a Win64 app the type is a Double) into the 10 bytes struct
  Write(str, SizeOf(str));
  {$ELSE}
  str.typ := byte(vaExtended);
  str.val := aFloat;
  Write(str, SizeOf(str));
  {$ENDIF}
end;

procedure TgxBinaryWriter.WriteListBegin;
const
  buf: byte = byte(vaList);
begin
  Write(buf, 1);
end;

procedure TgxBinaryWriter.WriteListEnd;
const
  buf: byte = byte(vaNull);
begin
  Write(buf, 1);
end;

// ------------------
// ------------------ TgxTextReader ------------------
// ------------------

procedure TgxTextReader.ReadLine(const requestedType: string = '');
var
  line: string;
  c: Byte;
  p: Integer;
begin
  // will need speed upgrade, someday...
  line := '';
  repeat
    Stream.Read(c, 1);
    if c >= 32 then
      line := line + chr(c);
  until c = 10;
  line := Trim(line);
  p := Pos(' ', line);
  if p > 0 then
  begin
    FValueType := Copy(line, 1, p - 1);
    FData := Trim(Copy(line, p + 1, MaxInt));
  end
  else
  begin
    FValueType := line;
    FData := '';
  end;
  if requestedType <> '' then
    if requestedType <> FValueType then
      raise EFilerException.Create('Invalid type, expected "'
        + requestedType + '", found "FValueType".');
end;

procedure TgxTextReader.Read(var Buf; Count: Longint);

  function HexCharToInt(const c: Char): Integer;
  begin
    if c <= '9' then
      Result := Integer(c) - Integer('0')
    else if c < 'a' then
      Result := Integer(c) - Integer('A') + 10
    else
      Result := Integer(c) - Integer('a') + 10;
  end;

var
  i, j: Integer;
begin
  ReadLine(cVTRaw);
  j := 1;
  for i := 0 to Count - 1 do
  begin
    PAnsiChar(@Buf)[i] := AnsiChar((HexCharToInt(FData[j]) shl 4)
      + HexCharToInt(FData[j + 1]));
    Inc(j, 2);
  end;
end;

function TgxTextReader.NextValue: TValueType;
var
  p: Int64;
begin
  p := Stream.Position;
  ReadLine;
  if FValueType = cVTInteger then
    Result := vaInt32
  else if FValueType = cVTFloat then
    Result := vaExtended
  else if FValueType = cVTString then
    Result := vaString
  else if FValueType = cVTBoolean then
    if FData = cTrue then
      Result := vaTrue
    else
      Result := vaFalse
  else if FValueType = cVTRaw then
    Result := vaBinary
  else if FValueType = cVTListBegin then
    Result := vaList
  else
    Result := vaNULL;
  Stream.Position := p;
end;

function TgxTextReader.ReadInteger: Integer;
begin
  ReadLine(cVTInteger);
  Result := StrToInt(FData);
end;

function TgxTextReader.ReadBoolean: Boolean;
begin
  ReadLine(cVTBoolean);
  Result := (FData = cTrue);
end;

function TgxTextReader.ReadString: string;
var
  i: Integer;
begin
  ReadLine(cVTString);
  Result := '';
  i := 1;
  while i < Length(FData) do
  begin
    if FData[i] = '#' then
    begin
      Result := Result + Char(StrToInt(Copy(FData, i + 1, 3)));
      Inc(i, 3);
    end
    else
      Result := Result + FData[i];
    Inc(i);
  end;
  Assert(FData[i] = '.', 'Invalid stored string.');
end;

function TgxTextReader.ReadFloat: Extended;
var
  oldDc: Char;
begin
  ReadLine(cVTInteger);
  oldDc := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  Result := StrToFloat(FData);
  FormatSettings.DecimalSeparator := oldDc;
end;

procedure TgxTextReader.ReadListBegin;
begin
  ReadLine(cVTListBegin);
end;

procedure TgxTextReader.ReadListEnd;
begin
  ReadLine(cVTListEnd);
end;

function TgxTextReader.EndOfList: Boolean;
var
  p: Int64;
begin
  p := Stream.Position;
  ReadLine;
  Result := (FValueType = cVTListEnd);
  Stream.Position := p;
end;

// ------------------
// ------------------ TgxTextWriter ------------------
// ------------------

constructor TgxTextWriter.Create(aStream: TStream);
begin
  inherited;
end;

destructor TgxTextWriter.Destroy;
begin
  inherited;
end;

procedure TgxTextWriter.WriteLine(const valueType, data: string);
var
  buf: AnsiString;
begin
  buf := StringOfChar(AnsiChar(#32), FIndentLevel);
  buf := buf + AnsiString(valueType + ' ' + data) + #13#10;
  Stream.Write(buf[1], Length(buf));
end;

procedure TgxTextWriter.Write(const Buf; Count: Longint);
const
  cNibbleToHex: PChar = '0123456789ABCDEF';
var
  i, j, b: Integer;
  data: string;
begin
  SetLength(data, Count * 2);
  j := 1;
  for i := 0 to Count - 1 do
  begin
    b := Integer(PAnsiChar(@buf)[i]);
    data[j] := cNibbleToHex[b shr 4];
    data[j + 1] := cNibbleToHex[b and 15];
    Inc(j, 2);
  end;
  WriteLine(cVTRaw, data);
end;

procedure TgxTextWriter.WriteInteger(anInteger: Integer);
begin
  WriteLine(cVTInteger, IntToStr(anInteger));
end;

procedure TgxTextWriter.WriteBoolean(aBoolean: Boolean);
begin
  if aBoolean then
    WriteLine(cVTBoolean, cTrue)
  else
    WriteLine(cVTBoolean, cFalse);
end;

procedure TgxTextWriter.WriteString(const aString: string);
var
  i: Integer;
  s: string;
begin
  s := '';
  for i := 1 to Length(aString) do
    if aString[i] >= #32 then
      s := s + aString[i]
    else
      s := s + Format('#%.3d', [Integer(aString[i])]);
  WriteLine(cVTString, s + '.');
end;

procedure TgxTextWriter.WriteFloat(const aFloat: Extended);
begin
  WriteLine(cVTInteger, FloatToStr(aFloat));
end;

procedure TgxTextWriter.WriteListBegin;
begin
  WriteLine(cVTListBegin, '');
  Inc(FIndentLevel, 3);
end;

procedure TgxTextWriter.WriteListEnd;
begin
  Dec(FIndentLevel, 3);
  WriteLine(cVTListEnd, '');
end;

// ------------------
// ------------------ TgxOwnedPersistent ------------------
// ------------------

constructor TgxOwnedPersistent.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
end;

function TgxOwnedPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// ------------------
// ------------------ TgxInterfacedPersistent ------------------
// ------------------

function TgxInterfacedPersistent._AddRef: Integer; stdcall;
begin
  Result := -1; //ignore
end;

function TgxInterfacedPersistent._Release: Integer; stdcall;
begin
  Result := -1; //ignore
end;

function TgxInterfacedPersistent.QueryInterface(const IID: TGUID;
  out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

// ------------------
// ------------------ TgxInterfacedCollectionItem ------------------
// ------------------


function TgxInterfacedCollectionItem._AddRef: Integer; stdcall;
begin
  Result := -1; //ignore
end;

function TgxInterfacedCollectionItem._Release: Integer; stdcall;
begin
  Result := -1; //ignore
end;

function TgxInterfacedCollectionItem.QueryInterface(const IID: TGUID;
    out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

   
  RegisterClass(TgxPersistentObjectList);
end.
