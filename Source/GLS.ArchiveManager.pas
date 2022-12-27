//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.ArchiveManager;

(* Archive manager -  the class to work with archives *)

{$I GLScene.inc}

interface

uses
  System.Classes,
  System.SysUtils,

  GLS.ApplicationFileIO,
  GLS.PersistentClasses,
  GLS.Strings;

type

  TCompressionLevel = (clNone, clFastest, clDefault, clMax, clLevel1, clLevel2,
    clLevel3, clLevel4, clLevel5, clLevel6, clLevel7, clLevel8, clLevel9);

  // BaseArchive class
  TGLBaseArchive = class(TGLDataFile)
  protected
    FFileName: string;
    FContentList: TStrings;
    FCompressionLevel: TCompressionLevel;
    procedure SetCompressionLevel(aValue: TCompressionLevel); Virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    property ContentList: TStrings read FContentList;
    property CompressionLevel: TCompressionLevel read FCompressionLevel
      write SetCompressionLevel default clNone;
    procedure Clear; virtual; abstract;
    function ContentExists(ContentName: string): boolean; virtual; abstract;
    function GetContent(Stream: TStream; index: integer): TStream; overload;
      virtual; abstract;
    function GetContent(ContentName: string): TStream; overload;
      virtual; abstract;
    function GetContent(index: integer): TStream; overload; virtual; abstract;
    function GetContentSize(index: integer): integer; overload;
      virtual; abstract;
    function GetContentSize(ContentName: string): integer; overload;
      virtual; abstract;
    procedure AddFromStream(ContentName, Path: string; FS: TStream);
      virtual; abstract;
    procedure AddFromFile(FileName, Path: string); virtual; abstract;
    procedure RemoveContent(index: integer); overload; virtual; abstract;
    procedure RemoveContent(ContentName: string); overload; virtual; abstract;
    procedure Extract(index: integer; NewName: string); overload;
      virtual; abstract;
    procedure Extract(ContentName, NewName: string); overload; virtual;
      abstract;
  end;

  TGLBaseArchiveClass = class of TGLBaseArchive;

  // Archive registration classes to use proper srchiver for extensions like:
  // GLFilePak, GLFileZLib etc.

  (* The type to record a registered class *)
  TGLArchiveFileFormat = class
  public
    BaseArchiveClass: TGLBaseArchiveClass;
    Extension: string;
    Description: string;
    DescResID: integer;
  end;

  // The list of registered classes
  TGLArchiveFileFormatsList = class(TGLPersistentObjectList)
  public
    destructor Destroy; override;
    procedure Add(const Ext, Desc: string; DescID: integer;
      AClass: TGLBaseArchiveClass);
    function FindExt(Ext: string): TGLBaseArchiveClass;
    function FindFromFileName(const FileName: string): TGLBaseArchiveClass;
    procedure Remove(AClass: TGLBaseArchiveClass);
  end;

  // Using the collection item for simultaneous work with several archives
  TGLLibArchive = class(TCollectionItem)
  private
    vArchive: TGLBaseArchive;
    ArcClass: TGLBaseArchiveClass;
    FFileName: string;
    FName: string;
    procedure SetCompressionLevel(aValue: TCompressionLevel);
    function GetCompressionLevel: TCompressionLevel;
    function GetContentList: TStrings;
    procedure SetName(const val: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property CompressionLevel: TCompressionLevel read GetCompressionLevel
      write SetCompressionLevel default clDefault;
    procedure CreateArchive(FileName: string;
      OverwriteExistingFile: boolean = False);
    property ContentList: TStrings read GetContentList;
    procedure LoadFromFile(aFileName: string); overload;
    procedure LoadFromFile(aFileName, aAchiverType: string); overload;
    procedure Clear;
    function ContentExists(aContentName: string): boolean;
    property FileName: string read FFileName;
    function GetContent(aindex: integer): TStream; overload;
    function GetContent(aContentName: string): TStream; overload;
    function GetContentSize(aindex: integer): integer; overload;
    function GetContentSize(aContentName: string): integer; overload;
    procedure AddFromStream(aContentName, aPath: string; aF: TStream); overload;
    procedure AddFromStream(aContentName: string; aF: TStream); overload;
    procedure AddFromFile(aFileName, aPath: string); overload;
    procedure AddFromFile(aFileName: string); overload;
    procedure RemoveContent(aindex: integer); overload;
    procedure RemoveContent(aContentName: string); overload;
    procedure Extract(aindex: integer; aNewName: string); overload;
    procedure Extract(aContentName, aNewName: string); overload;
  published
    property Name: string read FName write SetName;
  end;

  TGLLibArchives = class(TOwnedCollection)
  protected
    procedure SetItems(index: integer; const val: TGLLibArchive);
    function GetItems(index: integer): TGLLibArchive;
  public
    constructor Create(AOwner: TComponent);
    function Owner: TPersistent;
    function IndexOf(const Item: TGLLibArchive): integer;
    function Add: TGLLibArchive;
    function FindItemID(ID: integer): TGLLibArchive;
    property Items[index: integer]: TGLLibArchive read GetItems
      write SetItems; default;
    // searching archiver by name
    function GetArchiveByFileName(const AName: string): TGLLibArchive;
    function GetFileNameOfArchive(aValue: TGLLibArchive): string;
    // searching needed item
    function MakeUniqueName(const nameRoot: string): string;
    function GetLibArchiveByName(const AName: string): TGLLibArchive;
    function GetNameOfLibArchive(const Archive: TGLLibArchive): string;
  end;

  // ArchiveManager class
  TGLSArchiveManager = class(TComponent)
  private
    FArchives: TGLLibArchives;
    Procedure SetArchives(aValue: TGLLibArchives);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetArchiveByFileName(const AName: string): TGLLibArchive;
    function GetFileNameOfArchive(const aArchive: TGLLibArchive): string;
    function GetContent(aContentName: string): TStream;
    function ContentExists(aContentName: string): boolean;
    function OpenArchive(aFileName: string): TGLLibArchive; overload;
    function OpenArchive(aFileName, aAchiverType: string): TGLLibArchive;
      overload;
    procedure CloseArchive(aArchive: TGLLibArchive);
  published
    property Archives: TGLLibArchives read FArchives write SetArchives;
  end;

  EInvalidArchiveFile = class(Exception);

// getting a class of accessed archiver
function GetArchiveFileFormats: TGLArchiveFileFormatsList;
procedure RegisterArchiveFormat(const AExtension, ADescription: string;
  AClass: TGLBaseArchiveClass);
procedure UnregisterArchiveFormat(AClass: TGLBaseArchiveClass);

// Caution!!! Work for one archive manager only
function GetArchiveManager: TGLSArchiveManager;

// GLS.ApplicationFileIO
// These functions are used to automate loading
// User enters LoadFromFile and through these functions gets the result

function ArcCreateFileStream(const FileName: string; mode: word): TStream;
function ArcFileStreamExists(const FileName: string): boolean;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  vArchiveFileFormats: TGLArchiveFileFormatsList;
  vArchiveManager: TGLSArchiveManager;

function GetArchiveFileFormats: TGLArchiveFileFormatsList;
begin
  if not Assigned(vArchiveFileFormats) then
    vArchiveFileFormats := TGLArchiveFileFormatsList.Create;
  Result := vArchiveFileFormats;
end;

procedure RegisterArchiveFormat(const AExtension, ADescription: string;
  AClass: TGLBaseArchiveClass);
begin
  RegisterClass(AClass);
  GetArchiveFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

procedure UnregisterArchiveFormat(AClass: TGLBaseArchiveClass);
begin
  if Assigned(vArchiveFileFormats) then
    vArchiveFileFormats.Remove(AClass);
end;

function GetArchiveManager: TGLSArchiveManager;
begin
  Result := vArchiveManager;
end;

function ArcCreateFileStream(const FileName: string; mode: word): TStream;
begin
  If GetArchiveManager <> nil then
    with GetArchiveManager do
      if ContentExists(FileName) then
      begin
        Result := GetContent(FileName);
        Exit;
      end;
  if FileExists(FileName) then
  begin
    Result := TFileStream.Create(FileName, mode);
    Exit;
    // why create a file stream when a file is not found ???
    { end
      else begin
      Result := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
      Exit; }
  end;

  Result := nil;
end;

function ArcFileStreamExists(const FileName: string): boolean;
begin
  If GetArchiveManager <> nil then
    with GetArchiveManager do
      if ContentExists(FileName) then
      begin
        Result := True;
        Exit;
      end;
  Result := FileExists(FileName);
end;

// ******************************************************************************

// TGLLibArchive

constructor TGLLibArchive.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FName := TGLLibArchives(ACollection).MakeUniqueName('LibArchive');
end;

destructor TGLLibArchive.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TGLLibArchive.SetCompressionLevel(aValue: TCompressionLevel);
begin
  if vArchive = nil then
    Exit;
  vArchive.CompressionLevel := aValue;
end;

function TGLLibArchive.GetCompressionLevel: TCompressionLevel;
begin
  Result := clDefault;
  if vArchive = nil then
    Exit;
  Result := vArchive.CompressionLevel;
end;

procedure TGLLibArchive.CreateArchive(FileName: string;
  OverwriteExistingFile: boolean = False);
var
  fFile: TFileStream;
begin
  if OverwriteExistingFile or not FileExists(FileName) then
  begin
    fFile := TFileStream.Create(FileName, fmCreate);
    fFile.Free;
  end;
end;

procedure TGLLibArchive.LoadFromFile(aFileName: string);
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(aFileName));
  Delete(Ext, 1, 1);
  LoadFromFile(aFileName, Ext);
end;

procedure TGLLibArchive.LoadFromFile(aFileName, aAchiverType: string);
begin
  if not FileExists(aFileName) then
    Exit;
  ArcClass := GetArchiveFileFormats.FindExt(aAchiverType);
  If ArcClass = nil then
  begin
    raise Exception.Create(ClassName +
    ': Unable to find module archiver to expand ' + aAchiverType);
    Exit;
  end;
  vArchive := ArcClass.Create(nil);
  vArchive.LoadFromFile(aFileName);
  FFileName := aFileName;
end;

procedure TGLLibArchive.Clear;
begin
  if vArchive = nil then
    Exit;
  vArchive.Clear;
  vArchive.Free;
  ArcClass := nil;
  FFileName := '';
end;

function TGLLibArchive.ContentExists(aContentName: string): boolean;
begin
  Result := False;
  if vArchive = nil then
    Exit;
  Result := vArchive.ContentExists(aContentName)
end;

function TGLLibArchive.GetContent(aindex: integer): TStream;
begin
  Result := nil;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContent(aindex)
end;

function TGLLibArchive.GetContent(aContentName: string): TStream;
begin
  Result := nil;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContent(aContentName)
end;

function TGLLibArchive.GetContentSize(aindex: integer): integer;
begin
  Result := -1;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContentSize(aindex)
end;

function TGLLibArchive.GetContentSize(aContentName: string): integer;
begin
  Result := -1;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContentSize(aContentName)
end;

procedure TGLLibArchive.AddFromStream(aContentName, aPath: string; aF: TStream);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromStream(aContentName, aPath, aF)
end;

procedure TGLLibArchive.AddFromStream(aContentName: string; aF: TStream);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromStream(aContentName, '', aF)
end;

procedure TGLLibArchive.AddFromFile(aFileName, aPath: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromFile(aFileName, aPath)
end;

procedure TGLLibArchive.AddFromFile(aFileName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromFile(aFileName, '')
end;

procedure TGLLibArchive.RemoveContent(aindex: integer);
begin
  if vArchive = nil then
    Exit;
  vArchive.RemoveContent(aindex)
end;

procedure TGLLibArchive.RemoveContent(aContentName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.RemoveContent(aContentName)
end;

procedure TGLLibArchive.Extract(aindex: integer; aNewName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.Extract(aindex, aNewName)
end;

procedure TGLLibArchive.Extract(aContentName, aNewName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.Extract(aContentName, aNewName)
end;

function TGLLibArchive.GetContentList: TStrings;
begin
  Result := nil;
  if vArchive = nil then
    Exit;
  Result := vArchive.ContentList;
end;

procedure TGLLibArchive.SetName(const val: string);
begin
  if val <> FName then
  begin
    if not(csLoading in TComponent(TGLLibArchives(Collection).GetOwner)
      .ComponentState) then
    begin
      if TGLLibArchives(Collection).GetLibArchiveByName(val) <> Self then
        FName := TGLLibArchives(Collection).MakeUniqueName(val)
      else
        FName := val;
    end
    else
      FName := val;
  end;
end;

function TGLLibArchive.GetDisplayName: string;
begin
  Result := Name;
end;

procedure TGLLibArchives.SetItems(index: integer; const val: TGLLibArchive);
begin
  GetItems(Index).Assign(val);
end;

function TGLLibArchives.GetItems(index: integer): TGLLibArchive;
begin
  Result := TGLLibArchive(inherited GetItem(Index));
end;

constructor TGLLibArchives.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TGLLibArchive);
end;

function TGLLibArchives.Owner: TPersistent;
begin
  Result := GetOwner;
end;

function TGLLibArchives.IndexOf(const Item: TGLLibArchive): integer;
var
  I: integer;
begin
  Result := -1;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      if GetItems(I) = Item then
      begin
        Result := I;
        Exit;
      end;
end;

function TGLLibArchives.Add: TGLLibArchive;
begin
  Result := (inherited Add) as TGLLibArchive;
end;

function TGLLibArchives.FindItemID(ID: integer): TGLLibArchive;
begin
  Result := (inherited FindItemID(ID)) as TGLLibArchive;
end;

function TGLLibArchives.GetArchiveByFileName(const AName: string): TGLLibArchive;
var
  I: integer;
  Arc: TGLLibArchive;
begin
  for I := 0 to Count - 1 do
  begin
    Arc := TGLLibArchive(inherited Items[I]);
    if Arc.FileName = AName then
    begin
      Result := Arc;
      Exit;
    end;
  end;
  Result := nil;
end;

function TGLLibArchives.GetFileNameOfArchive(aValue: TGLLibArchive): string;
var
  ArcIndex: integer;
begin
  ArcIndex := IndexOf(aValue);
  if ArcIndex <> -1 then
    Result := GetItems(ArcIndex).FileName
  else
    Result := '';
end;

function TGLLibArchives.MakeUniqueName(const nameRoot: string): string;
var
  I: integer;
begin
  Result := nameRoot;
  I := 1;
  while GetLibArchiveByName(Result) <> nil do
  begin
    Result := nameRoot + IntToStr(I);
    Inc(I);
  end;
end;

function TGLLibArchives.GetLibArchiveByName(const AName: string): TGLLibArchive;
var
  I: integer;
  Arc: TGLLibArchive;
begin
  for I := 0 to Count - 1 do
  begin
    Arc := TGLLibArchive(inherited Items[I]);
    if (Arc.Name = AName) then
    begin
      Result := Arc;
      Exit;
    end;
  end;
  Result := nil;
end;

function TGLLibArchives.GetNameOfLibArchive(const Archive: TGLLibArchive): string;
var
  MatIndex: integer;
begin
  MatIndex := IndexOf(Archive);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

// ******************************************************************************
{ TGLArchiveFileFormatsList }

destructor TGLArchiveFileFormatsList.Destroy;
begin
  Clean;
  inherited Destroy;
end;

procedure TGLArchiveFileFormatsList.Add(const Ext, Desc: string;
  DescID: integer; AClass: TGLBaseArchiveClass);
var
  newRec: TGLArchiveFileFormat;
begin
  newRec := TGLArchiveFileFormat.Create;
  with newRec do
  begin
    Extension := AnsiLowerCase(Ext);
    BaseArchiveClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(newRec);
end;

function TGLArchiveFileFormatsList.FindExt(Ext: string): TGLBaseArchiveClass;
var
  I: integer;
begin
  Ext := AnsiLowerCase(Ext);
  for I := Count - 1 downto 0 do
    with TGLArchiveFileFormat(Items[I]) do
    begin
      if Extension = Ext then
      begin
        Result := BaseArchiveClass;
        Exit;
      end;
    end;
  Result := nil;
end;

function TGLArchiveFileFormatsList.FindFromFileName(const FileName: string)
  : TGLBaseArchiveClass;
var
  Ext: string;
begin
  Ext := ExtractFileExt(FileName);
  System.Delete(Ext, 1, 1);
  Result := FindExt(Ext);
  if not Assigned(Result) then
    raise EInvalidArchiveFile.CreateFmt(strUnknownExtension,
      [Ext, 'GLFile' + UpperCase(Ext)]);
end;

procedure TGLArchiveFileFormatsList.Remove(AClass: TGLBaseArchiveClass);
var
  I: integer;
begin
  for I := Count - 1 downto 0 do
  begin
    if TGLArchiveFileFormat(Items[I]).BaseArchiveClass.InheritsFrom(AClass) then
      DeleteAndFree(I);
  end;
end;


// ******************************************************************************
{ TGLBaseArchive }

procedure TGLBaseArchive.SetCompressionLevel(aValue: TCompressionLevel);
begin
  if FCompressionLevel <> aValue then
    FCompressionLevel := aValue;
end;

constructor TGLBaseArchive.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FContentList := TStringList.Create;
  FCompressionLevel := clDefault;
end;

destructor TGLBaseArchive.Destroy;
begin
  FContentList.Free;
  inherited Destroy;
end;

// ******************************************************************************
{ TGLSArchiveManager }

constructor TGLSArchiveManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArchives := TGLLibArchives.Create(Self);
  vArchiveManager := Self;
  vAFIOCreateFileStream := ArcCreateFileStream;
  vAFIOFileStreamExists := ArcFileStreamExists;
end;

destructor TGLSArchiveManager.Destroy;
begin
  vArchiveManager := nil;
  vAFIOCreateFileStream := nil;
  vAFIOFileStreamExists := nil;
  FArchives.Free;
  inherited Destroy;
end;

procedure TGLSArchiveManager.SetArchives(aValue: TGLLibArchives);
begin
  FArchives.Assign(aValue);
end;

function TGLSArchiveManager.GetArchiveByFileName(const AName: string)
  : TGLLibArchive;
begin
  Result := FArchives.GetArchiveByFileName(AName);
end;

function TGLSArchiveManager.GetFileNameOfArchive(const aArchive
  : TGLLibArchive): string;
begin
  Result := FArchives.GetFileNameOfArchive(aArchive)
end;

function TGLSArchiveManager.GetContent(aContentName: string): TStream;
var
  I: integer;
begin
  Result := nil;
  With FArchives do
    for I := 0 to Count - 1 do
      if Items[I].ContentExists(aContentName) then
      begin
        Result := Items[I].GetContent(aContentName);
        Exit;
      end;
end;

function TGLSArchiveManager.ContentExists(aContentName: string): boolean;
var
  I: integer;
begin
  Result := False;
  With FArchives do
    for I := 0 to Count - 1 do
      if Items[I].ContentExists(aContentName) then
      begin
        Result := Items[I].ContentExists(aContentName);
        Exit;
      end;
end;

function TGLSArchiveManager.OpenArchive(aFileName: string): TGLLibArchive;
begin
  Result := FArchives.Add;
  Result.LoadFromFile(aFileName);
end;

function TGLSArchiveManager.OpenArchive(aFileName, aAchiverType: string)
  : TGLLibArchive;
begin
  Result := FArchives.Add;
  Result.LoadFromFile(aFileName, aAchiverType);
end;

procedure TGLSArchiveManager.CloseArchive(aArchive: TGLLibArchive);
begin
  FArchives.Delete(FArchives.IndexOf(aArchive));
end;

// -----------------------------------------------------------
initialization

// -----------------------------------------------------------

RegisterClasses([TGLSArchiveManager, TGLLibArchives]);

finalization

FreeAndNil(vArchiveFileFormats);

end.
