//
// The graphics rendering engine GLScene http://glscene.org
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
    Procedure SetCompressionLevel(aValue: TCompressionLevel); Virtual;
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
  TArchiveFileFormat = class
  public
    BaseArchiveClass: TGLBaseArchiveClass;
    Extension: string;
    Description: string;
    DescResID: integer;
  end;

  // The list of registered classes
  TGLArchiveFileFormatsList = class(TPersistentObjectList)
  public
    destructor Destroy; override;
    procedure Add(const Ext, Desc: string; DescID: integer;
      AClass: TGLBaseArchiveClass);
    function FindExt(Ext: string): TGLBaseArchiveClass;
    function FindFromFileName(const FileName: string): TGLBaseArchiveClass;
    procedure Remove(AClass: TGLBaseArchiveClass);
  end;

  // Using the collection item for simultaneous work with several archives
  TLibArchive = class(TCollectionItem)
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

  TLibArchives = class(TOwnedCollection)
  protected
    procedure SetItems(index: integer; const val: TLibArchive);
    function GetItems(index: integer): TLibArchive;
  public
    constructor Create(AOwner: TComponent);
    function Owner: TPersistent;
    function IndexOf(const Item: TLibArchive): integer;
    function Add: TLibArchive;
    function FindItemID(ID: integer): TLibArchive;
    property Items[index: integer]: TLibArchive read GetItems
      write SetItems; default;
    // searching archiver by name
    function GetArchiveByFileName(const AName: string): TLibArchive;
    function GetFileNameOfArchive(aValue: TLibArchive): string;
    // searching needed item
    function MakeUniqueName(const nameRoot: string): string;
    function GetLibArchiveByName(const AName: string): TLibArchive;
    function GetNameOfLibArchive(const Archive: TLibArchive): string;
  end;

  // ArchiveManager class
  TGLSArchiveManager = class(TComponent)
  private
    FArchives: TLibArchives;
    Procedure SetArchives(aValue: TLibArchives);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetArchiveByFileName(const AName: string): TLibArchive;
    function GetFileNameOfArchive(const aArchive: TLibArchive): string;
    function GetContent(aContentName: string): TStream;
    function ContentExists(aContentName: string): boolean;
    function OpenArchive(aFileName: string): TLibArchive; overload;
    function OpenArchive(aFileName, aAchiverType: string): TLibArchive;
      overload;
    procedure CloseArchive(aArchive: TLibArchive);
  published
    property Archives: TLibArchives read FArchives write SetArchives;
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
{ TLibArchive }

constructor TLibArchive.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FName := TLibArchives(ACollection).MakeUniqueName('LibArchive');
end;

destructor TLibArchive.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLibArchive.SetCompressionLevel(aValue: TCompressionLevel);
begin
  if vArchive = nil then
    Exit;
  vArchive.CompressionLevel := aValue;
end;

function TLibArchive.GetCompressionLevel: TCompressionLevel;
begin
  Result := clDefault;
  if vArchive = nil then
    Exit;
  Result := vArchive.CompressionLevel;
end;

procedure TLibArchive.CreateArchive(FileName: string;
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

procedure TLibArchive.LoadFromFile(aFileName: string);
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(aFileName));
  Delete(Ext, 1, 1);
  LoadFromFile(aFileName, Ext);
end;

procedure TLibArchive.LoadFromFile(aFileName, aAchiverType: string);
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

procedure TLibArchive.Clear;
begin
  if vArchive = nil then
    Exit;
  vArchive.Clear;
  vArchive.Free;
  ArcClass := nil;
  FFileName := '';
end;

function TLibArchive.ContentExists(aContentName: string): boolean;
begin
  Result := False;
  if vArchive = nil then
    Exit;
  Result := vArchive.ContentExists(aContentName)
end;

function TLibArchive.GetContent(aindex: integer): TStream;
begin
  Result := nil;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContent(aindex)
end;

function TLibArchive.GetContent(aContentName: string): TStream;
begin
  Result := nil;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContent(aContentName)
end;

function TLibArchive.GetContentSize(aindex: integer): integer;
begin
  Result := -1;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContentSize(aindex)
end;

function TLibArchive.GetContentSize(aContentName: string): integer;
begin
  Result := -1;
  if vArchive = nil then
    Exit;
  Result := vArchive.GetContentSize(aContentName)
end;

procedure TLibArchive.AddFromStream(aContentName, aPath: string; aF: TStream);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromStream(aContentName, aPath, aF)
end;

procedure TLibArchive.AddFromStream(aContentName: string; aF: TStream);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromStream(aContentName, '', aF)
end;

procedure TLibArchive.AddFromFile(aFileName, aPath: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromFile(aFileName, aPath)
end;

procedure TLibArchive.AddFromFile(aFileName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.AddFromFile(aFileName, '')
end;

procedure TLibArchive.RemoveContent(aindex: integer);
begin
  if vArchive = nil then
    Exit;
  vArchive.RemoveContent(aindex)
end;

procedure TLibArchive.RemoveContent(aContentName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.RemoveContent(aContentName)
end;

procedure TLibArchive.Extract(aindex: integer; aNewName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.Extract(aindex, aNewName)
end;

procedure TLibArchive.Extract(aContentName, aNewName: string);
begin
  if vArchive = nil then
    Exit;
  vArchive.Extract(aContentName, aNewName)
end;

function TLibArchive.GetContentList: TStrings;
begin
  Result := nil;
  if vArchive = nil then
    Exit;
  Result := vArchive.ContentList;
end;

procedure TLibArchive.SetName(const val: string);
begin
  if val <> FName then
  begin
    if not(csLoading in TComponent(TLibArchives(Collection).GetOwner)
      .ComponentState) then
    begin
      if TLibArchives(Collection).GetLibArchiveByName(val) <> Self then
        FName := TLibArchives(Collection).MakeUniqueName(val)
      else
        FName := val;
    end
    else
      FName := val;
  end;
end;

function TLibArchive.GetDisplayName: string;
begin
  Result := Name;
end;

procedure TLibArchives.SetItems(index: integer; const val: TLibArchive);
begin
  GetItems(Index).Assign(val);
end;

function TLibArchives.GetItems(index: integer): TLibArchive;
begin
  Result := TLibArchive(inherited GetItem(Index));
end;

constructor TLibArchives.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TLibArchive);
end;

function TLibArchives.Owner: TPersistent;
begin
  Result := GetOwner;
end;

function TLibArchives.IndexOf(const Item: TLibArchive): integer;
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

function TLibArchives.Add: TLibArchive;
begin
  Result := (inherited Add) as TLibArchive;
end;

function TLibArchives.FindItemID(ID: integer): TLibArchive;
begin
  Result := (inherited FindItemID(ID)) as TLibArchive;
end;

function TLibArchives.GetArchiveByFileName(const AName: string): TLibArchive;
var
  I: integer;
  Arc: TLibArchive;
begin
  for I := 0 to Count - 1 do
  begin
    Arc := TLibArchive(inherited Items[I]);
    if Arc.FileName = AName then
    begin
      Result := Arc;
      Exit;
    end;
  end;
  Result := nil;
end;

function TLibArchives.GetFileNameOfArchive(aValue: TLibArchive): string;
var
  ArcIndex: integer;
begin
  ArcIndex := IndexOf(aValue);
  if ArcIndex <> -1 then
    Result := GetItems(ArcIndex).FileName
  else
    Result := '';
end;

function TLibArchives.MakeUniqueName(const nameRoot: string): string;
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

function TLibArchives.GetLibArchiveByName(const AName: string): TLibArchive;
var
  I: integer;
  Arc: TLibArchive;
begin
  for I := 0 to Count - 1 do
  begin
    Arc := TLibArchive(inherited Items[I]);
    if (Arc.Name = AName) then
    begin
      Result := Arc;
      Exit;
    end;
  end;
  Result := nil;
end;

function TLibArchives.GetNameOfLibArchive(const Archive: TLibArchive): string;
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
  newRec: TArchiveFileFormat;
begin
  newRec := TArchiveFileFormat.Create;
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
    with TArchiveFileFormat(Items[I]) do
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
    if TArchiveFileFormat(Items[I]).BaseArchiveClass.InheritsFrom(AClass) then
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
  FArchives := TLibArchives.Create(Self);
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

procedure TGLSArchiveManager.SetArchives(aValue: TLibArchives);
begin
  FArchives.Assign(aValue);
end;

function TGLSArchiveManager.GetArchiveByFileName(const AName: string)
  : TLibArchive;
begin
  Result := FArchives.GetArchiveByFileName(AName);
end;

function TGLSArchiveManager.GetFileNameOfArchive(const aArchive
  : TLibArchive): string;
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

function TGLSArchiveManager.OpenArchive(aFileName: string): TLibArchive;
begin
  Result := FArchives.Add;
  Result.LoadFromFile(aFileName);
end;

function TGLSArchiveManager.OpenArchive(aFileName, aAchiverType: string)
  : TLibArchive;
begin
  Result := FArchives.Add;
  Result.LoadFromFile(aFileName, aAchiverType);
end;

procedure TGLSArchiveManager.CloseArchive(aArchive: TLibArchive);
begin
  FArchives.Delete(FArchives.IndexOf(aArchive));
end;

// -----------------------------------------------------------
initialization

// -----------------------------------------------------------

RegisterClasses([TGLSArchiveManager, TLibArchives]);

finalization

FreeAndNil(vArchiveFileFormats);

end.
