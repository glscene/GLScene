//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.ArchiveManager;

{$I GXS.Scene.inc}

interface

uses
  System.Classes,
  System.SysUtils,

  GXS.Strings,
  GXS.PersistentClasses,
  GXS.ApplicationFileIO;

Type

  TCompressionLevel = (
    clNone,
    clFastest,
    clDefault,
    clMax,
    clLevel1,
    clLevel2,
    clLevel3,
    clLevel4,
    clLevel5,
    clLevel6,
    clLevel7,
    clLevel8,
    clLevel9
  );

  //Base classes for archivers
  TgxBaseArchive = class(TgxDataFile)
    protected
      FFileName: string;
      FContentList: TStrings;
      FCompressionLevel: TCompressionLevel;
      Procedure SetCompressionLevel(aValue: TCompressionLevel); Virtual;
    public
      constructor Create(AOwner: TPersistent); override;
      destructor Destroy; override;
      property ContentList: TStrings read FContentList;
      property CompressionLevel: TCompressionLevel
               read FCompressionLevel
               write SetCompressionLevel default clNone;
      procedure Clear; virtual;abstract;
      function ContentExists(ContentName: string): boolean;virtual;abstract;
      function GetContent(Stream: TStream; index: integer): TStream; overload;virtual;abstract;
      function GetContent(ContentName: string): TStream; overload;virtual;abstract;
      function GetContent(index: integer): TStream; overload;virtual;abstract;
      function GetContentSize(index: integer): integer; overload;virtual;abstract;
      function GetContentSize(ContentName: string): integer; overload;virtual;abstract;
      procedure AddFromStream(ContentName, Path: string; FS: TStream);virtual;abstract;
      procedure AddFromFile(FileName, Path: string);virtual;abstract;
      procedure RemoveContent(index: integer); overload;virtual;abstract;
      procedure RemoveContent(ContentName: string); overload;virtual;abstract;
      procedure Extract(index: integer; NewName: string); overload; virtual;abstract;
      procedure Extract(ContentName, NewName: string); overload; virtual;abstract;
  end;

  TgxBaseArchiveClass = class of TgxBaseArchive;

  // Registered archives for using extentions, e.g.: GLFilePak,GLFileZLib
  TArchiveFileFormat = class
  public
    BaseArchiveClass: TgxBaseArchiveClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  // List of registered classes
  TgxArchiveFileFormatsList = class(TgxPersistentObjectList)
  public
    destructor Destroy; override;
    procedure Add(const Ext, Desc: string; DescID: Integer; AClass:
      TgxBaseArchiveClass);
    function FindExt(ext: string): TgxBaseArchiveClass;
    function FindFromFileName(const fileName: string): TgxBaseArchiveClass;
    procedure Remove(AClass: TgxBaseArchiveClass);
  end;

  (* To work with several archives simultaniously collections implemented
     Item to work with one archive *)
  TLibArchive = class(TCollectionItem)
  private
      vArchive: TgxBaseArchive;
      ArcClass: TgxBaseArchiveClass;
      FFileName:  string;
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
    property CompressionLevel: TCompressionLevel
               read GetCompressionLevel
               write SetCompressionLevel default clNone;
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
    procedure SetItems(index: Integer; const val: TLibArchive);
    function GetItems(index: Integer): TLibArchive;
  public
    constructor Create(AOwner: TComponent);
    function Owner: TPersistent;
    function IndexOf(const Item: TLibArchive): integer;
    function Add: TLibArchive;
    function FindItemID(ID: integer): TLibArchive;
    property Items[index: integer]: TLibArchive read GetItems
      write SetItems; default;
    // Looking for an archive by the name of an open archive
    function GetArchiveByFileName(const AName: string): TLibArchive;
    function GetFileNameOfArchive(aValue: TLibArchive): string;
    // Looking for an necessary item
    function MakeUniqueName(const nameRoot: string): string;
    function GetLibArchiveByName(const AName: string): TLibArchive;
    function GetNameOfLibArchive(const Archive: TLibArchive): string;
  end;

  TgxSArchiveManager = class(TComponent)
    Private
      FArchives: TLibArchives;
      Procedure SetArchives(aValue: TLibArchives);
    Public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function GetArchiveByFileName(const aName: string): TLibArchive;
      function GetFileNameOfArchive(const aArchive: TLibArchive): string;
      function GetContent(aContentName: string): TStream;
      function ContentExists(aContentName: string): boolean;
      function OpenArchive(aFileName: string): TLibArchive; overload;
      function OpenArchive(aFileName, aAchiverType: string): TLibArchive; overload;
      procedure CloseArchive(aArchive: TLibArchive);
    Published
      property Archives: TLibArchives read FArchives write SetArchives;
  end;

  EInvalidArchiveFile = class(Exception);

  function GetArchiveFileFormats: TgxArchiveFileFormatsList;
  procedure RegisterArchiveFormat(const AExtension, ADescription: string;
    AClass: TgxBaseArchiveClass);
  procedure UnregisterArchiveFormat(AClass: TgxBaseArchiveClass);
  // Note! It's working only with one manager
  function GetArchiveManager: TgxSArchiveManager;
  // Users getting results after LoadFromFile with this functions
  function ArcCreateFileStream(const fileName: string; mode: word): TStream;
  function ArcFileStreamExists(const fileName: string): boolean;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  vArchiveFileFormats: TgxArchiveFileFormatsList;
  vArchiveManager: TgxSArchiveManager;

function GetArchiveFileFormats: TgxArchiveFileFormatsList;
begin
  if not Assigned(vArchiveFileFormats) then
    vArchiveFileFormats := TgxArchiveFileFormatsList.Create;
  Result := vArchiveFileFormats;
end;

procedure RegisterArchiveFormat(const AExtension, ADescription: string;
  AClass: TgxBaseArchiveClass);
begin
  RegisterClass(AClass);
  GetArchiveFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

procedure UnregisterArchiveFormat(AClass: TgxBaseArchiveClass);
begin
  if Assigned(vArchiveFileFormats) then
    vArchiveFileFormats.Remove(AClass);
end;

function GetArchiveManager: TgxSArchiveManager;
begin
   Result := vArchiveManager;
end;

function ArcCreateFileStream(const fileName: string; mode: word): TStream;
begin
  If GetArchiveManager <> nil then
     with GetArchiveManager do
       if ContentExists(fileName) then
       begin
          Result := GetContent(fileName);
          Exit;
       end;
   if FileExists(fileName) then begin
      Result := TFileStream.Create(FileName, mode);
      Exit;
      // Why to create filestream having no file while searching ???
  (* end
   else begin
      Result := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
      Exit;  *)
   end;

   Result:=nil;
end;

function ArcFileStreamExists(const fileName: string): boolean;
begin
  If GetArchiveManager <> nil then
    with GetArchiveManager do
      if ContentExists(fileName) then
      begin
         Result:=True;
         Exit;
      end;
   Result := FileExists(fileName);
end;

//--------------------------------------
// TLibArchive
//--------------------------------------

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
  if vArchive = nil then Exit;
  vArchive.CompressionLevel := aValue;
end;

function TLibArchive.GetCompressionLevel: TCompressionLevel;
begin
  Result := clNone;
  if vArchive = nil then Exit;
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
  ext: string;
begin
  ext := LowerCase(ExtractFileExt(aFileName));
  Delete(ext,1,1);
  LoadFromFile(aFileName, ext);
end;

procedure TLibArchive.LoadFromFile(aFileName, aAchiverType: string);
begin
  if not FileExists(aFileName) then
    Exit;
  ArcClass := GetArchiveFileFormats.FindExt(aAchiverType);
  If ArcClass=nil then
  begin
       raise Exception.Create(ClassName+': Unable to find module archiver to expand '+ aAchiverType);
       exit;
  end;
  vArchive := ArcClass.Create(nil);
  vArchive .LoadFromFile(aFileName);
  FFileName := aFileName;
end;

procedure TLibArchive.Clear;
begin
  if vArchive=nil then Exit;
  vArchive.Clear;
  vArchive.Free;
  ArcClass :=nil;
  FFileName := '';
end;

function TLibArchive.ContentExists(aContentName: string): boolean;
begin
  Result := false;
  if vArchive=nil then Exit;
  Result := vArchive.ContentExists(aContentName)
end;

function TLibArchive.GetContent(aindex: integer): TStream;
begin
  Result := nil;
  if vArchive=nil then Exit;
  Result := vArchive.GetContent(aindex)
end;

function TLibArchive.GetContent(aContentName: string): TStream;
begin
  Result := nil;
  if vArchive=nil then Exit;
  Result := vArchive.GetContent(aContentName)
end;

function TLibArchive.GetContentSize(aindex: integer): integer;
begin
  Result := -1;
  if vArchive=nil then Exit;
  Result := vArchive.GetContentSize(aindex)
end;

function TLibArchive.GetContentSize(aContentName: string): integer;
begin
  Result := -1;
  if vArchive=nil then Exit;
  Result := vArchive.GetContentSize(aContentName)
end;

procedure TLibArchive.AddFromStream(aContentName, aPath: string; aF: TStream);
begin
  if vArchive=nil then Exit;
  vArchive.AddFromStream(aContentName, aPath, aF)
end;

procedure TLibArchive.AddFromStream(aContentName: string; aF: TStream);
begin
  if vArchive=nil then Exit;
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
    if not (csLoading in
      TComponent(TLibArchives(Collection).GetOwner).ComponentState) then
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

//--------------------------------------
// TLibArchives
//--------------------------------------

procedure TLibArchives.SetItems(index: Integer; const val: TLibArchive);
begin
  GetItems(Index).Assign(Val);
end;

function TLibArchives.GetItems(index: Integer): TLibArchive;
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

function TLibArchives.IndexOf(const Item: TLibArchive): Integer;
var
  I: Integer;
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

function TLibArchives.FindItemID(ID: Integer): TLibArchive;
begin
  Result := (inherited FindItemID(ID)) as TLibArchive;
end;

function TLibArchives.GetArchiveByFileName(const AName: string): TLibArchive;
var
  i: Integer;
  Arc: TLibArchive;
begin
  for i := 0 to Count - 1 do
  begin
    Arc := TLibArchive(inherited Items[i]);
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
  ArcIndex: Integer;
begin
  ArcIndex := IndexOf(aValue);
  if ArcIndex <> -1 then
    Result := GetItems(ArcIndex).FileName
  else
    Result := '';
end;

function TLibArchives.MakeUniqueName(const nameRoot: string): string;
var
  i: Integer;
begin
  Result := nameRoot;
  i := 1;
  while GetLibArchiveByName(Result) <> nil do
  begin
    Result := nameRoot + IntToStr(i);
    Inc(i);
  end;
end;

function TLibArchives.GetLibArchiveByName(const AName: string): TLibArchive;
var
  i: Integer;
  Arc: TLibArchive;
begin
  for i := 0 to Count - 1 do
  begin
    Arc := TLibArchive(inherited Items[i]);
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
  MatIndex: Integer;
begin
  MatIndex := IndexOf(Archive);
  if MatIndex <> -1 then
    Result := GetItems(MatIndex).Name
  else
    Result := '';
end;

//--------------------------------------
// TgxArchiveFileFormatsList
//--------------------------------------

destructor TgxArchiveFileFormatsList.Destroy;
begin
  Clean;
  inherited Destroy;
end;

procedure TgxArchiveFileFormatsList.Add(const Ext, Desc: string; DescID: Integer;
  AClass: TgxBaseArchiveClass);
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

function TgxArchiveFileFormatsList.FindExt(ext: string): TgxBaseArchiveClass;
var
  i: Integer;
begin
  ext := AnsiLowerCase(ext);
  for i := Count - 1 downto 0 do
    with TArchiveFileFormat(Items[I]) do
    begin
      if Extension = ext then
      begin
        Result := BaseArchiveClass;
        Exit;
      end;
    end;
  Result := nil;
end;

function TgxArchiveFileFormatsList.FindFromFileName(const fileName: string
  ): TgxBaseArchiveClass;
var
  ext: string;
begin
  ext := ExtractFileExt(Filename);
  System.Delete(ext, 1, 1);
  Result := FindExt(ext);
  if not Assigned(Result) then
    raise EInvalidArchiveFile.CreateFmt(strUnknownExtension,
      [ext, 'GLFile' + UpperCase(ext)]);
end;

procedure TgxArchiveFileFormatsList.Remove(AClass: TgxBaseArchiveClass);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if TArchiveFileFormat(Items[i]).BaseArchiveClass.InheritsFrom(AClass) then
      DeleteAndFree(i);
  end;
end;

//--------------------------------------
// TgxBaseArchive
//--------------------------------------

procedure TgxBaseArchive.SetCompressionLevel(aValue: TCompressionLevel);
begin
  if FCompressionLevel <> aValue then
     FCompressionLevel := aValue;
end;

constructor TgxBaseArchive.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FContentList := TStringList.Create;
  FCompressionLevel := clNone;
end;

destructor TgxBaseArchive.Destroy;
begin
  FContentList.Free;
  inherited Destroy;
end;

//--------------------------------------
// TgxSArchiveManager
//--------------------------------------

constructor TgxSArchiveManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FArchives := TLibArchives.Create(self);
  vArchiveManager := Self;
  vGXAFIOCreateFileStream := ArcCreateFileStream;
  vGXAFIOFileStreamExists := ArcFileStreamExists;
end;

destructor TgxSArchiveManager.Destroy;
begin
  vArchiveManager := nil;
  FArchives.Free;
  inherited Destroy;
end;

procedure TgxSArchiveManager.SetArchives(aValue: TLibArchives);
begin
  FArchives.Assign(aValue);
end;

function TgxSArchiveManager.GetArchiveByFileName(const aName: string): TLibArchive;
begin
  Result := FArchives.GetArchiveByFileName(AName);
end;

function TgxSArchiveManager.GetFileNameOfArchive(const aArchive: TLibArchive): string;
begin
  Result := FArchives.GetFileNameOfArchive(aArchive)
end;

function TgxSArchiveManager.GetContent(aContentName: string): TStream;
var
  i: integer;
begin
  Result := nil;
  With FArchives do
    for i:=0 to Count-1 do
      if Items[i].ContentExists(aContentName) then
      begin
        Result := Items[i].GetContent(aContentName);
        Exit;
      end;
end;

function TgxSArchiveManager.ContentExists(aContentName: string): boolean;
var
  i: integer;
begin
    Result := false;
    With FArchives do
      for i:=0 to Count-1 do
        if Items[i].ContentExists(aContentName) then
        begin
          Result := Items[i].ContentExists(aContentName);
          Exit;
        end;
end;

function TgxSArchiveManager.OpenArchive(aFileName: string): TLibArchive;
begin
  Result := FArchives.Add;
  Result.LoadFromFile(aFileName);
end;

function TgxSArchiveManager.OpenArchive(aFileName, aAchiverType: string
  ): TLibArchive;
begin
  Result := FArchives.Add;
  Result.LoadFromFile(aFileName, aAchiverType);
end;

procedure TgxSArchiveManager.CloseArchive(aArchive: TLibArchive);
begin
  FArchives.Delete(FArchives.IndexOf(aArchive));
end;

//--------------------------------------------------------
initialization
//--------------------------------------------------------

  RegisterClasses([TgxSArchiveManager, TLibArchives]);

finalization

  FreeAndNil(vArchiveFileFormats);

end.
