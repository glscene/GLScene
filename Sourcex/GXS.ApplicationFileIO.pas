//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.ApplicationFileIO;
(*
  Components and functions that abstract file I/O access for an application.
  Allows re-routing file reads to reads from a single archive file f.i.
*)
interface

{$I GXS.Scene.inc}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,

  GXS.BaseClasses,
  GXS.Strings;

const
  RC_DDS_Type = RT_RCDATA;
  RC_JPG_Type = RT_RCDATA;
  RC_XML_Type = RT_RCDATA;
  RC_String_Type = RT_RCDATA;

type

  TgxApplicationResource = (aresNone, aresSplash, aresTexture, aresMaterial,
    aresSampler, aresFont, aresMesh);

  TgxAFIOCreateFileStream = function(const fileName: String; mode: Word): TStream;
  TgxAFIOFileStreamExists = function(const fileName: String): Boolean;
  TgxAFIOFileStreamEvent = procedure(const fileName: String; mode: Word;
    var Stream: TStream) of object;
  TgxAFIOFileStreamExistsEvent = function(const fileName: String): Boolean of object;

  (* Allows specifying a custom behaviour for TFileStream.Create.
    The component should be considered a helper only, you can directly specify
    a function via the vAFIOCreateFileStream variable.
    If multiple ApplicationFileIO components exist in the application,
    the last one created will be the active one. *)
  TgxApplicationFileIO = class(TComponent)
  private
    FOnFileStream: TgxAFIOFileStreamEvent;
    FOnFileStreamExists: TgxAFIOFileStreamExistsEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Event that allows you to specify a stream for the file.
      Destruction of the stream is at the discretion of the code that
      invoked TFileStream.Create. Return nil to let the default mechanism
      take place (ie. attempt a regular file system access). *)
    property OnFileStream: TgxAFIOFileStreamEvent read FOnFileStream write FOnFileStream;
    // Event that allows you to specify if a stream for the file exists.
    property OnFileStreamExists: TgxAFIOFileStreamExistsEvent
      read FOnFileStreamExists write FOnFileStreamExists;
  end;

  TgxDataFileCapability = (dfcRead, dfcWrite);
  TDataFileCapabilities = set of TgxDataFileCapability;

  (* Abstract base class for data file formats interfaces.
    This class declares base file-related behaviours, ie. ability to load/save
    from a file or a stream.
    It is highly recommended to overload ONLY the stream based methods, as the
    file-based one just call these, and stream-based behaviours allow for more
    enhancement (such as other I/O abilities, compression, cacheing, etc.)
    to this class, without the need to rewrite subclasses. *)
  TgxDataFile = class(TgxUpdateAbleObject)
  private
    FOwner : TPersistent;
    FResourceName: string;
  protected
    function GetOwner : TPersistent;
    procedure SetResourceName(const AName: string);
  public
	constructor Create(AOwner: TPersistent); virtual;
    destructor Destroy; override;
    // Describes what the TgxDataFile is capable of. Default value is [dfcRead].
    class function Capabilities: TDataFileCapabilities; virtual;
    (* Duplicates Self and returns a copy.
      Subclasses should override this method to duplicate their data. *)
    function CreateCopy(AOwner: TPersistent): TgxDataFile; virtual;
    procedure LoadFromFile(const fileName: string); virtual;
    procedure SaveToFile(const fileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    (* Optionnal resource name.
      When using LoadFromFile/SaveToFile, the filename is placed in it,
      when using the Stream variants, the caller may place the resource
      name in it for parser use. *)
    property ResourceName: string read FResourceName write SetResourceName;
  end;

  TgxDataFileClass = class of TgxDataFile;
  TgxResourceStream = TResourceStream;

  // Returns true if an ApplicationFileIO has been defined
function ApplicationFileIODefined: Boolean;
// Queries is a file stream corresponding to the fileName exists.
function FileStreamExists(const fileName: string): Boolean;
function CreateResourceStream(const ResName: string; ResType: PChar): TgxResourceStream;
function StrToResType(const AStrRes: string): TgxApplicationResource;

var
  vGXAFIOCreateFileStream: TgxAFIOCreateFileStream = nil;
  vGXAFIOFileStreamExists: TgxAFIOFileStreamExists = nil;

// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------

var
  vAFIO: TgxApplicationFileIO = nil;

function ApplicationFileIODefined: Boolean;
begin
  Result := (Assigned(vGxAFIOCreateFileStream) and Assigned(vGxAFIOFileStreamExists)) or Assigned(vAFIO);
end;

function FileStreamExists(const fileName: string): Boolean;
begin
  if Assigned(vGxAFIOFileStreamExists) then
    Result := vGxAFIOFileStreamExists(fileName)
  else
  begin
    if Assigned(vAFIO) and Assigned(vAFIO.FOnFileStreamExists) then
      Result := vAFIO.FOnFileStreamExists(fileName)
    else
      Result := FileExists(fileName);
  end;
end;

function CreateResourceStream(const ResName: string; ResType: PChar)
  : TgxResourceStream;
var
  InfoBlock: HRSRC;
begin
  Result := nil;
  InfoBlock := FindResource(HInstance, PChar(ResName), ResType);
  if InfoBlock <> 0 then
    Result := TResourceStream.Create(HInstance, ResName, ResType)
  else
    raise Exception.Create('Can''t create stream of application resource "%ResName"');
end;

function StrToResType(const AStrRes: string): TgxApplicationResource;
begin
  if AStrRes = '[SAMPLERS]' then
    Result := aresSampler
  else if AStrRes = '[TEXTURES]' then
    Result := aresTexture
  else if AStrRes = '[MATERIALS]' then
    Result := aresMaterial
  else if AStrRes = '[STATIC MESHES]' then
    Result := aresMesh
  else if AStrRes = '[SPLASH]' then
    Result := aresSplash
  else if AStrRes = '[FONTS]' then
    Result := aresFont
  else
    Result := aresNone;
end;

// ------------------
// ------------------ TgxApplicationFileIO ------------------
// ------------------

constructor TgxApplicationFileIO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  vAFIO := Self;
end;

destructor TgxApplicationFileIO.Destroy;
begin
  vAFIO := nil;
  inherited Destroy;
end;

// ------------------
// ------------------ TgxDataFile ------------------
// ------------------

constructor TgxDataFile.Create(AOwner: TPersistent);
begin
   inherited Create(AOwner);
   FOwner := AOwner;
end;

destructor TgxDataFile.Destroy;
begin
   inherited;
end;

class function TgxDataFile.Capabilities : TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

function TgxDataFile.CreateCopy(AOwner: TPersistent): TgxDataFile;
begin
  if Self <> nil then
    Result := TgxDataFileClass(Self.ClassType).Create(AOwner)
  else
    Result := nil;
end;

procedure TgxDataFile.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  ResourceName := ExtractFileName(fileName);
  fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyNone);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TgxDataFile.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  ResourceName := ExtractFileName(fileName);
  fs := TFileStream.Create(fileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

function TgxDataFile.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

procedure TgxDataFile.LoadFromStream(Stream: TStream);
begin
  Assert(False, 'Import for ' + ClassName + ' to ' + Stream.ClassName +
    ' not available.');
end;

procedure TgxDataFile.SaveToStream(Stream: TStream);
begin
  Assert(False, 'Export for ' + ClassName + ' to ' + Stream.ClassName +
    ' not available.');
end;

procedure TgxDataFile.SetResourceName(const AName: string);
begin
  FResourceName := AName;
end;

end.
