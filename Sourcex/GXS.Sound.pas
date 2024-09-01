//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Sound;

(* Base classes and interface for GLScene Sound System *)

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,

  GXS.XCollection,
  GXS.VectorTypes,
  GXS.SoundFileObjects,
  GXS.Scene,
  GXS.VectorGeometry,
  GXS.Cadencer,
  GXS.BaseClasses,
  GXS.Utils;

{$I GXS.Scene.inc}

type

  { Stores a single PCM coded sound sample. }
  TgxSoundSample = class(TCollectionItem)
  private
    FName: string;
    FData: TgxSoundFile;
    FTag: Integer;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    function GetDisplayName: string; override;
    procedure SetData(const val: TgxSoundFile);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromFile(const fileName: string);
    procedure PlayOnWaveOut;
    function Sampling: TgxSoundSampling;
    function LengthInBytes: Integer;
    function LengthInSamples: Integer;
    function LengthInSec: Single;
    // This Tag is reserved for sound manager use only
    property ManagerTag: Integer read FTag write FTag;
  published
    property Name: string read FName write FName;
    property Data: TgxSoundFile read FData write SetData stored False;
  end;

  TgxSoundSamples = class(TCollection)
  protected
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TgxSoundSample);
    function GetItems(index: Integer): TgxSoundSample;
  public
    constructor Create(AOwner: TComponent);
    function Add: TgxSoundSample;
    function FindItemID(ID: Integer): TgxSoundSample;
    property Items[index: Integer]: TgxSoundSample read GetItems write SetItems;
      default;
    function GetByName(const aName: string): TgxSoundSample;
    function AddFile(const fileName: string; const sampleName: string = ''):
      TgxSoundSample;
  end;

  TgxSoundLibrary = class(TComponent)
  private
    FSamples: TgxSoundSamples;
  protected
    procedure SetSamples(const val: TgxSoundSamples);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Samples: TgxSoundSamples read FSamples write SetSamples;
  end;

  TgxSoundSourceChange = (sscTransformation, sscSample, sscStatus);
  TgxSoundSourceChanges = set of TgxSoundSourceChange;

  TgxBSoundEmitter = class;

  { Base class for origin of sound playback. }
  TgxBaseSoundSource = class(TCollectionItem)
  private
    FBehaviourToNotify: TgxBSoundEmitter;
      // private only, NOT persistent, not assigned
    FPriority: Integer;
    FOrigin: TgxBaseSceneObject; // NOT persistent
    FVolume: Single;
    FMinDistance, FMaxDistance: Single;
    FInsideConeAngle, FOutsideConeAngle: Single;
    FConeOutsideVolume: Single;
    FSoundLibraryName: string; // used for persistence
    FSoundLibrary: TgxSoundLibrary; // persistence via name
    FSoundName: string;
    FMute: Boolean;
    FPause: Boolean;
    FChanges: TgxSoundSourceChanges; // NOT persistent, not assigned
    FNbLoops: Integer;
    FTag: Cardinal; // NOT persistent, not assigned
    FFrequency: Integer;
  protected
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);
    function GetDisplayName: string; override;
    procedure SetPriority(const val: Integer);
    procedure SetOrigin(const val: TgxBaseSceneObject);
    procedure SetVolume(const val: Single);
    procedure SetMinDistance(const val: Single);
    procedure SetMaxDistance(const val: Single);
    procedure SetInsideConeAngle(const val: Single);
    procedure SetOutsideConeAngle(const val: Single);
    procedure SetConeOutsideVolume(const val: Single);
    function GetSoundLibrary: TgxSoundLibrary;
    procedure SetSoundLibrary(const val: TgxSoundLibrary);
    procedure SetSoundName(const val: string);
    procedure SetMute(const val: Boolean);
    procedure SetPause(const val: Boolean);
    procedure SetNbLoops(const val: Integer);
    procedure SetFrequency(const val: Integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Changes: TgxSoundSourceChanges read FChanges;
    function Sample: TgxSoundSample;
    // This Tag is reserved for sound manager use only
    property ManagerTag: Cardinal read FTag write FTag;
    { Origin object for the sound sources.
       Absolute object position/orientation are taken into account, the
       object's TgxBInertia is considered if any.
       If origin is nil, the source is assumed to be static at the origin.
        Note :  since TCollectionItem do not support the "Notification"
       scheme, it is up to the Origin object to take care of updating this
       property prior to release/destruction. }
    property Origin: TgxBaseSceneObject read FOrigin write SetOrigin;
  published
    property SoundLibrary: TgxSoundLibrary read GetSoundLibrary write
      SetSoundLibrary;
    property SoundName: string read FSoundName write SetSoundName;
    { Volume of the source, [0.0; 1.0] range }
    property Volume: Single read FVolume write SetVolume;
    { Nb of playing loops. }
    property NbLoops: Integer read FNbLoops write SetNbLoops default 1;
    property Mute: Boolean read FMute write SetMute default False;
    property Pause: Boolean read FPause write SetPause default False;
    { Sound source priority, the higher the better.
       When maximum number of sound sources is reached, only the sources
       with the highest priority will continue to play, however, even
       non-playing sources should be tracked by the manager, thus allowing
       an "unlimited" amount of sources from the application point of view. }
    property Priority: Integer read FPriority write SetPriority default 0;
    { Min distance before spatial attenuation occurs.
       1.0 by default }
    property MinDistance: Single read FMinDistance write SetMinDistance;
    { Max distance, if source is further away, it will not be heard.
       100.0 by default }
    property MaxDistance: Single read FMaxDistance write SetMaxDistance;
    { Inside cone angle, [0°; 360°].
       Sound volume is maximal within this cone.
       See DirectX SDK for details. }
    property InsideConeAngle: Single read FInsideConeAngle write
      SetInsideConeAngle;
    { Outside cone angle, [0°; 360°].
       Between inside and outside cone, sound volume decreases between max
       and cone outside volume.
       See DirectX SDK for details. }
    property OutsideConeAngle: Single read FOutsideConeAngle write
      SetOutsideConeAngle;
    { Cone outside volume, [0.0; 1.0] range.
       See DirectX SDK for details. }
    property ConeOutsideVolume: Single read FConeOutsideVolume write
      SetConeOutsideVolume;
    { Sample custom playback frequency.
       Values null or negative are interpreted as 'default frequency'. }
    property Frequency: Integer read FFrequency write SetFrequency default -1;
  end;

    { Origin of sound playback.
       Just publishes the 'Origin' property.
       Note that the "orientation" is the the source's Direction, ie. the "Z"
       vector. }
  TgxSoundSource = class(TgxBaseSoundSource)
  public
    destructor Destroy; override;
  published
    property Origin;
  end;

  TgxSoundSources = class(TCollection)
  protected
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TgxSoundSource);
    function GetItems(index: Integer): TgxSoundSource;
    function Add: TgxSoundSource;
    function FindItemID(ID: Integer): TgxSoundSource;
  public
    constructor Create(AOwner: TComponent);
    property Items[index: Integer]: TgxSoundSource read GetItems write SetItems;
      default;
  end;

  { EAX standard sound environments. }
  TgxSoundEnvironment = (seDefault, sePaddedCell, seRoom, seBathroom,
    seLivingRoom, seStoneroom, seAuditorium,
    seConcertHall, seCave, seArena, seHangar,
    seCarpetedHallway, seHallway, seStoneCorridor,
    seAlley, seForest, seCity, seMountains, seQuarry,
    sePlain, seParkingLot, seSewerPipe, seUnderWater,
    seDrugged, seDizzy, sePsychotic);

    { Base class for sound manager components.
       The sound manager component is the interface to a low-level audio API
       (like DirectSound), there can only be one active manager at any time
       (this class takes care of this).
       Subclass should override the DoActivate and DoDeActivate protected methods
       to "initialize/unitialize" their sound layer, actual data releases should
       occur in destructor however. }
  TgxSoundManager = class(TgxCadenceAbleComponent)
  private
    FActive: Boolean;
    FMute: Boolean;
    FPause: Boolean;
    FMasterVolume: Single;
    FListener: TgxBaseSceneObject;
    FLastListenerPosition: TVector4f;
    FSources: TgxSoundSources;
    FMaxChannels: Integer;
    FOutputFrequency: Integer;
    FUpdateFrequency: Single;
    FDistanceFactor: Single;
    FRollOffFactor: Single;
    FDopplerFactor: Single;
    FSoundEnvironment: TgxSoundEnvironment;
    FLastUpdateTime, FLastDeltaTime: Single;
      // last time UpdateSources was fired, not persistent
    FCadencer: TgxCadencer;
    procedure SetActive(const val: Boolean);
    procedure SetMute(const val: Boolean);
    procedure SetPause(const val: Boolean);
    procedure WriteDoppler(writer: TWriter);
    procedure ReadDoppler(reader: TReader);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetSources(const val: TgxSoundSources);
    procedure SetMasterVolume(const val: Single);
    procedure SetListener(const val: TgxBaseSceneObject);
    procedure SetMaxChannels(const val: Integer);
    procedure SetOutputFrequency(const val: Integer);
    procedure SetUpdateFrequency(const val: Single);
    function StoreUpdateFrequency: Boolean;
    procedure SetCadencer(const val: TgxCadencer);
    procedure SetDistanceFactor(const val: Single);
    function StoreDistanceFactor: Boolean;
    procedure SetRollOffFactor(const val: Single);
    function StoreRollOffFactor: Boolean;
    procedure SetDopplerFactor(const val: Single);
    procedure SetSoundEnvironment(const val: TgxSoundEnvironment);
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ListenerCoordinates(var position, velocity, direction, up:
      TVector4f);
    function DoActivate: Boolean; virtual;
    // Invoked AFTER all sources have been stopped
    procedure DoDeActivate; virtual;
    { Effect mute of all sounds.
       Default implementation call MuteSource for all non-muted sources
       with "True" as parameter. }
    function DoMute: Boolean; virtual;
    { Effect un-mute of all sounds.
       Default implementation call MuteSource for all non-muted sources
       with "False" as parameter. }
    procedure DoUnMute; virtual;
    { Effect pause of all sounds.
       Default implementation call PauseSource for all non-paused sources
       with "True" as parameter. }
    function DoPause: Boolean; virtual;
    { Effect un-pause of all sounds.
       Default implementation call PauseSource for all non-paused sources
       with "True" as parameter. }
    procedure DoUnPause; virtual;
    procedure NotifyMasterVolumeChange; virtual;
    procedure Notify3DFactorsChanged; virtual;
    procedure NotifyEnvironmentChanged; virtual;
    // Called when a source will be freed
    procedure KillSource(aSource: TgxBaseSoundSource); virtual;
    { Request to update source's data in low-level sound API.
       Default implementation just clears the "Changes" flags. }
    procedure UpdateSource(aSource: TgxBaseSoundSource); virtual;
    procedure MuteSource(aSource: TgxBaseSoundSource; muted: Boolean); virtual;
    procedure PauseSource(aSource: TgxBaseSoundSource; paused: Boolean);
      virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Manual request to update all sources to reflect changes.
       Default implementation invokes UpdateSource for all known sources. }
    procedure UpdateSources; virtual;
    { Stop and free all sources. }
    procedure StopAllSources;
    { Progress notification for time synchronization.
       This method will call UpdateSources depending on the last time
       it was performed and the value of the UpdateFrequency property. }
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    { Sound manager API reported CPU Usage.
       Returns -1 when unsupported. }
    function CPUUsagePercent: Single; virtual;
    { True if EAX is supported. }
    function EAXSupported: Boolean; virtual;
  published
      { Activation/deactivation of the low-level sound API }
    property Active: Boolean read FActive write SetActive default False;
    { Maximum number of sound output channels.
       While some drivers will just ignore this value, others cannot
       dynamically adjust the maximum number of channels (you need to
       de-activate and re-activate the manager for this property to be
       taken into account). }
    property MaxChannels: Integer read FMaxChannels write SetMaxChannels default
      8;
    { Sound output mixing frequency.
       Commonly used values ar 11025, 22050 and 44100.
       Note that most driver cannot dynamically adjust the output frequency
       (you need to de-ativate and re-activate the manager for this property
       to be taken into account). }
    property OutputFrequency: Integer read FOutputFrequency write
      SetOutputFrequency default 44100;
    { Request to mute all sounds.
       All sound requests should be handled as if sound is unmuted though,
       however drivers should try to take a CPU advantage of mute over
       MasterVolume=0 }
    property Mute: Boolean read FMute write SetMute default False;
    { Request to pause all sound, sound output should be muted too.
       When unpausing, all sound should resume at the point they were paused. }
    property Pause: Boolean read FPause write SetPause default False;
    { Master Volume adjustement in the [0.0; 1.0] range.
       Driver should take care of properly clamping the master volume. }
    property MasterVolume: Single read FMasterVolume write SetMasterVolume;
    { Scene object that materializes the listener.
       The sceneobject's AbsolutePosition and orientation are used to define
       the listener coordinates, velocity is automatically calculated
       (if you're using DoProgress or connected the manager to a cadencer).
       If this property is nil, the listener is assumed to be static at
       the NullPoint coordinate, facing Z axis, with up being Y (ie. the
       default orientation). }
    property Listener: TgxBaseSceneObject read FListener write SetListener;
    { Currently active and playing sound sources. }
    property Sources: TgxSoundSources read FSources write SetSources;
    { Update frequency for time-based control (DoProgress).
       Default value is 10 Hz (frequency is clamped in the 1Hz-60Hz range). }
    property UpdateFrequency: Single read FUpdateFrequency write
      SetUpdateFrequency stored StoreUpdateFrequency;
    { Cadencer for time-based control.  }
    property Cadencer: TgxCadencer read FCadencer write SetCadencer;
    { Engine relative distance factor, compared to 1.0 meters.
       Equates to 'how many units per meter' your engine has. }
    property DistanceFactor: Single read FDistanceFactor write SetDistanceFactor
      stored StoreDistanceFactor;
    { Sets the global attenuation rolloff factor.
       Normally volume for a sample will scale at 1 / distance.
       This gives a logarithmic attenuation of volume as the source gets
       further away (or closer).
       Setting this value makes the sound drop off faster or slower.
       The higher the value, the faster volume will fall off. }
    property RollOffFactor: Single read FRollOffFactor write SetRollOffFactor
      stored StoreRollOffFactor;
    { Engine relative Doppler factor, compared to 1.0 meters.
       Equates to 'how many units per meter' your engine has. }
    property DopplerFactor: Single read FDopplerFactor write SetDopplerFactor
      stored False;
    { Sound environment (requires EAX compatible soundboard). }
    property Environment: TgxSoundEnvironment read FSoundEnvironment write
      SetSoundEnvironment default seDefault;
  end;

  { A sound emitter behaviour, plug it on any object to make it noisy.
    This behaviour is just an interface to a TgxSoundSource, for editing  convenience. }
  TgxBSoundEmitter = class(TgxBehaviour)
  private
    FPlaying: Boolean; // used at design-time ONLY
    FSource: TgxBaseSoundSource;
    FPlayingSource: TgxSoundSource;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure SetSource(const val: TgxBaseSoundSource);
    procedure SetPlaying(const val: Boolean);
    function GetPlaying: Boolean;
    procedure NotifySourceDestruction(aSource: TgxSoundSource);
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: Boolean; override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    property PlayingSource: TgxSoundSource read FPlayingSource;
  published
    property Source: TgxBaseSoundSource read FSource write SetSource;
    property Playing: Boolean read GetPlaying write SetPlaying default False;
  end;

function ActiveSoundManager: TgxSoundManager;
function GetSoundLibraryByName(const aName: string): TgxSoundLibrary;
function GetOrCreateSoundEmitter(behaviours: TgxBehaviours): TgxBSoundEmitter; overload;
function GetOrCreateSoundEmitter(obj: TgxBaseSceneObject): TgxBSoundEmitter; overload;

var
  // If this variable is true, errors in GLSM may be displayed to the user
  vVerboseGLSMErrors: Boolean = True;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  vActiveSoundManager: TgxSoundManager;
  vSoundLibraries: TList;

function ActiveSoundManager: TgxSoundManager;
begin
  Result := vActiveSoundManager;
end;

function GetSoundLibraryByName(const aName: string): TgxSoundLibrary;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(vSoundLibraries) then
    for i := 0 to vSoundLibraries.Count - 1 do
      if TgxSoundLibrary(vSoundLibraries[i]).Name = aName then
      begin
        Result := TgxSoundLibrary(vSoundLibraries[i]);
        Break;
      end;
end;

function GetOrCreateSoundEmitter(behaviours: TgxBehaviours): TgxBSoundEmitter;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TgxBSoundEmitter);
  if i >= 0 then
    Result := TgxBSoundEmitter(behaviours[i])
  else
    Result := TgxBSoundEmitter.Create(behaviours);
end;

function GetOrCreateSoundEmitter(obj: TgxBaseSceneObject): TgxBSoundEmitter;
begin
  Result := GetOrCreateSoundEmitter(obj.Behaviours);
end;

// ------------------
// ------------------ TgxSoundSample ------------------
// ------------------

constructor TgxSoundSample.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TgxSoundSample.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

procedure TgxSoundSample.Assign(Source: TPersistent);
begin
  if Source is TgxSoundSample then
  begin
    FName := TgxSoundSample(Source).Name;
    FData.Free;
    FData := TgxSoundFile(TgxSoundSample(Source).Data.CreateCopy(Self));
  end
  else
    inherited Assign(Source); // Assign error
end;

procedure TgxSoundSample.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('BinData', ReadData, WriteData, Assigned(FData));
end;

procedure TgxSoundSample.ReadData(Stream: TStream);
var
  n: Integer;
  clName: AnsiString;
begin
  with Stream do
  begin
    Read(n, SizeOf(Integer));
    SetLength(clName, n);
    if n > 0 then
      Read(clName[1], n);
    FData := TgxSoundFileClass(FindClass(string(clName))).Create(Self);
    FData.LoadFromStream(Stream);
  end;
end;

procedure TgxSoundSample.WriteData(Stream: TStream);
var
  n: Integer;
  buf: AnsiString;
begin
  with Stream do
  begin
    n := Length(FData.ClassName);
    Write(n, SizeOf(Integer));
    buf := AnsiString(FData.ClassName);
    if n > 0 then
      Write(buf[1], n);
    FData.SaveToStream(Stream);
  end;
end;

function TgxSoundSample.GetDisplayName: string;
var
  s: string;
begin
  if Assigned(FData) then
  begin
    if Data.Sampling.NbChannels > 1 then
      s := 's'
    else
      s := '';
    Result := Format('%s (%d Hz, %d bits, %d channel%s, %.2f sec)',
      [Name, Data.Sampling.Frequency,
      Data.Sampling.BitsPerSample,
        Data.Sampling.NbChannels, s, LengthInSec])
  end
  else
    Result := Format('%s (empty)', [Name]);
end;

procedure TgxSoundSample.LoadFromFile(const fileName: string);
var
  sfc: TgxSoundFileClass;
begin
  FData.Free;
  sfc := GetGLSoundFileFormats.FindExt(ExtractFileExt(fileName));
  if Assigned(sfc) then
  begin
    FData := sfc.Create(Self);
    FData.LoadFromFile(fileName);
  end
  else
    FData := nil;
  Assert(Data <> nil, 'Could not load ' + fileName +
    ', make sure you include the unit required to load this format in your uses clause.');
  Name := ExtractFileName(fileName);
end;

procedure TgxSoundSample.PlayOnWaveOut;
begin
  if Assigned(FData) then
    FData.PlayOnWaveOut;
end;

// TgxSoundSample
//

function TgxSoundSample.Sampling: TgxSoundSampling;
begin
  if Assigned(FData) then
    Result := FData.Sampling
  else
    Result := nil;
end;

function TgxSoundSample.LengthInBytes: Integer;
begin
  if Assigned(FData) then
    Result := FData.LengthInBytes
  else
    Result := 0;
end;

function TgxSoundSample.LengthInSamples: Integer;
begin
  if Assigned(FData) then
    Result := FData.LengthInSamples
  else
    Result := 0;
end;

function TgxSoundSample.LengthInSec: Single;
begin
  if Assigned(FData) then
    Result := FData.LengthInSec
  else
    Result := 0;
end;

procedure TgxSoundSample.SetData(const val: TgxSoundFile);
begin
  FData.Free;
  if Assigned(val) then
    FData := TgxSoundFile(val.CreateCopy(Self))
  else
    FData := nil;
end;

// ------------------
// ------------------ TgxSoundSamples ------------------
// ------------------

constructor TgxSoundSamples.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TgxSoundSample);
end;

function TgxSoundSamples.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TgxSoundSamples.SetItems(index: Integer; const val: TgxSoundSample);
begin
  inherited Items[index] := val;
end;

function TgxSoundSamples.GetItems(index: Integer): TgxSoundSample;
begin
  Result := TgxSoundSample(inherited Items[index]);
end;

function TgxSoundSamples.Add: TgxSoundSample;
begin
  Result := (inherited Add) as TgxSoundSample;
end;

function TgxSoundSamples.FindItemID(ID: Integer): TgxSoundSample;
begin
  Result := (inherited FindItemID(ID)) as TgxSoundSample;
end;

function TgxSoundSamples.GetByName(const aName: string): TgxSoundSample;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Name, aName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TgxSoundSamples.AddFile(const fileName: string; const sampleName: string
  = ''): TgxSoundSample;
begin
  Result := Add;
  Result.LoadFromFile(fileName);
  if sampleName <> '' then
    Result.Name := sampleName;
end;

// ------------------
// ------------------ TgxSoundLibrary ------------------
// ------------------

constructor TgxSoundLibrary.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSamples := TgxSoundSamples.Create(Self);
  vSoundLibraries.Add(Self);
end;

destructor TgxSoundLibrary.Destroy;
begin
  vSoundLibraries.Remove(Self);
  FSamples.Free;
  inherited Destroy;
end;

procedure TgxSoundLibrary.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
end;

procedure TgxSoundLibrary.SetSamples(const val: TgxSoundSamples);
begin
  FSamples.Assign(val);
end;

// ------------------
// ------------------ TgxBaseSoundSource ------------------
// ------------------

constructor TgxBaseSoundSource.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FChanges := [sscTransformation, sscSample, sscStatus];
  FVolume := 1.0;
  FMinDistance := 1.0;
  FMaxDistance := 100.0;
  FInsideConeAngle := 360;
  FOutsideConeAngle := 360;
  FConeOutsideVolume := 0.0;
  FNbLoops := 1;
  FFrequency := -1;
end;

destructor TgxBaseSoundSource.Destroy;
begin
  inherited Destroy;
end;

function TgxBaseSoundSource.GetDisplayName: string;
begin
  Result := Format('%s', [FSoundName]);
end;

procedure TgxBaseSoundSource.Assign(Source: TPersistent);
begin
  if Source is TgxBaseSoundSource then
  begin
    FPriority := TgxBaseSoundSource(Source).FPriority;
    FOrigin := TgxBaseSoundSource(Source).FOrigin;
    FVolume := TgxBaseSoundSource(Source).FVolume;
    FMinDistance := TgxBaseSoundSource(Source).FMinDistance;
    FMaxDistance := TgxBaseSoundSource(Source).FMaxDistance;
    FInsideConeAngle := TgxBaseSoundSource(Source).FInsideConeAngle;
    FOutsideConeAngle := TgxBaseSoundSource(Source).FOutsideConeAngle;
    FConeOutsideVolume := TgxBaseSoundSource(Source).FConeOutsideVolume;
    FSoundLibraryName := TgxBaseSoundSource(Source).FSoundLibraryName;
    FSoundLibrary := TgxBaseSoundSource(Source).FSoundLibrary;
    FSoundName := TgxBaseSoundSource(Source).FSoundName;
    FMute := TgxBaseSoundSource(Source).FMute;
    FPause := TgxBaseSoundSource(Source).FPause;
    FChanges := [sscTransformation, sscSample, sscStatus];
    FNbLoops := TgxBaseSoundSource(Source).FNbLoops;
    FFrequency := TgxBaseSoundSource(Source).FFrequency;
  end
  else
    inherited Assign(Source);
end;

procedure TgxBaseSoundSource.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FPriority);
    WriteFloat(FVolume);
    WriteFloat(FMinDistance);
    WriteFloat(FMaxDistance);
    WriteFloat(FInsideConeAngle);
    WriteFloat(FOutsideConeAngle);
    WriteFloat(FConeOutsideVolume);
    if Assigned(FSoundLibrary) then
      WriteString(FSoundLibrary.Name)
    else
      WriteString(FSoundLibraryName);
    WriteString(FSoundName);
    WriteBoolean(FMute);
    WriteBoolean(FPause);
    WriteInteger(FNbLoops);
    //      WriteInteger(FFrequency);
  end;
end;

procedure TgxBaseSoundSource.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FPriority := ReadInteger;
    FVolume := ReadFloat;
    FMinDistance := ReadFloat;
    FMaxDistance := ReadFloat;
    FInsideConeAngle := ReadFloat;
    FOutsideConeAngle := ReadFloat;
    FConeOutsideVolume := ReadFloat;
    FSoundLibraryName := ReadString;
    FSoundLibrary := nil;
    FSoundName := ReadString;
    FMute := ReadBoolean;
    FPause := ReadBoolean;
    FChanges := [sscTransformation, sscSample, sscStatus];
    FNbLoops := ReadInteger;
    //      FFrequency:=ReadInteger;
  end;
end;

function TgxBaseSoundSource.Sample: TgxSoundSample;
begin
  if SoundLibrary <> nil then
    Result := FSoundLibrary.Samples.GetByName(FSoundName)
  else
    Result := nil;
end;

procedure TgxBaseSoundSource.SetPriority(const val: Integer);
begin
  if val <> FPriority then
  begin
    FPriority := val;
    Include(FChanges, sscStatus);
  end;
end;

procedure TgxBaseSoundSource.SetOrigin(const val: TgxBaseSceneObject);
begin
  if val <> FOrigin then
  begin
    FOrigin := val;
    Include(FChanges, sscTransformation);
  end;
end;

procedure TgxBaseSoundSource.SetVolume(const val: Single);
begin
  if val <> FVolume then
  begin
    FVolume := ClampValue(val, 0, 1);
    Include(FChanges, sscStatus);
  end;
end;

procedure TgxBaseSoundSource.SetMinDistance(const val: Single);
begin
  if val <> FMinDistance then
  begin
    FMinDistance := ClampValue(val, 0);
    Include(FChanges, sscStatus);
  end;
end;

procedure TgxBaseSoundSource.SetMaxDistance(const val: Single);
begin
  if val <> FMaxDistance then
  begin
    FMaxDistance := ClampValue(val, 0);
    Include(FChanges, sscStatus);
  end;
end;

procedure TgxBaseSoundSource.SetInsideConeAngle(const val: Single);
begin
  if val <> FInsideConeAngle then
  begin
    FInsideConeAngle := ClampValue(val, 0, 360);
    Include(FChanges, sscStatus);
  end;
end;

procedure TgxBaseSoundSource.SetOutsideConeAngle(const val: Single);
begin
  if val <> FOutsideConeAngle then
  begin
    FOutsideConeAngle := ClampValue(val, 0, 360);
    Include(FChanges, sscStatus);
  end;
end;

procedure TgxBaseSoundSource.SetConeOutsideVolume(const val: Single);
begin
  if val <> FConeOutsideVolume then
  begin
    FConeOutsideVolume := ClampValue(val, 0, 1);
    Include(FChanges, sscStatus);
  end;
end;

function TgxBaseSoundSource.GetSoundLibrary: TgxSoundLibrary;
begin
  if (FSoundLibrary = nil) and (FSoundLibraryName <> '') then
    FSoundLibrary := GetSoundLibraryByName(FSoundLibraryName);
  Result := FSoundLibrary;
end;

procedure TgxBaseSoundSource.SetSoundLibrary(const val: TgxSoundLibrary);
begin
  if val <> FSoundLibrary then
  begin
    FSoundLibrary := val;
    if Assigned(FSoundLibrary) then
      FSoundLibraryName := FSoundLibrary.Name
    else
      FSoundLibraryName := '';
    Include(FChanges, sscSample);
  end;
end;

procedure TgxBaseSoundSource.SetSoundName(const val: string);
begin
  if val <> FSoundName then
  begin
    FSoundName := val;
    Include(FChanges, sscSample);
  end;
end;

procedure TgxBaseSoundSource.SetPause(const val: Boolean);
begin
  if val <> FPause then
  begin
    FPause := val;
    if Collection <> nil then
      TgxSoundManager(TgxSoundSources(Collection).owner).PauseSource(Self,
        FPause);
  end;
end;

procedure TgxBaseSoundSource.SetNbLoops(const val: Integer);
begin
  if val <> FNbLoops then
  begin
    FNbLoops := val;
    Include(FChanges, sscSample);
  end;
end;

procedure TgxBaseSoundSource.SetFrequency(const val: integer);
begin
  if val <> FFrequency then
  begin
    FFrequency := val;
    Include(FChanges, sscStatus);
  end;
end;

procedure TgxBaseSoundSource.SetMute(const val: Boolean);
begin
  if val <> FMute then
  begin
    FMute := val;
    if Collection <> nil then
      TgxSoundManager(TgxSoundSources(Collection).owner).MuteSource(Self,
        FMute);
  end;
end;

// ------------------
// ------------------ TgxSoundSource ------------------
// ------------------

destructor TgxSoundSource.Destroy;
begin
  if Assigned(FBehaviourToNotify) then
    FBehaviourToNotify.NotifySourceDestruction(Self);
  if Collection <> nil then
    ((Collection as TgxSoundSources).Owner as TgxSoundManager).KillSource(Self);
  inherited;
end;

// ------------------
// ------------------ TgxSoundSources ------------------
// ------------------

constructor TgxSoundSources.Create(AOwner: TComponent);
begin
  Owner := AOwner;
  inherited Create(TgxSoundSource);
end;

function TgxSoundSources.GetOwner: TPersistent;
begin
  Result := Owner;
end;

procedure TgxSoundSources.SetItems(index: Integer; const val: TgxSoundSource);
begin
  inherited Items[index] := val;
end;

function TgxSoundSources.GetItems(index: Integer): TgxSoundSource;
begin
  Result := TgxSoundSource(inherited Items[index]);
end;

function TgxSoundSources.Add: TgxSoundSource;
begin
  Result := (inherited Add) as TgxSoundSource;
end;

function TgxSoundSources.FindItemID(ID: Integer): TgxSoundSource;
begin
  Result := (inherited FindItemID(ID)) as TgxSoundSource;
end;

// ------------------
// ------------------ TgxSoundManager ------------------
// ------------------

constructor TgxSoundManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSources := TgxSoundSources.Create(Self);
  FMasterVolume := 1.0;
  FOutputFrequency := 44100;
  FMaxChannels := 8;
  FUpdateFrequency := 10;
  FLastUpdateTime := -1e30;
  FDistanceFactor := 1.0;
  FRollOffFactor := 1.0;
  FDopplerFactor := 1.0;
end;

destructor TgxSoundManager.Destroy;
begin
  Active := False;
  Listener := nil;
  FSources.Free;
  inherited Destroy;
end;

procedure TgxSoundManager.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FListener then
      Listener := nil;
    if AComponent = FCadencer then
      Cadencer := nil;
  end;
  inherited;
end;

procedure TgxSoundManager.SetActive(const val: Boolean);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    FActive := val
  else if val <> FActive then
  begin
    if val then
    begin
      if Assigned(vActiveSoundManager) then
        vActiveSoundManager.Active := False;
      if DoActivate then
      begin
        FActive := True;
        vActiveSoundManager := Self;
      end;
    end
    else
    begin
      try
        StopAllSources;
        DoDeActivate;
      finally
        FActive := val;
        vActiveSoundManager := nil;
      end;
    end;
  end;
end;

function TgxSoundManager.DoActivate: Boolean;
begin
  Result := True;
end;

procedure TgxSoundManager.DoDeActivate;
begin
  StopAllSources;
end;

procedure TgxSoundManager.SetMute(const val: Boolean);
begin
  if val <> FMute then
  begin
    if val then
    begin
      if DoMute then
        FMute := True
    end
    else
    begin
      DoUnMute;
      FMute := False;
    end;
  end;
end;

function TgxSoundManager.DoMute: Boolean;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Mute then
      MuteSource(Sources[i], True);
  Result := True;
end;

procedure TgxSoundManager.DoUnMute;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Mute then
      MuteSource(Sources[i], False);
end;

procedure TgxSoundManager.SetPause(const val: Boolean);
begin
  if val <> FPause then
  begin
    if val then
    begin
      if DoPause then
        FPause := True
    end
    else
    begin
      DoUnPause;
      FPause := False;
    end;
  end;
end;

procedure TgxSoundManager.Loaded;
begin
  inherited;
  if Active and (not (csDesigning in ComponentState)) then
  begin
    FActive := False;
    Active := True;
  end;
end;

procedure TgxSoundManager.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Doppler', ReadDoppler, WriteDoppler, (DopplerFactor <>
    1));
end;

procedure TgxSoundManager.WriteDoppler(writer: TWriter);
begin
  writer.WriteFloat(DopplerFactor);
end;

procedure TgxSoundManager.ReadDoppler(reader: TReader);
begin
  FDopplerFactor := reader.ReadFloat;
end;

function TgxSoundManager.DoPause: Boolean;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Pause then
      PauseSource(Sources[i], True);
  Result := True;
end;

procedure TgxSoundManager.DoUnPause;
var
  i: Integer;
begin
  for i := 0 to Sources.Count - 1 do
    if not Sources[i].Pause then
      PauseSource(Sources[i], False);
end;

procedure TgxSoundManager.SetMasterVolume(const val: Single);
begin
  if val < 0 then
    FMasterVolume := 0
  else if val > 1 then
    FMasterVolume := 1
  else
    FMasterVolume := val;
  NotifyMasterVolumeChange;
end;

procedure TgxSoundManager.SetMaxChannels(const val: Integer);
begin
  if val <> FMaxChannels then
  begin
    if val < 1 then
      FMaxChannels := 1
    else
      FMaxChannels := val;
  end;
end;

procedure TgxSoundManager.SetOutputFrequency(const val: Integer);
begin
  if val <> FOutputFrequency then
  begin
    if val < 11025 then
      FOutputFrequency := 11025
    else
      FOutputFrequency := val;
  end;
end;

procedure TgxSoundManager.SetUpdateFrequency(const val: Single);
begin
  FUpdateFrequency := ClampValue(val, 1, 60);
end;

function TgxSoundManager.StoreUpdateFrequency: Boolean;
begin
  Result := (FUpdateFrequency <> 10);
end;

procedure TgxSoundManager.SetCadencer(const val: TgxCadencer);
begin
  if val <> FCadencer then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(Self);
    FCadencer := val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(Self);
  end;
end;

procedure TgxSoundManager.SetDistanceFactor(const val: Single);
begin
  if val <= 0 then
    FDistanceFactor := 1
  else
    FDistanceFactor := val;
  Notify3DFactorsChanged;
end;

function TgxSoundManager.StoreDistanceFactor: Boolean;
begin
  Result := (FDistanceFactor <> 1);
end;

procedure TgxSoundManager.SetRollOffFactor(const val: Single);
begin
  if val <= 0 then
    FRollOffFactor := 1
  else
    FRollOffFactor := val;
  Notify3DFactorsChanged;
end;

function TgxSoundManager.StoreRollOffFactor: Boolean;
begin
  Result := (FRollOffFactor <> 1);
end;

procedure TgxSoundManager.SetDopplerFactor(const val: Single);
begin
  if val < 0 then
    FDopplerFactor := 0
  else if val > 10 then
    FDopplerFactor := 10
  else
    FDopplerFactor := val;
  Notify3DFactorsChanged;
end;

procedure TgxSoundManager.SetSoundEnvironment(const val: TgxSoundEnvironment);
begin
  if val <> FSoundEnvironment then
  begin
    FSoundEnvironment := val;
    NotifyEnvironmentChanged;
  end;
end;

procedure TgxSoundManager.ListenerCoordinates(var position, velocity, direction,
  up: TVector4f);
var
  right: TVector4f;
begin
  if Listener <> nil then
  begin
    position := Listener.AbsolutePosition;
    if FLastDeltaTime <> 0 then
    begin
      velocity := VectorSubtract(position, FLastListenerPosition);
      ScaleVector(velocity, 1 / FLastDeltaTime);
    end;
    FLastListenerPosition := position;
    if (Listener is TgxCamera) and (TgxCamera(Listener).TargetObject <> nil)
      then
    begin
      // special case of the camera targeting something
      direction := TgxCamera(Listener).AbsoluteVectorToTarget;
      NormalizeVector(direction);
      up := Listener.AbsoluteYVector;
      right := VectorCrossProduct(direction, up);
      up := VectorCrossProduct(right, direction);
    end
    else
    begin
      direction := Listener.AbsoluteZVector;
      up := Listener.AbsoluteYVector;
    end;
  end
  else
  begin
    position := NullHmgPoint;
    velocity := NullHmgVector;
    direction := ZHmgVector;
    up := YHmgVector;
  end;
end;

procedure TgxSoundManager.NotifyMasterVolumeChange;
begin
  // nothing
end;

procedure TgxSoundManager.Notify3DFactorsChanged;
begin
  // nothing
end;

procedure TgxSoundManager.NotifyEnvironmentChanged;
begin
  // nothing
end;

procedure TgxSoundManager.SetListener(const val: TgxBaseSceneObject);
begin
  if Assigned(FListener) then
    FListener.RemoveFreeNotification(Self);
  FListener := val;
  if Assigned(FListener) then
    FListener.FreeNotification(Self);
end;

procedure TgxSoundManager.SetSources(const val: TgxSoundSources);
begin
  FSources.Assign(val);
end;

procedure TgxSoundManager.KillSource(aSource: TgxBaseSoundSource);
begin
  // nothing
end;

procedure TgxSoundManager.UpdateSource(aSource: TgxBaseSoundSource);
begin
  aSource.FChanges := [];
end;

procedure TgxSoundManager.MuteSource(aSource: TgxBaseSoundSource; muted:
  Boolean);
begin
  // nothing
end;

procedure TgxSoundManager.PauseSource(aSource: TgxBaseSoundSource; paused:
  Boolean);
begin
  // nothing
end;

procedure TgxSoundManager.UpdateSources;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do
    UpdateSource(Sources[i]);
end;

procedure TgxSoundManager.StopAllSources;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do
    Sources.Delete(i);
end;

procedure TgxSoundManager.DoProgress(const progressTime: TgxProgressTimes);
begin
  if not Active then
    Exit;
  with progressTime do
    if newTime - FLastUpdateTime > 1 / FUpdateFrequency then
    begin
      FLastDeltaTime := newTime - FLastUpdateTime;
      FLastUpdateTime := newTime;
      UpdateSources;
    end;
end;

function TgxSoundManager.CPUUsagePercent: Single;
begin
  Result := -1;
end;

function TgxSoundManager.EAXSupported: Boolean;
begin
  Result := False;
end;

// ------------------
// ------------------ TgxBSoundEmitter ------------------
// ------------------

constructor TgxBSoundEmitter.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FSource := TgxSoundSource.Create(nil);
end;

destructor TgxBSoundEmitter.Destroy;
begin
  if Assigned(FPlayingSource) then
    FPlayingSource.Free;
  FSource.Free;
  inherited Destroy;
end;

procedure TgxBSoundEmitter.Assign(Source: TPersistent);
begin
  if Source is TgxBSoundEmitter then
  begin
    FSource.Assign(TgxBSoundEmitter(Source).FSource);
  end;
  inherited Assign(Source);
end;

procedure TgxBSoundEmitter.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FSource.WriteToFiler(writer);
    WriteBoolean(FPlaying);
  end;
end;

procedure TgxBSoundEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FSource.ReadFromFiler(reader);
    FPlaying := ReadBoolean;
  end;
end;

procedure TgxBSoundEmitter.Loaded;
begin
  inherited;
  if not (csDesigning in OwnerBaseSceneObject.ComponentState) then
    SetPlaying(FPlaying);
end;

class function TgxBSoundEmitter.FriendlyName: string;
begin
  Result := 'Sound Emitter';
end;

class function TgxBSoundEmitter.FriendlyDescription: string;
begin
  Result := 'A simple sound emitter behaviour';
end;

class function TgxBSoundEmitter.UniqueItem: Boolean;
begin
  Result := False;
end;

procedure TgxBSoundEmitter.DoProgress(const progressTime: TgxProgressTimes);
begin
  // nothing, yet
end;

procedure TgxBSoundEmitter.SetSource(const val: TgxBaseSoundSource);
begin
  FSource.Assign(val);
end;

procedure TgxBSoundEmitter.SetPlaying(const val: Boolean);
begin
  if csDesigning in OwnerBaseSceneObject.ComponentState then
    FPlaying := val
  else if ActiveSoundManager <> nil then
  begin
    if val <> Playing then
    begin
      if val then
      begin
        FPlayingSource := ActiveSoundManager.Sources.Add;
        FPlayingSource.FBehaviourToNotify := Self;
        FPlayingSource.Assign(FSource);
        FPlayingSource.Origin := OwnerBaseSceneObject;
      end
      else
        FPlayingSource.Free;
    end;
  end
  else if vVerboseGLSMErrors then
    InformationDlg('No Active Sound Manager.'#13#10'Make sure manager is created before emitter');
end;

function TgxBSoundEmitter.GetPlaying: Boolean;
begin
  if csDesigning in OwnerBaseSceneObject.ComponentState then
    Result := FPlaying
  else
    Result := Assigned(FPlayingSource);
end;

procedure TgxBSoundEmitter.NotifySourceDestruction(aSource: TgxSoundSource);
begin
  Assert(FPlayingSource = aSource);
  FPlayingSource := nil;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

   
  RegisterClasses([TgxSoundLibrary]);
  RegisterXCollectionItemClass(TgxBSoundEmitter);
  vSoundLibraries := TList.Create;

// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------

  if Assigned(vActiveSoundManager) then
    vActiveSoundManager.Active := False;

  vSoundLibraries.Free;
  vSoundLibraries := nil;

  UnregisterXCollectionItemClass(TgxBSoundEmitter);

end.
