//
// The graphics platform GLArena https://github.com/glscene
//
unit Soundx.SMFMOD;

(*
  FMOD based sound-manager (http://www.fmod.org/, free for freeware).
  Unsupported feature(s) : sound source velocity looping
  (sounds are played either once or forever) sound cones
*)

interface

{$I Scena.inc}

uses
  System.Classes,
  System.SysUtils,

  Soundx.FMOD,
  FMOD.Types,
  FMOD.Presets,

  Scena.VectorTypes,
  GLX.Sound,
  GLX.Scene,
  Scena.VectorGeometry;

type

  // TgxSMFMOD
  //
  TgxSMFMOD = class(TgxSoundManager)
  private
    FActivated: Boolean;
    FEAXCapable: Boolean; // not persistent
  protected
    function DoActivate: Boolean; override;
    procedure DoDeActivate; override;
    procedure NotifyMasterVolumeChange; override;
    procedure Notify3DFactorsChanged; override;
    procedure NotifyEnvironmentChanged; override;
    procedure KillSource(aSource: TgxBaseSoundSource); override;
    procedure UpdateSource(aSource: TgxBaseSoundSource); override;
    procedure MuteSource(aSource: TgxBaseSoundSource; muted: Boolean); override;
    procedure PauseSource(aSource: TgxBaseSoundSource; paused: Boolean); override;
    function GetDefaultFrequency(aSource: TgxBaseSoundSource): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSources; override;
    function CPUUsagePercent: Single; override;
    function EAXSupported: Boolean; override;
  published
    property MaxChannels default 32;
  end;

// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------

type
  TFMODInfo = record
    channel: Integer;
    pfs: PFSoundSample;
  end;

  PFMODInfo = ^TFMODInfo;

  // VectorToFMODVector
  //
procedure VectorToFMODVector(const aVector: TVector4f; var aFMODVector: TFSoundVector);
begin
  aFMODVector.x := aVector.x;
  aFMODVector.y := aVector.y;
  aFMODVector.z := -aVector.z;
end;

// ------------------
// ------------------ TgxSMFMOD ------------------
// ------------------

// Create
//
constructor TgxSMFMOD.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MaxChannels := 32;
end;

// Destroy
//
destructor TgxSMFMOD.Destroy;
begin
  inherited Destroy;
end;

// DoActivate
//
function TgxSMFMOD.DoActivate: Boolean;
var
  cap: Cardinal;
begin
  FMOD_Load(nil);
{$IFDEF MSWindows}
  if not FSOUND_SetOutput(FSOUND_OUTPUT_DSOUND) then
  begin
    Result := False;
    Exit;
  end;
{$ENDIF}
{$IFDEF LINUX}
  if not FSOUND_SetOutput(FSOUND_OUTPUT_ALSA) then
  begin
    Result := False;
    Exit;
  end;
{$ENDIF}
  if not FSOUND_SetDriver(0) then
  begin
    Result := False;
    Exit;
  end;
  cap := 0;
  if FSOUND_GetDriverCaps(0, cap) then
    FEAXCapable := ((cap and (FSOUND_CAPS_EAX2 or FSOUND_CAPS_EAX3)) > 0)
  else
    Assert(False, 'Failed to retrieve driver Caps.');
  if not FSOUND_Init(OutputFrequency, MaxChannels, 0) then
    Assert(False, 'FSOUND_Init failed.');
  FActivated := True;
  NotifyMasterVolumeChange;
  Notify3DFactorsChanged;
  if Environment <> seDefault then
    NotifyEnvironmentChanged;
  Result := True;
end;

// DoDeActivate
//
procedure TgxSMFMOD.DoDeActivate;
begin
  FSOUND_StopSound(FSOUND_ALL);
  FSOUND_Close;
  FMOD_Unload;
  FEAXCapable := False;
end;

// NotifyMasterVolumeChange
//
procedure TgxSMFMOD.NotifyMasterVolumeChange;
begin
  if FActivated then
    FSOUND_SetSFXMasterVolume(Round(MasterVolume * 255));
end;

// Notify3DFactorsChanged
//
procedure TgxSMFMOD.Notify3DFactorsChanged;
begin
  if FActivated then
  begin
    FSOUND_3D_SetDistanceFactor(DistanceFactor);
    FSOUND_3D_SetRolloffFactor(RollOffFactor);
    FSOUND_3D_SetDopplerFactor(DopplerFactor);
  end;
end;

// NotifyEnvironmentChanged
//
procedure TgxSMFMOD.NotifyEnvironmentChanged;
var
  SoundRevProps: TFSoundReverbProperties;
begin
  if FActivated and EAXSupported then
  begin
    case Environment of
      seDefault:
        SoundRevProps := FSOUND_PRESET_GENERIC;
      sePaddedCell:
        SoundRevProps := FSOUND_PRESET_PADDEDCELL;
      seRoom:
        SoundRevProps := FSOUND_PRESET_ROOM;
      seBathroom:
        SoundRevProps := FSOUND_PRESET_BATHROOM;
      seLivingRoom:
        SoundRevProps := FSOUND_PRESET_LIVINGROOM;
      seStoneroom:
        SoundRevProps := FSOUND_PRESET_STONEROOM;
      seAuditorium:
        SoundRevProps := FSOUND_PRESET_AUDITORIUM;
      seConcertHall:
        SoundRevProps := FSOUND_PRESET_CONCERTHALL;
      seCave:
        SoundRevProps := FSOUND_PRESET_CAVE;
      seArena:
        SoundRevProps := FSOUND_PRESET_ARENA;
      seHangar:
        SoundRevProps := FSOUND_PRESET_HANGAR;
      seCarpetedHallway:
        SoundRevProps := FSOUND_PRESET_CARPETTEDHALLWAY;
      seHallway:
        SoundRevProps := FSOUND_PRESET_HALLWAY;
      seStoneCorridor:
        SoundRevProps := FSOUND_PRESET_STONECORRIDOR;
      seAlley:
        SoundRevProps := FSOUND_PRESET_ALLEY;
      seForest:
        SoundRevProps := FSOUND_PRESET_FOREST;
      seCity:
        SoundRevProps := FSOUND_PRESET_CITY;
      seMountains:
        SoundRevProps := FSOUND_PRESET_MOUNTAINS;
      seQuarry:
        SoundRevProps := FSOUND_PRESET_QUARRY;
      sePlain:
        SoundRevProps := FSOUND_PRESET_PLAIN;
      seParkingLot:
        SoundRevProps := FSOUND_PRESET_PARKINGLOT;
      seSewerPipe:
        SoundRevProps := FSOUND_PRESET_SEWERPIPE;
      seUnderWater:
        SoundRevProps := FSOUND_PRESET_UNDERWATER;
      seDrugged:
        SoundRevProps := FSOUND_PRESET_DRUGGED;
      seDizzy:
        SoundRevProps := FSOUND_PRESET_DIZZY;
      sePsychotic:
        SoundRevProps := FSOUND_PRESET_PSYCHOTIC;
    else
      Assert(False);
    end;
    FSOUND_Reverb_SetProperties(SoundRevProps);
  end;
end;

// KillSource
//
procedure TgxSMFMOD.KillSource(aSource: TgxBaseSoundSource);
var
  p: PFMODInfo;
begin
  if aSource.ManagerTag <> 0 then
  begin
    p := PFMODInfo(aSource.ManagerTag);
    aSource.ManagerTag := 0;
    if p.channel <> -1 then
      if not FSOUND_StopSound(p.channel) then
        Assert(False, IntToStr(Integer(p)));
    FSOUND_Sample_Free(p.pfs);
    FreeMem(p);
  end;
end;

// UpdateSource
//
procedure TgxSMFMOD.UpdateSource(aSource: TgxBaseSoundSource);
var
  p: PFMODInfo;
  objPos, objVel: TVector4f;
  position, velocity: TFSoundVector;
begin
  if (sscSample in aSource.Changes) then
  begin
    KillSource(aSource);
  end;

  if (aSource.Sample = nil) or (aSource.Sample.Data = nil) or (aSource.Sample.Data.WAVDataSize = 0)
  then
    Exit;
  if aSource.ManagerTag <> 0 then
  begin
    p := PFMODInfo(aSource.ManagerTag);
    if not FSOUND_IsPlaying(p.channel) then
    begin
      p.channel := -1;
      aSource.Free;
      Exit;
    end;
  end
  else
  begin
    p := AllocMem(SizeOf(TFMODInfo));
    p.channel := -1;
    p.pfs := FSOUND_Sample_Load(FSOUND_FREE, aSource.Sample.Data.WAVData,
      FSOUND_HW3D + FSOUND_LOOP_OFF + FSOUND_LOADMEMORY, 0, aSource.Sample.Data.WAVDataSize);

    if aSource.NbLoops > 1 then
      FSOUND_Sample_SetMode(p.pfs, FSOUND_LOOP_NORMAL);
    FSOUND_Sample_SetMinMaxDistance(p.pfs, aSource.MinDistance, aSource.MaxDistance);
    aSource.ManagerTag := Integer(p);
  end;
  if aSource.Origin <> nil then
  begin
    objPos := aSource.Origin.AbsolutePosition;
    objVel := NullHmgVector;
  end
  else
  begin
    objPos := NullHmgPoint;
    objVel := NullHmgVector;
  end;
  VectorToFMODVector(objPos, position);
  VectorToFMODVector(objVel, velocity);
  if p.channel = -1 then
    p.channel := FSOUND_PlaySound(FSOUND_FREE, p.pfs);
  if p.channel <> -1 then
  begin
    FSOUND_3D_SetAttributes(p.channel, @position, @velocity);
    FSOUND_SetVolume(p.channel, Round(aSource.Volume * 255));
    FSOUND_SetMute(p.channel, aSource.Mute);
    FSOUND_SetPaused(p.channel, aSource.Pause);
    FSOUND_SetPriority(p.channel, aSource.Priority);
    if aSource.Frequency > 0 then
      FSOUND_SetFrequency(p.channel, aSource.Frequency);
  end
  else
    aSource.Free;

  inherited UpdateSource(aSource);
end;

// MuteSource
//
procedure TgxSMFMOD.MuteSource(aSource: TgxBaseSoundSource; muted: Boolean);
var
  p: PFMODInfo;
begin
  if aSource.ManagerTag <> 0 then
  begin
    p := PFMODInfo(aSource.ManagerTag);
    FSOUND_SetMute(p.channel, muted);
  end;
end;

// PauseSource
//
procedure TgxSMFMOD.PauseSource(aSource: TgxBaseSoundSource; paused: Boolean);
var
  p: PFMODInfo;
begin
  if aSource.ManagerTag <> 0 then
  begin
    p := PFMODInfo(aSource.ManagerTag);
    FSOUND_SetPaused(p.channel, paused);
  end;
end;

// UpdateSources
//
procedure TgxSMFMOD.UpdateSources;
var
  objPos, objVel, objDir, objUp: TVector4f;
  position, velocity, fwd, top: TFSoundVector;
begin
  // update listener
  ListenerCoordinates(objPos, objVel, objDir, objUp);
  VectorToFMODVector(objPos, position);
  VectorToFMODVector(objVel, velocity);
  VectorToFMODVector(objDir, fwd);
  VectorToFMODVector(objUp, top);
  FSOUND_3D_Listener_SetAttributes(@position, @velocity, fwd.x, fwd.y, fwd.z, top.x, top.y, top.z);
  // update sources
  inherited;
  FSOUND_Update;
end;

// CPUUsagePercent
//
function TgxSMFMOD.CPUUsagePercent: Single;
begin
  Result := FSOUND_GetCPUUsage;
end;

// EAXSupported
//
function TgxSMFMOD.EAXSupported: Boolean;
begin
  Result := FEAXCapable;
end;

// GetDefaultFrequency
//
function TgxSMFMOD.GetDefaultFrequency(aSource: TgxBaseSoundSource): Integer;
var
  p: PFMODInfo;
  dfreq, dVol, dPan, dPri: Integer;
begin
  try
    p := PFMODInfo(aSource.ManagerTag);
    FSOUND_Sample_GetDefaults(p.pfs, dfreq, dVol, dPan, dPri);
    Result := dfreq;
  except
    Result := -1;
  end;
end;

end.
