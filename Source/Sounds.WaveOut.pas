//
// The graphics rendering engine GLScene http://glscene.org
//
unit Sounds.WaveOut;

(* Basic sound manager based on WinMM *)

interface

{$I GLScene.inc}

uses
  Winapi.MMSystem,
  System.Classes,
  System.SysUtils,

  GLS.Sound,
  GLS.SoundFileObjects;

type

  (* Basic sound manager based on WinMM waveOut function.
    This manager has NO 3D miximing capacity, this is merely a default manager
    that should work on any windows based system, and help showcasing/testing
    basic GLSS core functionality.
    Apart from 3D, mute, pause, priority and volume are ignored too, and only
    sampling conversions supported by the windows ACM driver are supported
    (ie. no 4bits samples playback etc.). *)
  TGLSMWaveOut = class(TGLSoundManager)
  protected
    function DoActivate: Boolean; override;
    procedure DoDeActivate; override;
    procedure KillSource(aSource: TGLBaseSoundSource); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSources; override;
  published
    property MaxChannels default 4;
  end;

procedure PlayOnWaveOut(pcmData: Pointer; lengthInBytes: Integer;
  sampling: TGLSoundSampling); overload;
function PlayOnWaveOut(pcmData: Pointer; lengthInBytes: Integer;
  waveFormat: TWaveFormatEx): HWaveOut; overload;

// -------------------------------------------------------------
implementation
// -------------------------------------------------------------

type
  TSoundState = (ssPlaying, ssFinished);

  TWaveOutPlayingRec = record
    CurrentState: TSoundState;
    WaveOutDevice: HWaveOut;
    WaveHeader: wavehdr;
  end;

  PWaveOutPlayingRec = ^TWaveOutPlayingRec;

procedure _waveOutCallBack2(hwo: HWaveOut; uMsg: Cardinal;
  dwInstance, dwParam1, dwParam2: Integer); stdcall;
begin
  if uMsg = WOM_DONE then
    waveOutClose(hwo);
end;

function PlayOnWaveOut(pcmData: Pointer; lengthInBytes: Integer;
  waveFormat: TWaveFormatEx): HWaveOut;
var
  hwo: HWaveOut;
  wh: wavehdr;
  mmres: MMRESULT;
begin
  mmres := waveOutOpen(@hwo, WAVE_MAPPER, @waveFormat,
    Cardinal(@_waveOutCallBack2), 0, CALLBACK_FUNCTION);
  Assert(mmres = MMSYSERR_NOERROR, IntToStr(mmres));
  wh.dwBufferLength := lengthInBytes;
  wh.lpData := pcmData;
  wh.dwFlags := 0;
  wh.dwLoops := 1;
  wh.lpNext := nil;
  mmres := waveOutPrepareHeader(hwo, @wh, SizeOf(wavehdr));
  Assert(mmres = MMSYSERR_NOERROR, IntToStr(mmres));
  mmres := waveOutWrite(hwo, @wh, SizeOf(wavehdr));
  Assert(mmres = MMSYSERR_NOERROR, IntToStr(mmres));
  Result := hwo;
end;

procedure PlayOnWaveOut(pcmData: Pointer; lengthInBytes: Integer;
  sampling: TGLSoundSampling);
var
  wfx: TWaveFormatEx;
begin
  wfx := sampling.waveFormat;
  PlayOnWaveOut(pcmData, lengthInBytes, wfx);
end;

// ------------------
// ------------------ TGLSMWaveOut ------------------
// ------------------

constructor TGLSMWaveOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MaxChannels := 4;
end;

destructor TGLSMWaveOut.Destroy;
begin
  inherited Destroy;
end;

function TGLSMWaveOut.DoActivate: Boolean;
begin
  Result := True;
end;

procedure TGLSMWaveOut.DoDeActivate;
var
  i: Integer;
begin
  for i := Sources.Count - 1 downto 0 do
    KillSource(Sources[i]);
end;

procedure TGLSMWaveOut.KillSource(aSource: TGLBaseSoundSource);
var
  pRec: PWaveOutPlayingRec;
begin
  if aSource.ManagerTag <> 0 then
  begin
    pRec := PWaveOutPlayingRec(aSource.ManagerTag);
    if pRec.CurrentState = ssPlaying then
      waveOutReset(pRec.WaveOutDevice);
    waveOutUnprepareHeader(pRec.WaveOutDevice, @pRec.WaveHeader,
      SizeOf(wavehdr));
    waveOutClose(pRec.WaveOutDevice);
    Dispose(pRec);
    aSource.ManagerTag := 0;
  end;
end;

(*
 Note: This callback function is called from another thread, from MSDN docs:
 "Applications should not call any system-defined functions from inside a
 callback function, except for EnterCriticalSection, LeaveCriticalSection,
 midiOutLongMsg, midiOutShortMsg, OutputDebugString, PostMessage,
 PostThreadMessage, SetEvent, timeGetSystemTime, timeGetTime, timeKillEvent,
 and timeSetEvent. Calling other wave functions will cause deadlock."
*)
procedure _waveOutCallBack(hwo: HWaveOut; uMsg: Cardinal;
  dwInstance, dwParam1, dwParam2: Integer); stdcall;
begin
  if uMsg = WOM_DONE then
  begin
    PWaveOutPlayingRec(TGLSoundSource(dwInstance).ManagerTag).CurrentState :=
      ssFinished;
  end;
end;

procedure TGLSMWaveOut.UpdateSources;
var
  i, n: Integer;
  wfx: TWaveFormatEx;
  smp: TGLSoundSample;
  wh: wavehdr;
  mmres: MMRESULT;
  hwo: HWaveOut;
  pRec: PWaveOutPlayingRec;
begin
  // count nb of playing sources and delete done ones
  n := 0;
  for i := Sources.Count - 1 downto 0 do
    if Sources[i].ManagerTag <> 0 then
      if PWaveOutPlayingRec(Sources[i].ManagerTag).CurrentState = ssPlaying then
        Inc(n)
      else
        Sources.Delete(i);
  // start sources if some capacity remains, and forget the others
  for i := Sources.Count - 1 downto 0 do
    if Sources[i].ManagerTag = 0 then
    begin
      if n < MaxChannels then
      begin
        smp := Sources[i].Sample;
        if Assigned(smp) and (smp.Data <> nil) then
        begin
          wfx := smp.Data.sampling.waveFormat;
          mmres := waveOutOpen(@hwo, WAVE_MAPPER, @wfx,
            Cardinal(@_waveOutCallBack), Integer(Sources[i]),
            CALLBACK_FUNCTION);
          Assert(mmres = MMSYSERR_NOERROR, IntToStr(mmres));

          FillChar(wh, SizeOf(wh), 0);
          wh.dwBufferLength := smp.lengthInBytes;
          wh.lpData := smp.Data.pcmData;
          wh.dwLoops := Sources[i].NbLoops;
          if wh.dwLoops > 1 then
            wh.dwFlags := WHDR_BEGINLOOP + WHDR_ENDLOOP
          else
            wh.dwFlags := 0;
          wh.lpNext := nil;

          new(pRec);
          pRec.WaveOutDevice := hwo;
          pRec.WaveHeader := wh;
          pRec.CurrentState := ssPlaying;

          mmres := waveOutPrepareHeader(hwo, @pRec.WaveHeader, SizeOf(wavehdr));
          Assert(mmres = MMSYSERR_NOERROR, IntToStr(mmres));
          Sources[i].ManagerTag := Integer(pRec);
          mmres := waveOutWrite(hwo, @pRec.WaveHeader, SizeOf(wavehdr));
          Assert(mmres = MMSYSERR_NOERROR, IntToStr(mmres));

          Inc(n);
        end
        else
          Sources.Delete(i);
      end
      else
        Sources.Delete(i);
    end;
end;

initialization

RegisterClasses([TGLSMWaveOut]);

end.
