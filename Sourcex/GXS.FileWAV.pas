//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FileWAV;

(* Support for Windows WAV format *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.MMSystem,
  System.Classes,

  GXS.ApplicationFileIO,
  GXS.SoundFileObjects;

type

  // Support for Windows WAV format.
  TgxWAVFile = class(TgxSoundFile)
  private
{$IFDEF MSWINDOWS}
    waveFormat: TWaveFormatEx;
    pcmOffset: Integer;
{$ENDIF}
    FPCMDataLength: Integer;
    data: array of Byte; // used to store WAVE bitstream
  protected
  public
    function CreateCopy(AOwner: TPersistent): TgxDataFile; override;
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure PlayOnWaveOut; override;
    function WAVData: Pointer; override;
    function WAVDataSize: Integer; override;
    function PCMData: Pointer; override;
    function LengthInBytes: Integer; override;
  end;

//=================================================================
implementation
//=================================================================

{$IFDEF MSWINDOWS}

type
  TRIFFChunkInfo = packed record
    ckID: FOURCC;
    ckSize: LongInt;
  end;

const
  WAVE_Format_ADPCM = 2;
{$ENDIF}

  // ------------------
  // ------------------ TgxWAVFile ------------------
  // ------------------

function TgxWAVFile.CreateCopy(AOwner: TPersistent): TgxDataFile;
begin
  Result := inherited CreateCopy(AOwner);
  if Assigned(Result) then
  begin
{$IFDEF MSWINDOWS}
    TgxWAVFile(Result).waveFormat := waveFormat;
{$ENDIF}
    TgxWAVFile(Result).data := Copy(data);
  end;
end;

class function TgxWAVFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TgxWAVFile.LoadFromStream(Stream: TStream);
{$IFDEF MSWINDOWS}
var
  ck: TRIFFChunkInfo;
  dw, bytesToGo, startPosition, totalSize: Integer;
  id: Cardinal;
  dwDataOffset, dwDataSamples, dwDataLength: Integer;
begin
  // this WAVE loading code is an adaptation of the 'minimalist' sample from
  // the Microsoft DirectX SDK.
  Assert(Assigned(Stream));
  dwDataOffset := 0;
  dwDataLength := 0;
  // Check RIFF Header
  startPosition := Stream.Position;
  Stream.Read(ck, SizeOf(TRIFFChunkInfo));
  Assert((ck.ckID = mmioStringToFourCC('RIFF', 0)), 'RIFF required');
  totalSize := ck.ckSize + SizeOf(TRIFFChunkInfo);
  Stream.Read(id, SizeOf(Integer));
  Assert((id = mmioStringToFourCC('WAVE', 0)), 'RIFF-WAVE required');
  // lookup for 'fmt '
  repeat
    Stream.Read(ck, SizeOf(TRIFFChunkInfo));
    bytesToGo := ck.ckSize;
    if (ck.ckID = mmioStringToFourCC('fmt ', 0)) then
    begin
      if waveFormat.wFormatTag = 0 then
      begin
        dw := ck.ckSize;
        if dw > SizeOf(TWaveFormatEx) then
          dw := SizeOf(TWaveFormatEx);
        Stream.Read(waveFormat, dw);
        bytesToGo := ck.ckSize - dw;
      end;
      // other 'fmt ' chunks are ignored (?)
    end
    else if (ck.ckID = mmioStringToFourCC('fact', 0)) then
    begin
      if (dwDataSamples = 0) and (waveFormat.wFormatTag = WAVE_Format_ADPCM)
      then
      begin
        Stream.Read(dwDataSamples, SizeOf(LongInt));
        Dec(bytesToGo, SizeOf(LongInt));
      end;
      // other 'fact' chunks are ignored (?)
    end
    else if (ck.ckID = mmioStringToFourCC('data', 0)) then
    begin
      dwDataOffset := Stream.Position - startPosition;
      dwDataLength := ck.ckSize;
      Break;
    end;
    // all other sub-chunks are ignored, move to the next chunk
    Stream.Seek(bytesToGo, soFromCurrent);
  until Stream.Position = 2048; // this should never be reached
  // Only PCM wave format is recognized
  // Assert((waveFormat.wFormatTag=Wave_Format_PCM), 'PCM required');
  // seek start of data
  pcmOffset := dwDataOffset;
  FPCMDataLength := dwDataLength;
  SetLength(data, totalSize);
  Stream.Position := startPosition;
  if totalSize > 0 then
    Stream.Read(data[0], totalSize);
  // update Sampling data
  with waveFormat do
  begin
    Sampling.Frequency := nSamplesPerSec;
    Sampling.NbChannels := nChannels;
    Sampling.BitsPerSample := wBitsPerSample;
  end;
{$ELSE}

begin
  Assert(Assigned(Stream));
  SetLength(data, Stream.Size);
  if Length(data) > 0 then
    Stream.Read(data[0], Length(data));
{$ENDIF}
end;

procedure TgxWAVFile.SaveToStream(Stream: TStream);
begin
  if Length(data) > 0 then
    Stream.Write(data[0], Length(data));
end;

procedure TgxWAVFile.PlayOnWaveOut;
begin
{$IFDEF MSWINDOWS}
  PlaySound(WAVData, 0, SND_ASYNC + SND_MEMORY);
{$ENDIF}
  // GLSoundFileObjects.PlayOnWaveOut(PCMData, LengthInBytes, waveFormat);
end;

function TgxWAVFile.WAVData: Pointer;
begin
  if Length(data) > 0 then
    Result := @data[0]
  else
    Result := nil;
end;

function TgxWAVFile.WAVDataSize: Integer;
begin
  Result := Length(data);
end;

function TgxWAVFile.PCMData: Pointer;
begin
{$IFDEF MSWINDOWS}
  if Length(data) > 0 then
    Result := @data[pcmOffset]
  else
    Result := nil;
{$ELSE}
  Result := nil;
{$ENDIF}
end;

function TgxWAVFile.LengthInBytes: Integer;
begin
  Result := FPCMDataLength;
end;

//-------------------------------------------------
initialization
//-------------------------------------------------

RegisterSoundFileFormat('wav', 'Windows WAV files', TgxWAVFile);

end.
