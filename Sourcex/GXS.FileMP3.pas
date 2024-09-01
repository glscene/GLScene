//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FileMP3;

(* Support for MP3 format *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  GXS.ApplicationFileIO,
  GXS.SoundFileObjects;

type

  (* Support for MP3 format.
    *Partial* support only, access to PCMData is NOT supported. *)
  TgxMP3File = class(TgxSoundFile)
  private
    data: array of Byte; // used to store MP3 bitstream
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

//=============================================================
implementation
//=============================================================

// ------------------
// ------------------ TgxMP3File ------------------
// ------------------

function TgxMP3File.CreateCopy(AOwner: TPersistent): TgxDataFile;
begin
  Result := inherited CreateCopy(AOwner);
  if Assigned(Result) then
  begin
    TgxMP3File(Result).data := Copy(data);
  end;
end;

class function TgxMP3File.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TgxMP3File.LoadFromStream(Stream: TStream);
begin
  // MP3 isn't actually, just loaded directly...
  Assert(Assigned(Stream));
  SetLength(data, Stream.Size);
  if Length(data) > 0 then
    Stream.Read(data[0], Length(data));
end;

procedure TgxMP3File.SaveToStream(Stream: TStream);
begin
  if Length(data) > 0 then
    Stream.Write(data[0], Length(data));
end;

procedure TgxMP3File.PlayOnWaveOut;
begin
  Assert(False, 'MP3 playback on WaveOut not supported.');
end;

function TgxMP3File.WAVData: Pointer;
begin
  if Length(data) > 0 then
    Result := @data[0]
  else
    Result := nil;
end;

function TgxMP3File.WAVDataSize: Integer;
begin
  Result := Length(data);
end;

function TgxMP3File.PCMData: Pointer;
begin
  Result := nil;
end;

function TgxMP3File.LengthInBytes: Integer;
begin
  Result := 0;
end;

//------------------------------------------------------------------
initialization
//------------------------------------------------------------------

RegisterSoundFileFormat('mp3', 'MPEG Layer3 files', TgxMP3File);

end.
