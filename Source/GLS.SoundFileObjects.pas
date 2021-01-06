//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.SoundFileObjects;

(*
  Support classes for loading various fileformats.
  These classes work together like vector file formats or Delphi's TGraphic classes.
*)

interface

{$I GLScene.inc}

uses
  Winapi.MMSystem,
  System.Classes,
  System.SysUtils,
  VCL.Consts,

  GLS.ApplicationFileIO;

type

  // Defines a sound sampling quality.
  TGLSoundSampling = class(TPersistent)
  private
    FOwner: TPersistent;
    FFrequency: Integer;
    FNbChannels: Integer;
    FBitsPerSample: Integer;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function BytesPerSec: Integer;
    function BytesPerSample: Integer;
    function WaveFormat: TWaveFormatEx;
  published
    // Sampling frequency in Hz (= samples per sec)
    property Frequency: Integer read FFrequency write FFrequency default 22050;
    (* Nb of sampling channels.
      1 = mono, 2 = stereo, etc. *)
    property NbChannels: Integer read FNbChannels write FNbChannels default 1;
    (* Nb of bits per sample.
      Common values are 8 and 16 bits. *)
    property BitsPerSample: Integer read FBitsPerSample write FBitsPerSample default 8;
  end;

  (* Abstract base class for different Sound file formats.
    The actual implementation for these files (WAV, RAW...) must be done
    seperately. The concept for TGLSoundFile is very similar to TGraphic
    (see Delphi Help).
    Default implementation for LoadFromFile/SaveToFile are to directly call the
    relevent stream-based methods, ie. you will just have to override the stream
    methods in most cases. *)
  TGLSoundFile = class(TGLDataFile)
  private
    FSampling: TGLSoundSampling;
  protected
    procedure SetSampling(const val: TGLSoundSampling);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure PlayOnWaveOut; virtual;
    // Returns a pointer to the sample data viewed as an in-memory WAV File.
    function WAVData: Pointer; virtual; abstract;
    // Returns the size (in bytes) of the WAVData.
    function WAVDataSize: Integer; virtual; abstract;
    // Returns a pointer to the sample data viewed as an in-memory PCM buffer.
    function PCMData: Pointer; virtual; abstract;
    // Length of PCM data, in bytes.
    function LengthInBytes: Integer; virtual; abstract;
    // Nb of intensity samples in the sample.
    function LengthInSamples: Integer;
    // Length of play of the sample at nominal speed in seconds.
    function LengthInSec: Single;
    property Sampling: TGLSoundSampling read FSampling write SetSampling;
  end;

  TGLSoundFileClass = class of TGLSoundFile;

  TGLSoundFileFormat = record
    SoundFileClass: TGLSoundFileClass;
    Extension: String;
    Description: String;
    DescResID: Integer;
  end;

  PSoundFileFormat = ^TGLSoundFileFormat;

  TGLSoundFileFormatsList = class(TList)
  public
    destructor Destroy; override;
    procedure Add(const Ext, Desc: String; DescID: Integer;
      AClass: TGLSoundFileClass);
    function FindExt(Ext: string): TGLSoundFileClass;
    procedure Remove(AClass: TGLSoundFileClass);
    procedure BuildFilterStrings(SoundFileClass: TGLSoundFileClass;
      out Descriptions, Filters: string);
  end;

function GetGLSoundFileFormats: TGLSoundFileFormatsList;
procedure RegisterSoundFileFormat(const AExtension, ADescription: String;
  AClass: TGLSoundFileClass);
procedure UnregisterSoundFileClass(AClass: TGLSoundFileClass);

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  vSoundFileFormats: TGLSoundFileFormatsList;

function GetGLSoundFileFormats: TGLSoundFileFormatsList;
begin
  if not Assigned(vSoundFileFormats) then
    vSoundFileFormats := TGLSoundFileFormatsList.Create;
  Result := vSoundFileFormats;
end;

procedure RegisterSoundFileFormat(const AExtension, ADescription: String;
  AClass: TGLSoundFileClass);
begin
  RegisterClass(AClass);
  GetGLSoundFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

procedure UnregisterSoundFileClass(AClass: TGLSoundFileClass);
begin
  if Assigned(vSoundFileFormats) then
    vSoundFileFormats.Remove(AClass);
end;

// ------------------
// ------------------ TGLSoundSampling ------------------
// ------------------

constructor TGLSoundSampling.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FFrequency := 22050;
  FNbChannels := 1;
  FBitsPerSample := 8;
end;

destructor TGLSoundSampling.Destroy;
begin
  inherited Destroy;
end;

procedure TGLSoundSampling.Assign(Source: TPersistent);
begin
  if Source is TGLSoundSampling then
  begin
    FFrequency := TGLSoundSampling(Source).Frequency;
    FNbChannels := TGLSoundSampling(Source).NbChannels;
    FBitsPerSample := TGLSoundSampling(Source).BitsPerSample;
  end
  else
    inherited;
end;

function TGLSoundSampling.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TGLSoundSampling.BytesPerSec: Integer;
begin
  Result := (FFrequency * FBitsPerSample * FNbChannels) shr 3;
end;

function TGLSoundSampling.BytesPerSample: Integer;
begin
  Result := FBitsPerSample shr 3;
end;

function TGLSoundSampling.WaveFormat: TWaveFormatEx;
begin
  Result.nSamplesPerSec := Frequency;
  Result.nChannels := NbChannels;
  Result.wFormatTag := Wave_Format_PCM;
  Result.nAvgBytesPerSec := BytesPerSec;
  Result.wBitsPerSample := BitsPerSample;
  Result.nBlockAlign := NbChannels * BytesPerSample;
  Result.cbSize := 0;
end;

// ------------------
// ------------------ TGLSoundFile ------------------
// ------------------

constructor TGLSoundFile.Create(AOwner: TPersistent);
begin
  inherited;
  FSampling := TGLSoundSampling.Create(Self);
end;

destructor TGLSoundFile.Destroy;
begin
  FSampling.Free;
  inherited;
end;

procedure TGLSoundFile.SetSampling(const val: TGLSoundSampling);
begin
  FSampling.Assign(val);
end;

procedure TGLSoundFile.PlayOnWaveOut;
begin
  // GLSM.SoundFileObjects.PlayOnWaveOut(PCMData, LengthInSamples, Sampling);
end;

function TGLSoundFile.LengthInSamples: Integer;
var
  d: Integer;
begin
  d := Sampling.BytesPerSample * Sampling.NbChannels;
  if d > 0 then
    Result := LengthInBytes div d
  else
    Result := 0;
end;

function TGLSoundFile.LengthInSec: Single;
begin
  Result := LengthInBytes / Sampling.BytesPerSec;
end;

// ------------------
// ------------------ TGLSoundFileFormatsList ------------------
// ------------------

destructor TGLSoundFileFormatsList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Dispose(PSoundFileFormat(Items[i]));
  inherited;
end;

procedure TGLSoundFileFormatsList.Add(const Ext, Desc: String; DescID: Integer;
  AClass: TGLSoundFileClass);
var
  newRec: PSoundFileFormat;
begin
  New(newRec);
  with newRec^ do
  begin
    Extension := AnsiLowerCase(Ext);
    SoundFileClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(newRec);
end;

function TGLSoundFileFormatsList.FindExt(Ext: string): TGLSoundFileClass;
var
  i: Integer;
begin
  Ext := AnsiLowerCase(Ext);
  for i := Count - 1 downto 0 do
    with PSoundFileFormat(Items[i])^ do
      if (Extension = Ext) or ('.' + Extension = Ext) then
      begin
        Result := SoundFileClass;
        Exit;
      end;
  Result := nil;
end;

procedure TGLSoundFileFormatsList.Remove(AClass: TGLSoundFileClass);
var
  i: Integer;
  p: PSoundFileFormat;
begin
  for i := Count - 1 downto 0 do
  begin
    p := PSoundFileFormat(Items[i]);
    if p^.SoundFileClass.InheritsFrom(AClass) then
    begin
      Dispose(p);
      Delete(i);
    end;
  end;
end;

procedure TGLSoundFileFormatsList.BuildFilterStrings(SoundFileClass
  : TGLSoundFileClass; out Descriptions, Filters: string);
var
  c, i: Integer;
  p: PSoundFileFormat;
begin
  Descriptions := '';
  Filters := '';
  c := 0;
  for i := Count - 1 downto 0 do
  begin
    p := PSoundFileFormat(Items[i]);
    if p^.SoundFileClass.InheritsFrom(SoundFileClass) and (p^.Extension <> '')
    then
      with p^ do
      begin
        if c <> 0 then
        begin
          Descriptions := Descriptions + '|';
          Filters := Filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s', [Descriptions, Description,
          Extension]);
        FmtStr(Filters, '%s*.%s', [Filters, Extension]);
        Inc(c);
      end;
  end;
  if c > 1 then
    FmtStr(Descriptions, '%s (%s)|%1:s|%s',
      [sAllFilter, Filters, Descriptions]);
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

finalization

FreeAndNil(vSoundFileFormats);

end.

