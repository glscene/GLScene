//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FileJPEG;

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.VectorGeometry,
  GXS.Strings,
  GXS.Context,
  GXS.Graphics,
  GXS.TextureFormat,
  GXS.ApplicationFileIO;

type

  TgxJPEGImage = class(TgxBaseImage)
  private
    FAbortLoading: boolean;
    FDivScale: longword;
    FDither: boolean;
    FSmoothing: boolean;
    FProgressiveEncoding: boolean;
    procedure SetSmoothing(const AValue: boolean);
  public
    constructor Create; override;
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    // Assigns from any Texture.
    procedure AssignFromTexture(textureContext: TgxContext; const textureHandle: Cardinal; textureTarget: TgxTextureTarget;
      const CurrentFormat: boolean; const intFormat: TgxInternalFormat); reintroduce;
    property DivScale: longword read FDivScale write FDivScale;
    property Dither: boolean read FDither write FDither;
    property Smoothing: boolean read FSmoothing write SetSmoothing;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
  end;

// ===================================================================
implementation
// ===================================================================

// ------------------
// ------------------ TgxJPEGImage ------------------
// ------------------

constructor TgxJPEGImage.Create;
begin
  inherited;
  FAbortLoading := False;
  FDivScale := 1;
  FDither := False;
end;

procedure TgxJPEGImage.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  if FileStreamExists(filename) then
  begin
    fs := TFileStream.Create(filename, fmOpenRead);
    try
      LoadFromStream(fs);
    finally
      fs.Free;
      ResourceName := filename;
    end;
  end
  else
    raise EInvalidRasterFile.CreateFmt('File %s not found', [filename]);
end;

procedure TgxJPEGImage.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := TFileStream.Create(filename, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := filename;
end;

procedure TgxJPEGImage.LoadFromStream(stream: TStream);
begin
  // Do nothing
end;

procedure TgxJPEGImage.SaveToStream(stream: TStream);
begin
  // Do nothing
end;

procedure TgxJPEGImage.AssignFromTexture(textureContext: TgxContext; const textureHandle: Cardinal;
  textureTarget: TgxTextureTarget; const CurrentFormat: boolean; const intFormat: TgxInternalFormat);
begin
  //
end;

procedure TgxJPEGImage.SetSmoothing(const AValue: boolean);
begin
  if FSmoothing <> AValue then
    FSmoothing := AValue;
end;

class function TgxJPEGImage.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead { , dfcWrite } ];
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterRasterFormat('jpg', 'Joint Photographic Experts Group Image', TgxJPEGImage);
RegisterRasterFormat('jpeg', 'Joint Photographic Experts Group Image', TgxJPEGImage);
RegisterRasterFormat('jpe', 'Joint Photographic Experts Group Image', TgxJPEGImage);

end.
