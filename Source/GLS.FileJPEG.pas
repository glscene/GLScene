//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.FileJPEG;

(* Methods for loading Jpeg images *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,
  Vcl.Imaging.Jpeg,
  
  GLS.OpenGLTokens,
  GLS.Context,
  GLS.Graphics,
  GLS.TextureFormat,
  GLS.ApplicationFileIO,
  GLS.VectorGeometry;


type
  TGLJPEGImage = class(TGLBaseImage)
  private
    FAbortLoading: boolean;
    FDivScale: longword;
    FDither: boolean;
    FSmoothing: boolean;
    FProgressiveEncoding: boolean;
    procedure SetSmoothing(const AValue: boolean);
  public
    constructor Create; override;
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    // Assigns from any Texture.
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: Cardinal;
      textureTarget: TGLTextureTarget;
      const CurrentFormat: Boolean;
      const intFormat: TGLInternalFormat); reintroduce;
    property DivScale: longword read FDivScale write FDivScale;
    property Dither: boolean read FDither write FDither;
    property Smoothing: boolean read FSmoothing write SetSmoothing;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
  end;

procedure Jpeg2Bmp(const BmpFileName, JpgFileName: string);

//---------------------------------------------------------------------
implementation
//---------------------------------------------------------------------

procedure Jpeg2Bmp(const BmpFileName, JpgFileName: string);
var
  Bmp: TBitmap;
  Jpg: TJPEGImage;
begin
  Bmp := TBitmap.Create;
  Jpg := TJPEGImage.Create;
  try
    Jpg.LoadFromFile(JpgFileName);
    Bmp.Assign(Jpg);
    Bmp.SaveToFile(BmpFileName);
  finally
    Jpg.Free;
    Bmp.Free;
  end;
end;



// ------------------
// ------------------ TGLJPEGImage ------------------
// ------------------

constructor TGLJPEGImage.Create;
begin
  inherited;
  FAbortLoading := False;
  FDivScale := 1;
  FDither := False;
end;

procedure TGLJPEGImage.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := TFileStream.Create(fileName, fmOpenRead);
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

procedure TGLJPEGImage.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := TFileStream.Create(fileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := filename;
end;

procedure TGLJPEGImage.LoadFromStream(AStream: TStream);
var
  JpegImage: TJpegImage;

begin
  try
    JpegImage := TJPEGImage.Create;
    JpegImage.LoadFromStream(AStream);

    if JpegImage.Grayscale then
    begin
      fColorFormat := GL_LUMINANCE;
      fInternalFormat := tfLUMINANCE8;
      fElementSize := 1;
    end
    else
    begin
      fColorFormat := GL_BGR;
      fInternalFormat := tfRGB8;
      fElementSize := 3;
    end;
    fDataType := GL_UNSIGNED_BYTE;
    FLOD[0].Width := JpegImage.Width;
    FLOD[0].Height := JpegImage.Height;
    FLOD[0].Depth := 0;
    fCubeMap := False;
    fTextureArray := False;
    ReallocMem(fData, DataSize);

  finally
    JpegImage.Free;
  end;
end;

procedure TGLJPEGImage.SaveToStream(AStream: TStream);
begin

end;

procedure TGLJPEGImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: Cardinal; textureTarget: TGLTextureTarget;
  const CurrentFormat: boolean; const intFormat: TGLInternalFormat);
begin

end;

procedure TGLJPEGImage.SetSmoothing(const AValue: boolean);
begin
  if FSmoothing <> AValue then
    FSmoothing := AValue;
end;

class function TGLJPEGImage.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead {, dfcWrite}];
end;

//-----------------------------------
initialization
//-----------------------------------

  RegisterRasterFormat('jpg', 'Joint Photographic Experts Group Image',
    TGLJPEGImage);
  RegisterRasterFormat('jpeg', 'Joint Photographic Experts Group Image',
    TGLJPEGImage);
  RegisterRasterFormat('jpe', 'Joint Photographic Experts Group Image',
    TGLJPEGImage);
end.

