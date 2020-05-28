//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
  PNG files loading implementation
}

unit GLFilePNG;

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  VCL.Imaging.pngimage,
  GLStrings,
  OpenGLTokens,
  GLContext,
  GLGraphics,
  GLTextureFormat,
  GLApplicationFileIO;

type

  TGLPNGImage = class(TGLBaseImage)
  private
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    {Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TGLContext; const textureHandle: Cardinal;
      textureTarget: TGLTextureTarget;
      const CurrentFormat: Boolean;
      const intFormat: TGLInternalFormat); reintroduce;
  end;

//--------------------------------------------------------------
implementation
//--------------------------------------------------------------

// ------------------
// ------------------ TGLPNGImage ------------------
// ------------------


procedure TGLPNGImage.LoadFromFile(const filename: string);
var
  fs: TStream;
begin
  if FileStreamExists(fileName) then
  begin
    fs := CreateFileStream(fileName, fmOpenRead);
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


procedure TGLPNGImage.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  fs := CreateFileStream(fileName, fmOpenWrite or fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
  ResourceName := filename;
end;


procedure TGLPNGImage.LoadFromStream(AStream: TStream);
var
  PngImage: TPngImage;
  rowBytes: Cardinal;

begin
  try
    PngImage := TPngImage.Create;
    PngImage.LoadFromStream(AStream);

    UpdateLevelsInfo;
    ReallocMem(fData, rowBytes * Cardinal(GetHeight));

  finally
    PngImage.Free;
  end;
end;

procedure TGLPNGImage.SaveToStream(AStream: TStream);
var
  PngImage: TPngImage;
begin
  try
    PngImage := TPngImage.Create;
    PngImage.SaveToStream(AStream);
  finally
    PngImage.Free;
  end;
end;


procedure TGLPNGImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: Cardinal;
  textureTarget: TGLTextureTarget;
  const CurrentFormat: Boolean;
  const intFormat: TGLInternalFormat);
var
  oldContext: TGLContext;
  contextActivate: Boolean;
  texFormat: Cardinal;
  residentFormat: TGLInternalFormat;
  glTarget: Cardinal;
begin
  if not ((textureTarget = ttTexture2D)
    or (textureTarget = ttTextureRect)) then
    Exit;

  oldContext := CurrentGLContext;
  contextActivate := (oldContext <> textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then
      oldContext.Deactivate;
    textureContext.Activate;
  end;
  glTarget := DecodeTextureTarget(textureTarget);

  try
    textureContext.GLStates.TextureBinding[0, textureTarget] := textureHandle;
    fLevelCount := 0;
    fCubeMap := false;
    fTextureArray := false;
    // Check level existence
    gl.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_INTERNAL_FORMAT,
      @texFormat);
    if texFormat > 1 then
    begin
      gl.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_WIDTH, @FLOD[0].Width);
      gl.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_HEIGHT, @FLOD[0].Height);
      FLOD[0].Depth := 0;
      residentFormat := OpenGLFormatToInternalFormat(texFormat);
      if CurrentFormat then
        fInternalFormat := residentFormat
      else
        fInternalFormat := intFormat;
      FindCompatibleDataFormat(fInternalFormat, fColorFormat, fDataType);
      Inc(fLevelCount);
    end;
    if fLevelCount > 0 then
    begin
      fElementSize := GetTextureElementSize(fColorFormat, fDataType);
      ReallocMem(FData, DataSize);
      gl.GetTexImage(glTarget, 0, fColorFormat, fDataType, fData);
    end
    else
      fLevelCount := 1;
    gl.CheckError;
  finally
    if contextActivate then
    begin
      textureContext.Deactivate;
      if Assigned(oldContext) then
        oldContext.Activate;
    end;
  end;
end;


class function TGLPNGImage.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

//---------------------------------------------------
initialization
//---------------------------------------------------

  RegisterRasterFormat('png', 'Portable Network Graphic', TGLPNGImage);

end.
