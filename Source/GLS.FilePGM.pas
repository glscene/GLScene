//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.FilePGM;

(* PGM image loader *)

{$I GLScene.inc}

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  
  GLS.Context,
  GLS.Graphics,
  GLS.TextureFormat,
  GLS.Strings,
  GLS.ApplicationFileIO,

  CUDA.Utility;


type

  TGLPGMImage = class(TGLBaseImage)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
    procedure AssignFromTexture(textureContext: TGLContext;
      const textureHandle: Cardinal; textureTarget: TGLTextureTarget;
      const CurrentFormat: Boolean; const intFormat: TGLInternalFormat);
      reintroduce;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLPGMImage ------------------
// ------------------

procedure TGLPGMImage.LoadFromFile(const filename: string);
var
  w, h: Integer;
  cutBuffer: System.PSingle;
begin
  if FileExists(filename) then
  begin
    if not IsCUTILInitialized then
      if not InitCUTIL then
      begin
        EInvalidRasterFile.Create(strCUTILFailed);
        exit;
      end;
    cutBuffer := nil;
    if cutLoadPGMf(PAnsiChar(AnsiString(filename)), cutBuffer, w, h) then
    begin
      ResourceName := filename;
      UnMipmap;
      FLOD[0].Width := w;
      FLOD[0].Height := h;
      FLOD[0].Depth := 0;
      fColorFormat := GL_LUMINANCE;
      fInternalFormat := tfLUMINANCE_FLOAT32;
      fDataType := GL_FLOAT;
      fCubeMap := false;
      fTextureArray := false;
      fElementSize := GetTextureElementSize(tfLUMINANCE_FLOAT32);
      ReallocMem(fData, DataSize);
      Move(cutBuffer^, fData^, DataSize);
      cutFree(cutBuffer);
    end;
  end
  else
    raise EInvalidRasterFile.CreateFmt('File %s not found', [filename]);
end;

procedure TGLPGMImage.SaveToFile(const filename: string);
begin
  if not IsCUTILInitialized then
    if not InitCUTIL then
    begin
      EInvalidRasterFile.Create(strCUTILFailed);
      exit;
    end;
  if not cutSavePGMf(PAnsiChar(AnsiString(filename)), System.PSingle(fData),
    FLOD[0].Width, FLOD[0].Height) then
    raise EInvalidRasterFile.Create('Saving to file failed');
end;

procedure TGLPGMImage.LoadFromStream(stream: TStream);
begin
  Assert(false, 'Stream loading not supported');
end;

procedure TGLPGMImage.SaveToStream(stream: TStream);
begin
  Assert(false, 'Stream saving not supported');
end;

procedure TGLPGMImage.AssignFromTexture(textureContext: TGLContext;
  const textureHandle: Cardinal; textureTarget: TGLTextureTarget;
  const CurrentFormat: Boolean; const intFormat: TGLInternalFormat);
var
  oldContext: TGLContext;
  contextActivate: Boolean;
  texFormat: Cardinal;
  residentFormat: TGLInternalFormat;
  glTarget: Cardinal;
begin
  if not((textureTarget = ttTexture2D) or (textureTarget = ttTextureRect)) then
    exit;

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
    fColorFormat := GL_LUMINANCE;
    fDataType := GL_FLOAT;
    // Check level existence
    gl.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_INTERNAL_FORMAT,
      @texFormat);
    if texFormat > 1 then
    begin
      gl.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_WIDTH, @FLOD[0].Width);
      gl.GetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_HEIGHT,
        @FLOD[0].Height);
      FLOD[0].Depth := 0;
      residentFormat := OpenGLFormatToInternalFormat(texFormat);
      if CurrentFormat then
        fInternalFormat := residentFormat
      else
        fInternalFormat := intFormat;
      Inc(fLevelCount);
    end;
    if fLevelCount > 0 then
    begin
      fElementSize := GetTextureElementSize(fColorFormat, fDataType);
      ReallocMem(fData, DataSize);
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

class function TGLPGMImage.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

//------------------------------------------------
initialization
//------------------------------------------------

RegisterRasterFormat('pgm', 'Portable Graymap', TGLPGMImage);

end.
