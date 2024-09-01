//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FilePNG;

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.Classes,
  System.SysUtils,

  GXS.Context,
  GXS.Graphics,
  GXS.TextureFormat,
  GXS.ApplicationFileIO;

type

  TgxPNGImage = class(TgxBaseImage)
  private
  public
    class function Capabilities: TDataFileCapabilities; override;

    procedure LoadFromFile(const filename: string); override;
    procedure SaveToFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;

    { Assigns from any Texture.}
    procedure AssignFromTexture(textureContext: TgxContext;
      const textureHandle: GLuint;
      textureTarget: TgxTextureTarget;
      const CurrentFormat: Boolean;
      const intFormat: TgxInternalFormat); reintroduce;
  end;

//==============================================================
implementation
//==============================================================


// ------------------
// ------------------ TgxPNGImage ------------------
// ------------------

procedure TgxPNGImage.LoadFromFile(const filename: string);
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

procedure TgxPNGImage.SaveToFile(const filename: string);
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

procedure TgxPNGImage.LoadFromStream(stream: TStream);
begin
  //Do nothing
end;

procedure TgxPNGImage.SaveToStream(stream: TStream);
begin
  //Do nothing
end;

procedure TgxPNGImage.AssignFromTexture(textureContext: TgxContext;
  const textureHandle: GLuint;
  textureTarget: TgxTextureTarget;
  const CurrentFormat: Boolean;
  const intFormat: TgxInternalFormat);
var
  oldContext: TgxContext;
  contextActivate: Boolean;
  texFormat: Cardinal;
  residentFormat: TgxInternalFormat;
  glTarget: GLEnum;
begin
  if not ((textureTarget = ttTexture2D)
    or (textureTarget = ttTextureRect)) then
    Exit;

  oldContext := CurrentContext;
  contextActivate := (oldContext <> textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then
      oldContext.Deactivate;
    textureContext.Activate;
  end;
  glTarget := DecodeTextureTarget(textureTarget);

  try
    textureContext.gxStates.TextureBinding[0, textureTarget] := textureHandle;
    fLevelCount := 0;
    fCubeMap := false;
    fTextureArray := false;
    // Check level existence
    glGetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_INTERNAL_FORMAT,
      @texFormat);
    if texFormat > 1 then
    begin
      glGetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_WIDTH, @FLOD[0].Width);
      glGetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_HEIGHT, @FLOD[0].Height);
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
      glGetTexImage(glTarget, 0, fColorFormat, fDataType, fData);
    end
    else
      fLevelCount := 1;
///    CheckOpenGLError;
  finally
    if contextActivate then
    begin
      textureContext.Deactivate;
      if Assigned(oldContext) then
        oldContext.Activate;
    end;
  end;
end;

class function TgxPNGImage.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

//----------------------------------------------------------
initialization
//----------------------------------------------------------

  { Register this Fileformat-Handler with GXScene }
  RegisterRasterFormat('png', 'Portable Network Graphic', TgxPNGImage);

end.
