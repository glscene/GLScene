//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FileHDR;

(* HDR File support *)

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.Classes,
  System.SysUtils,

  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.RGBE,
  GXS.ApplicationFileIO,
  GXS.Strings,

  GXS.Context,
  GXS.Graphics,
  GXS.TextureFormat;

type

  TgxHDRImage = class(TgxBaseImage)
  private
    function GetProgramType: Ansistring;
    procedure SetProgramType(aval: Ansistring);
  protected
    fGamma: Single; // image has already been gamma corrected with
    // given gamma.  defaults to 1.0 (no correction) */
    fExposure: Single; // a value of 1.0 in an image corresponds to
    // <exposure> watts/steradian/m^2.
    // defaults to 1.0
    fProgramType: string[16];
  public
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromFile(const filename: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure AssignFromTexture(textureContext: TgxContext;
      const textureHandle: GLuint;
      textureTarget: TgxTextureTarget;
      const CurrentFormat: Boolean;
      const intFormat: TgxInternalFormat); reintroduce;
    property Gamma: Single read fGamma;
    property Exposure: Single read fExposure;
    property ProgramType: Ansistring read GetProgramType write SetProgramType;
  end;

//====================================================================
implementation
//====================================================================

// ------------------
// ------------------ TgxHDRImage ------------------
// ------------------

procedure TgxHDRImage.LoadFromFile(const filename: string);
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

procedure TgxHDRImage.LoadFromStream(stream: TStream);
const
  cRgbeFormat32bit = 'FORMAT=32-bit_rle_rgbe';
  cGamma = 'GAMMA=';
  cEXPOSURE = 'EXPOSURE=';
  cY = '-Y ';
var
  buf: array [0 .. 1023] of AnsiChar;
  header: TStringList;
  s, sn: string;
  lineSize: integer;
  tempBuf, top, bottom: PByte;
  i, j, err: integer;
  formatDefined: Boolean;

  function CmpWord(const word: string): Boolean;
  var
    l: integer;
    ts: string;
  begin
    Result := false;
    ts := header.Strings[i];
    if Length(word) > Length(ts) then
      Exit;
    for l := 1 to Length(word) do
      if word[l] <> ts[l] then
        Exit;
    Result := true;
  end;

begin
  fProgramType := '';
  fGamma := 1.0;
  fExposure := 1.0;
  UnMipmap;
  // Read HDR header
  stream.Read(buf, Length(buf) * sizeOf(AnsiChar));
  header := TStringList.Create;
  s := '';
  i := 0;
  j := 0;
  while i < Length(buf) do
  begin
    if buf[i] = #0 then
      Break;
    if buf[i] = #10 then
    begin
      header.Add(s);
      s := '';
      Inc(i);
      j := i;
      Continue;
    end;
    s := s + string(buf[i]);
    Inc(i);
  end;
  if i < Length(buf) then
    stream.Position := j
  else
    raise EInvalidRasterFile.Create('Can''t find HDR header end.');

  if (header.Strings[0][1] <> '#') or (header.Strings[0][2] <> '?') then
  begin
    header.Free;
    raise EInvalidRasterFile.Create('Bad HDR initial token.');
  end;
  // Get program type
  SetProgramType(Ansistring(Copy(header.Strings[0], 3, Length(header.Strings[0]) - 2)));

  formatDefined := false;
  for i := 1 to header.Count - 1 do
  begin
    if header.Strings[i] = cRgbeFormat32bit then
      formatDefined := true
    else if CmpWord(cGamma) then
    begin
      j := Length(cGamma);
      s := Copy(header.Strings[i], j + 1, Length(header.Strings[i]) - j);
      val(s, fGamma, err);
      if err <> 0 then
        raise EInvalidRasterFile.Create('Bad HDR header.');
    end
    else if CmpWord(cEXPOSURE) then
    begin
      j := Length(cEXPOSURE);
      s := Copy(header.Strings[i], j + 1, Length(header.Strings[i]) - j);
      val(s, fExposure, err);
      if err <> 0 then
        raise EInvalidRasterFile.Create('Bad HDR header.');
    end
    else if CmpWord(cY) then
    begin
      j := Length(cY);
      s := Copy(header.Strings[i], j + 1, Length(header.Strings[i]) - j);
      j := Pos(' ', s);
      sn := Copy(s, 1, j - 1);
      val(sn, FLOD[0].Height, err);
      Delete(s, 1, j + 3); // scip '+X '
      val(s, FLOD[0].Width, err);
      if err <> 0 then
        raise EInvalidRasterFile.Create('Bad HDR header.');
    end
  end; // for i
  header.Free;

  if not formatDefined then
    raise EInvalidRasterFile.Create('no FORMAT specifier found.');

  if (FLOD[0].Width = 0) or (FLOD[0].Height = 0) then
    raise EInvalidRasterFile.Create('Bad image dimension.');
  // set all the parameters
  FLOD[0].Depth := 0;
  fColorFormat := GL_RGB;
  fInternalFormat := tfRGBA_FLOAT32;
  fDataType := GL_FLOAT;
  fCubeMap := false;
  fTextureArray := false;
  fElementSize := GetTextureElementSize(tfFLOAT_RGB32);
  ReallocMem(fData, DataSize);
  LoadRLEpixels(stream, PSingle(fData), FLOD[0].Width, FLOD[0].Height);

  // hdr images come in upside down then flip it
  lineSize := fElementSize * FLOD[0].Width;
  GetMem(tempBuf, lineSize);
  top := PByte(fData);
  bottom := top;
  Inc(bottom, lineSize * (FLOD[0].Height - 1));
  for j := 0 to (FLOD[0].Height div 2) - 1 do
  begin
    Move(top^, tempBuf^, lineSize);
    Move(bottom^, top^, lineSize);
    Move(tempBuf^, bottom^, lineSize);
    Inc(top, lineSize);
    Dec(bottom, lineSize);
  end;
  FreeMem(tempBuf);
end;

function TgxHDRImage.GetProgramType: Ansistring;
begin
  Result := fProgramType;
end;

procedure TgxHDRImage.SetProgramType(aval: Ansistring);
var
  i: integer;
begin
  for i := 1 to Length(fProgramType) do
    fProgramType[i] := aval[i];
end;

procedure TgxHDRImage.AssignFromTexture(textureContext: TgxContext; const textureHandle: GLuint;
  textureTarget: TgxTextureTarget; const CurrentFormat: Boolean; const intFormat: TgxInternalFormat);
var
  oldContext: TgxContext;
  contextActivate: Boolean;
  texFormat: Cardinal;
  residentFormat: TgxInternalFormat;
  glTarget: GLEnum;
begin
  glTarget := DecodeTextureTarget(textureTarget);
  if not((glTarget = GL_TEXTURE_2D) or (glTarget = GL_TEXTURE_RECTANGLE)) then
    Exit;

  oldContext := CurrentContext;
  contextActivate := (oldContext <> textureContext);
  if contextActivate then
  begin
    if Assigned(oldContext) then
      oldContext.Deactivate;
    textureContext.Activate;
  end;

  try
    textureContext.gxStates.TextureBinding[0, textureTarget] := textureHandle;
    fLevelCount := 0;
    fCubeMap := false;
    fTextureArray := false;
    fColorFormat := GL_RGB;
    fDataType := GL_FLOAT;
    // Check level existence
    glGetTexLevelParameteriv(glTarget, 0, GL_TEXTURE_INTERNAL_FORMAT, @texFormat);
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
      Inc(fLevelCount);
    end;

    if fLevelCount > 0 then
    begin
      fElementSize := GetTextureElementSize(fColorFormat, fDataType);
      ReallocMem(fData, DataSize);
      glGetTexImage(glTarget, 0, fColorFormat, fDataType, fData);
    end
    else
      fLevelCount := 1;
    /// CheckOpenGLError;
  finally
    if contextActivate then
    begin
      textureContext.Deactivate;
      if Assigned(oldContext) then
        oldContext.Activate;
    end;
  end;
end;

class function TgxHDRImage.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead { , dfcWrite } ];
end;

//-------------------------------------------------------------------
initialization
//-------------------------------------------------------------------

  RegisterRasterFormat('hdr', 'High Dynamic Range Image', TgxHDRImage);

end.
