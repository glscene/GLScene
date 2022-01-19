unit fEarthD;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Material,
  GLS.Cadencer,
  GLS.LensFlare,
  GLS.Scene,
  GLS.Objects,
  GLS.Coordinates,
  GLS.SkyDome,
  GLS.SceneViewer,
  GLS.Texture,
  GLS.RenderContextInfo,
  GLS.Color,
  GLS.State,
  GLS.Utils,
  GLS.Context,
  GLS.TextureFormat,
  GLSL.TextureShaders,
  GLS.BaseClasses;


type
  TForm1 = class(TForm)
    Scene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    Camera: TGLCamera;
    SPEarth: TGLSphere;
    LSSun: TGLLightSource;
    DirectOpenGL: TGLDirectOpenGL;
    Cadencer: TGLCadencer;
    Timer1: TTimer;
    SPMoon: TGLSphere;
    dcEarth: TGLDummyCube;
    DCMoon: TGLDummyCube;
    GLLensFlare1: TGLLensFlare;
    MatLib: TGLMaterialLibrary;
    EarthCombiner: TGLTexCombineShader;
    CameraControler: TGLCamera;
    SkyDome: TGLSkyDome;
    ConstellationLines: TGLLines;
    ConstellationBorders: TGLLines;
    procedure FormCreate(Sender: TObject);
    procedure DirectOpenGLRender(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure Timer1Timer(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLSceneViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewerDblClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure GLSceneViewerBeforeRender(Sender: TObject);
  public
    ConstellationsAlpha: Single;
    TimeMultiplier: Single;
    mx, my, dmx, dmy: Integer;
    HighResResourcesLoaded: Boolean;
    CameraTimeSteps: Single;
    radius, invAtmosphereHeight: Single;
    sunPos, eyePos, lightingVector: TGLVector;
    diskNormal, diskRight, diskUp: TGLVector;
  private
    function LonLatToPos(lon, lat: Single): TAffineVector;
    procedure LoadConstellationLines;
    function AtmosphereColor(const rayStart, rayEnd: TGLVector): TGLColorVector;
    function ComputeColor(var rayDest: TGLVector; mayHitGround: Boolean): TGLColorVector;
  end;

var
  Form1: TForm1;

const
  cOpacity: Single = 5;
  // unrealisticly thick atmospheres look better :)
  cAtmosphereRadius: Single = 0.55;
  // use value slightly lower than actual radius, for antialiasing effect
  cPlanetRadius: Single = 0.495;
  cLowAtmColor: TGLColorVector = (X:1; Y:1; Z:1; W:1);
  cHighAtmColor: TGLColorVector = (X:0; Y:0; Z:1; W:1);
  cIntDivTable: array[2..20] of Single =
    (1 / 2, 1 / 3, 1 / 4, 1 / 5, 1 / 6, 1 / 7, 1 / 8, 1 / 9, 1 / 10,
    1 / 11, 1 / 12, 1 / 13, 1 / 14, 1 / 15, 1 / 16, 1 / 17, 1 / 18, 1 / 19, 1 / 20);

//-----------------------------------------
implementation
//-----------------------------------------

{$R *.dfm}

uses
// accurate movements left for later... or the astute reader
  USolarSystem;

procedure TForm1.FormCreate(Sender: TObject);
var
  FileName: String;
begin
  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  FileName := 'Data\Yale_BSC.stars';
  SkyDome.Bands.Clear;
  if FileExists(FileName) then
    SkyDome.Stars.LoadStarsFile(FileName);
  LoadConstellationLines;
  TimeMultiplier := 1;
end;

procedure TForm1.GLSceneViewerBeforeRender(Sender: TObject);
begin
  GLLensFlare1.PreRender(Sender as TGLSceneBuffer);
  // if no multitexturing or no combiner support, turn off city lights
  MatLib.Materials[0].Shader := EarthCombiner;
  MatLib.Materials[0].Texture2Name := 'earthNight';
end;

function TForm1.AtmosphereColor(const rayStart, rayEnd: TGLVector)
  : TGLColorVector;
var
  i, n: Integer;
  atmPoint, normal: TGLVector;
  altColor: TGLColorVector;
  alt, rayLength, contrib, decay, intensity, invN: Single;

begin
  Result := clrTransparent;
  rayLength := VectorDistance(rayStart, rayEnd);
  n := Round(3 * rayLength * invAtmosphereHeight) + 2;
  if n > 10 then
    n := 10;
  invN := cIntDivTable[n]; // 1/n;
  contrib := rayLength * invN * cOpacity;
  decay := 1 - contrib * 0.5;
  contrib := contrib * (1 / 1.1);
  for i := n - 1 downto 0 do
  begin
    VectorLerp(rayStart, rayEnd, i * invN, atmPoint);
    // diffuse lighting normal
    normal := VectorNormalize(atmPoint);
    // diffuse lighting intensity
    intensity := VectorDotProduct(normal, lightingVector) + 0.1;
    if PInteger(@intensity)^ > 0 then
    begin
      // sample on the lit side
      intensity := intensity * contrib;
      alt := (VectorLength(atmPoint) - cPlanetRadius) * invAtmosphereHeight;
      VectorLerp(cLowAtmColor, cHighAtmColor, alt, altColor);
      Result.X := Result.X * decay + altColor.X * intensity;
      Result.Y := Result.Y * decay + altColor.Y * intensity;
      Result.Z := Result.Z * decay + altColor.Z * intensity;
    end
    else
    begin
      // sample on the dark sid
      Result.X := Result.X * decay;
      Result.Y := Result.Y * decay;
      Result.Z := Result.Z * decay;
    end;
  end;
  Result.W := n * contrib * cOpacity * 0.1;
end;

//---------------------------------------------------------
//
function TForm1.ComputeColor(var rayDest: TGLVector; mayHitGround: Boolean): TGLColorVector;
var
  ai1, ai2, pi1, pi2: TGLVector;
  rayVector: TGLVector;
begin
  rayVector := VectorNormalize(VectorSubtract(rayDest, eyePos));
  if RayCastSphereIntersect(eyePos, rayVector, NullHmgPoint, cAtmosphereRadius,
    ai1, ai2) > 1 then
  begin
    // atmosphere hit
    if mayHitGround and (RayCastSphereIntersect(eyePos, rayVector, NullHmgPoint,
      cPlanetRadius, pi1, pi2) > 0) then
    begin
      // hit ground
      Result := AtmosphereColor(ai1, pi1);
    end
    else
    begin
      // through atmosphere only
      Result := AtmosphereColor(ai1, ai2);
    end;
    rayDest := ai1;
  end
  else
    Result := clrTransparent;
end;


procedure TForm1.DirectOpenGLRender(Sender: TObject; var rci: TGLRenderContextInfo);

const
  cSlices = 60;
var
  i, j, k0, k1: Integer;
  cosCache, sinCache: array[0..cSlices] of Single;
  pVertex, pColor: PVectorArray;
begin
  sunPos := LSSun.AbsolutePosition;
  eyePos := Camera.AbsolutePosition;

  diskNormal := VectorNegate(eyePos);
  NormalizeVector(diskNormal);
  diskRight := VectorCrossProduct(Camera.AbsoluteUp, diskNormal);
  NormalizeVector(diskRight);
  diskUp := VectorCrossProduct(diskNormal, diskRight);
  NormalizeVector(diskUp);

  invAtmosphereHeight := 1 / (cAtmosphereRadius - cPlanetRadius);
  lightingVector := VectorNormalize(sunPos); // sun at infinity
  PrepareSinCosCache(sinCache, cosCache, 0, 360);

  GetMem(pVertex, 2 * (cSlices + 1) * SizeOf(TGLVector));
  GetMem(pColor, 2 * (cSlices + 1) * SizeOf(TGLVector));

  rci.GLStates.DepthWriteMask := False;
  rci.GLStates.Disable(stLighting);
  rci.GLStates.Enable(stBlend);
  rci.GLStates.SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
  for i := 0 to 13 do
  begin
    if i < 5 then
      radius := cPlanetRadius * Sqrt(i * (1 / 5))
    else
      radius := cPlanetRadius + (i - 5.1) * (cAtmosphereRadius - cPlanetRadius) * (1 / 6.9);
    radius := SphereVisibleRadius(VectorLength(eyePos), radius);
    k0 := (i and 1) * (cSlices + 1);
    k1 := (cSlices + 1) - k0;
    for j := 0 to cSlices do
    begin
      VectorCombine(diskRight, diskUp, cosCache[j] * radius, sinCache[j] * radius,
        pVertex[k0 + j]);
      if i < 13 then
        pColor[k0 + j] := ComputeColor(pVertex[k0 + j], i <= 7);
      if i = 0 then
        Break;
    end;

    if i > 1 then
    begin
      if i = 13 then
      begin
        glBegin(GL_QUAD_STRIP);
        for j := cSlices downto 0 do
        begin
          glColor4fv(@pColor[k1 + j]);
          glVertex3fv(@pVertex[k1 + j]);
          glColor4fv(@clrTransparent);
          glVertex3fv(@pVertex[k0 + j]);
        end;
        glEnd;
      end
      else
      begin
        glBegin(GL_QUAD_STRIP);
        for j := cSlices downto 0 do
        begin
          glColor4fv(@pColor[k1 + j]);
          glVertex3fv(@pVertex[k1 + j]);
          glColor4fv(@pColor[k0 + j]);
          glVertex3fv(@pVertex[k0 + j]);
        end;
        glEnd;
      end;
    end
    else if i = 1 then
    begin
      glBegin(GL_TRIANGLE_FAN);
      glColor4fv(@pColor[k1]);
      glVertex3fv(@pVertex[k1]);
      for j := k0 + cSlices downto k0 do
      begin
        glColor4fv(@pColor[j]);
        glVertex3fv(@pVertex[j]);
      end;
      glEnd;
    end;
  end;
  rci.GLStates.DepthWriteMask := True;

  FreeMem(pVertex);
  FreeMem(pColor);
end;

function TForm1.LonLatToPos(lon, lat: Single): TAffineVector;
var
  f: Single;
begin
  SinCosine(lat * (PI / 180), Result.Y, f);
  SinCosine(lon * (360 / 24 * PI / 180), f, Result.X, Result.Z);
end;

// -------------------------------------------
//
procedure TForm1.LoadConstellationLines;
var
  sl, line: TStrings;
  pos1, pos2: TAffineVector;

var
  i: Integer;
begin
  sl := TStringList.Create;
  line := TStringList.Create;
  sl.LoadFromFile('Data\Constellations.dat');
///  sl.LoadFromFile('Data\Constellation_borders.dat');
  for i := 0 to sl.Count - 1 do
  begin
    line.CommaText := sl[i];
    pos1 := LonLatToPos(StrToFloatDef(line[0], 0), StrToFloatDef(line[1], 0));
    ConstellationLines.AddNode(pos1);
///     ConstellationBorders.AddNode(pos1);
    pos2 := LonLatToPos(StrToFloatDef(line[2], 0), StrToFloatDef(line[3], 0));
    ConstellationLines.AddNode(pos2);
///    ConstellationBorders.AddNode(pos2);
  end;
  sl.Free;
  line.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('Earth ' + '%.1f FPS', [GLSceneViewer.FramesPerSecond]);
  GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TForm1.CadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
//var
//   d : Double;
//   p : TAffineVector;
begin
  //   d:=GMTDateTimeToJulianDay(Now-2+newTime*timeMultiplier);
  // make earth rotate
  SPEarth.TurnAngle := SPEarth.TurnAngle + deltaTime * timeMultiplier;
  {   p:=ComputePlanetPosition(cSunOrbitalElements, d);
     ScaleVector(p, 0.5*cAUToKilometers*(1/cEarthRadius));
     LSSun.Position.AsAffineVector:=p; }

  // moon rotates on itself and around earth (not sure about the rotation direction!)

  {   p:=ComputePlanetPosition(cMoonOrbitalElements, d);
     ScaleVector(p, 0.5*cAUToKilometers*(1/cEarthRadius)); }

  DCMoon.TurnAngle := DCMoon.TurnAngle + deltaTime * timeMultiplier / 29.5;
  SPMoon.TurnAngle := 180 - DCMoon.TurnAngle;

  // honour camera movements
  if (dmy <> 0) or (dmx <> 0) then
  begin
    CameraControler.MoveAroundTarget(ClampValue(dmy * 0.3, -5, 5),
      ClampValue(dmx * 0.3, -5, 5));
    dmx := 0;
    dmy := 0;
  end;
  // this gives us smoother camera movements
  cameraTimeSteps := cameraTimeSteps + deltaTime;
  while cameraTimeSteps > 0.005 do
  begin
    Camera.Position.AsVector := VectorLerp(Camera.Position.AsVector,
      CameraControler.Position.AsVector, 0.05);
    CameraTimeSteps := CameraTimeSteps - 0.005;
  end;
  // smooth constellation appearance/disappearance
  if ConstellationLines.LineColor.Alpha <> constellationsAlpha then
  begin
    ConstellationLines.LineColor.Alpha :=
      ClampValue(ConstellationLines.LineColor.Alpha + Sign(constellationsAlpha -
                 ConstellationLines.LineColor.Alpha) * deltaTime, 0, 0.5);
    ConstellationLines.Visible := (ConstellationLines.LineColor.Alpha > 0);
  end;
end;

procedure TForm1.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := x;
  my := y;
end;

procedure TForm1.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    dmx := dmx + (mx - x);
    dmy := dmy + (my - y);
  end
  else if Shift = [ssRight] then
    Camera.FocalLength := Camera.FocalLength * Power(1.05, (my - y) * 0.1);
  mx := x;
  my := y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  f: Single;
begin
  if (WheelDelta > 0) or (CameraControler.Position.VectorLength > 0.90) then
  begin
    f := Power(1.05, WheelDelta * (1 / 120));
    CameraControler.AdjustDistanceToTarget(f);
  end;
  Handled := True;
end;

procedure TForm1.GLSceneViewerDblClick(Sender: TObject);
begin
  GLSceneViewer.OnMouseMove := nil;
  if WindowState = wsMaximized then
  begin
    WindowState := wsNormal;
    BorderStyle := bsSizeToolWin;
  end
  else
  begin
    BorderStyle := bsNone;
    WindowState := wsMaximized;
  end;
  GLSceneViewer.OnMouseMove := GLSceneViewerMouseMove;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);

  procedure LoadHighResTexture(LibMat: TGLLibMaterial; const FileName: string);
  begin
    if FileExists(fileName) then
    begin
      LibMat.Material.Texture.Compression := tcStandard;
      LibMat.Material.Texture.Image.LoadFromFile(fileName);
    end;
  end;

begin
  case Key of
    #27: Close;
    'm', 'M':
      begin
        Camera.MoveTo(SPMoon);
        CameraControler.MoveTo(SPMoon);
        Camera.TargetObject := SPMoon;
        CameraControler.TargetObject := SPMoon;
      end;
    'e', 'E':
      begin
        Camera.MoveTo(dcEarth);
        CameraControler.MoveTo(dcEarth);
        Camera.TargetObject := dcEarth;
        CameraControler.TargetObject := dcEarth;
      end;
    'h': if not highResResourcesLoaded then
      begin
        GLSceneViewer.Cursor := crHourGlass;
        try
          if DirectoryExists('Data') then
            ChDir('Data');
          with MatLib do
          begin
            LoadHighResTexture(Materials[0], 'land_ocean_ice_4096.jpg');
            LoadHighResTexture(Materials[1], 'land_ocean_ice_lights_4096.jpg');
            LoadHighResTexture(Materials[2], 'moon_2048.jpg');
          end;
          if FileExists('Hipparcos_9.0.stars') then
          begin
            SkyDome.Stars.Clear;
            SkyDome.Stars.LoadStarsFile('Hipparcos_9.0.stars');
            SkyDome.StructureChanged;
          end;
          GLSceneViewer.Buffer.AntiAliasing := aa2x;
        finally
          GLSceneViewer.Cursor := crDefault;
        end;
        highResResourcesLoaded := True;
      end;
    'c': constellationsAlpha := 0.5 - constellationsAlpha;
    '0'..'9': timeMultiplier := Power(Integer(Key) - Integer('0'), 3);
  end;
end;

end.
