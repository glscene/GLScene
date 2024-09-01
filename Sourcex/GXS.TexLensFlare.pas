//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.TexLensFlare;

(* Texture-based Lens flare object *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,

  System.Classes,

  GXS.Scene,
  GXS.VectorGeometry,
  GXS.PersistentClasses,
  GXS.Objects,
  GXS.Texture,
  GXS.Context,
  GXS.RenderContextInfo,
  GXS.BaseClasses,
  GXS.State,
  GXS.VectorTypes;

type

  TgxTextureLensFlare = class(TgxBaseSceneObject)
  private
    FSize: integer;
    FCurrSize: Single;
    FNumSecs: integer;
    FAutoZTest: boolean;
    //used for internal calculation
    FDeltaTime: Double;
    FImgSecondaries: TgxTexture;
    FImgRays: TgxTexture;
    FImgRing: TgxTexture;
    FImgGlow: TgxTexture;
    FSeed: Integer;
    procedure SetImgGlow(const Value: TgxTexture);
    procedure SetImgRays(const Value: TgxTexture);
    procedure SetImgRing(const Value: TgxTexture);
    procedure SetImgSecondaries(const Value: TgxTexture);
    procedure SetSeed(const Value: Integer);
  protected
    procedure SetSize(aValue: integer);
    procedure SetNumSecs(aValue: integer);
    procedure SetAutoZTest(aValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
  published
    // MaxRadius of the flare.
    property Size: integer read FSize write SetSize default 50;
    // Random seed
    property Seed: Integer read FSeed write SetSeed;
    // Number of secondary flares.
    property NumSecs: integer read FNumSecs write SetNumSecs default 8;
    // Number of segments used when rendering circles.
    //property Resolution: integer read FResolution write SetResolution default 64;
    property AutoZTest: boolean read FAutoZTest write SetAutoZTest default True;
    // The Textures
    property ImgGlow: TgxTexture read FImgGlow write SetImgGlow;
    property ImgRays: TgxTexture read FImgRays write SetImgRays;
    property ImgRing: TgxTexture read FImgRing write SetImgRing;
    property ImgSecondaries: TgxTexture read FImgSecondaries write SetImgSecondaries;
    property ObjectsSorting;
    property Position;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

//------------------------------------------------------------------
implementation
//------------------------------------------------------------------

// ------------------
// ------------------ TgxTextureLensFlare ------------------
// ------------------

constructor TgxTextureLensFlare.Create(AOwner: TComponent);
begin
  inherited;
  Randomize;
  FSeed := Random(2000) + 465;

  // Set default parameters:
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FSize := 50;
  FCurrSize := FSize;
  FNumSecs := 8;
  FAutoZTest := True;

  FImgRays := TgxTexture.Create(Self);
  FImgSecondaries := TgxTexture.Create(Self);
  FImgRing := TgxTexture.Create(Self);
  FImgGlow := TgxTexture.Create(Self);
end;

procedure TgxTextureLensFlare.SetSize(aValue: integer);
begin
  if FSize <> aValue then
  begin
    FSize := aValue;
    FCurrSize := FSize;
    StructureChanged;
  end;
end;

procedure TgxTextureLensFlare.SetNumSecs(aValue: integer);
begin
  if FNumSecs <> aValue then
  begin
    FNumSecs := aValue;
    StructureChanged;
  end;
end;

procedure TgxTextureLensFlare.SetAutoZTest(aValue: boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;

procedure TgxTextureLensFlare.BuildList(var rci: TgxRenderContextInfo);
var
  v, rv, screenPos, posVector: TAffineVector;
  depth, rnd: Single;
  flag: Boolean;
  i: Integer;
  CurrentBuffer: TgxSceneBuffer;
begin
  CurrentBuffer := TgxSceneBuffer(rci.buffer);
  SetVector(v, AbsolutePosition);
  // are we looking towards the flare?
  rv := VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
  if VectorDotProduct(rci.cameraDirection, rv) > 0 then
  begin
    // find out where it is on the screen.
    screenPos := CurrentBuffer.WorldToScreen(v);
    if (screenPos.X < rci.viewPortSize.cx) and (screenPos.X >= 0)
      and (screenPos.Y < rci.viewPortSize.cy) and (screenPos.Y >= 0) then
    begin
      if FAutoZTest then
      begin
        depth := CurrentBuffer.GetPixelDepth(Round(ScreenPos.X),
          Round(rci.viewPortSize.cy - ScreenPos.Y));
        // but is it behind something?
        if screenPos.Z >= 1 then
          flag := (depth >= 1)
        else
          flag := (depth >= screenPos.Z);
      end
      else
        flag := True;
    end
    else
      flag := False;
  end
  else
    flag := False;

  MakeVector(posVector,
    screenPos.X - rci.viewPortSize.cx / 2,
    screenPos.Y - rci.viewPortSize.cy / 2, 0);

  // make the glow appear/disappear progressively

  if Flag then
    if FCurrSize < FSize then
      FCurrSize := FCurrSize + FDeltaTime * 200 {FSize * 4};
  if not Flag then
    if FCurrSize > 0 then
      FCurrSize := FCurrSize - FDeltaTime * 200 {FSize * 4};
  if FCurrSize <= 0 then
    Exit;

  // Prepare matrices
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadMatrixf(@CurrentBuffer.BaseProjectionMatrix);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glScalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);

  rci.gxStates.Disable(stLighting);
  rci.gxStates.Disable(stDepthTest);
  rci.gxStates.Enable(stBlend);
  rci.gxStates.SetBlendFunc(bfOne, bfOne);

  //Rays and Glow on Same Position
  glPushMatrix;
  glTranslatef(posVector.X, posVector.Y, posVector.Z);

  if not ImgGlow.Disabled and Assigned(ImgGlow.Image) then
  begin
    ImgGlow.Apply(rci);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex3f(-FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 0);
    glVertex3f(FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 1);
    glVertex3f(FCurrSize, FCurrSize, 0);
    glTexCoord2f(0, 1);
    glVertex3f(-FCurrSize, FCurrSize, 0);
    glEnd;
    ImgGlow.UnApply(rci);
  end;

  if not ImgRays.Disabled and Assigned(ImgRays.Image) then
  begin
    ImgRays.Apply(rci);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex3f(-FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 0);
    glVertex3f(FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 1);
    glVertex3f(FCurrSize, FCurrSize, 0);
    glTexCoord2f(0, 1);
    glVertex3f(-FCurrSize, FCurrSize, 0);
    glEnd;
    ImgRays.UnApply(rci);
  end;
  glPopMatrix;

  if not ImgRing.Disabled and Assigned(ImgRing.Image) then
  begin
    glPushMatrix;
    glTranslatef(posVector.X * 1.1, posVector.Y * 1.1, posVector.Z);
    ImgRing.Apply(rci);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 0);
    glVertex3f(-FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 0);
    glVertex3f(FCurrSize, -FCurrSize, 0);
    glTexCoord2f(1, 1);
    glVertex3f(FCurrSize, FCurrSize, 0);
    glTexCoord2f(0, 1);
    glVertex3f(-FCurrSize, FCurrSize, 0);
    glEnd;
    ImgRing.UnApply(rci);
    glPopMatrix;
  end;

  if not ImgSecondaries.Disabled and Assigned(ImgSecondaries.Image) then
  begin
    RandSeed := FSeed;
    glPushMatrix;
    ImgSecondaries.Apply(rci);
    for i := 1 to FNumSecs do
    begin
      rnd := 2 * Random - 1;
      v := PosVector;
      if rnd < 0 then
        ScaleVector(V, rnd)
      else
        ScaleVector(V, 0.8 * rnd);
      glPushMatrix;
      glTranslatef(v.X, v.Y, v.Z);

      rnd := random * 0.5 + 0.1;
      glBegin(GL_QUADS);
      glTexCoord2f(0, 0);
      glVertex3f(-FCurrSize * rnd, -FCurrSize * rnd, 0);
      glTexCoord2f(1, 0);
      glVertex3f(FCurrSize * rnd, -FCurrSize * rnd, 0);
      glTexCoord2f(1, 1);
      glVertex3f(FCurrSize * rnd, FCurrSize * rnd, 0);
      glTexCoord2f(0, 1);
      glVertex3f(-FCurrSize * rnd, FCurrSize * rnd, 0);
      glEnd;
      glPopMatrix
    end;
    ImgSecondaries.UnApply(rci);
    glPopMatrix;
  end;

  // restore state

  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TgxTextureLensFlare.DoProgress(const progressTime: TgxProgressTimes);
begin
  FDeltaTime := progressTime.deltaTime;
  inherited;
end;

procedure TgxTextureLensFlare.SetImgGlow(const Value: TgxTexture);
begin
  FImgGlow.Assign(Value);
  StructureChanged;
end;

procedure TgxTextureLensFlare.SetImgRays(const Value: TgxTexture);
begin
  FImgRays.Assign(Value);
  StructureChanged;
end;

procedure TgxTextureLensFlare.SetImgRing(const Value: TgxTexture);
begin
  FImgRing.Assign(Value);
  StructureChanged;
end;

procedure TgxTextureLensFlare.SetImgSecondaries(const Value: TgxTexture);
begin
  FImgSecondaries.Assign(Value);
  StructureChanged;
end;

destructor TgxTextureLensFlare.Destroy;
begin
  FImgRays.Free;
  FImgSecondaries.Free;
  FImgRing.Free;
  FImgGlow.Free;
  inherited;
end;

procedure TgxTextureLensFlare.SetSeed(const Value: Integer);
begin
  FSeed := Value;
  StructureChanged;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TgxTextureLensFlare]);

end.

