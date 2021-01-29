//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.TexLensFlare;

(* Texture-based Lens flare object. *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.VectorGeometry,
  GLS.Objects,
  GLS.Texture,
  GLS.OpenGLTokens,
  GLS.Context,
  GLS.RenderContextInfo,
  GLS.BaseClasses,
  GLS.State,
  GLS.VectorTypes;

type

  TGLTextureLensFlare = class(TGLBaseSceneObject)
  private
    FSize: integer;
    FCurrSize: Single;
    FNumSecs: integer;
    FAutoZTest: boolean;
    //used for internal calculation
    FDeltaTime: Double;
    FImgSecondaries: TGLTexture;
    FImgRays: TGLTexture;
    FImgRing: TGLTexture;
    FImgGlow: TGLTexture;
    FSeed: Integer;
    procedure SetImgGlow(const Value: TGLTexture);
    procedure SetImgRays(const Value: TGLTexture);
    procedure SetImgRing(const Value: TGLTexture);
    procedure SetImgSecondaries(const Value: TGLTexture);
    procedure SetSeed(const Value: Integer);
  protected
    procedure SetSize(aValue: integer);
    procedure SetNumSecs(aValue: integer);
    procedure SetAutoZTest(aValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure DoProgress(const progressTime: TGLProgressTimes); override;
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
    property ImgGlow: TGLTexture read FImgGlow write SetImgGlow;
    property ImgRays: TGLTexture read FImgRays write SetImgRays;
    property ImgRing: TGLTexture read FImgRing write SetImgRing;
    property ImgSecondaries: TGLTexture read FImgSecondaries write SetImgSecondaries;
    property ObjectsSorting;
    property Position;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

//==========================================================
implementation

// ------------------
// ------------------ TGLTextureLensFlare ------------------
// ------------------

constructor TGLTextureLensFlare.Create(AOwner: TComponent);
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

  FImgRays := TGLTexture.Create(Self);
  FImgSecondaries := TGLTexture.Create(Self);
  FImgRing := TGLTexture.Create(Self);
  FImgGlow := TGLTexture.Create(Self);
end;

procedure TGLTextureLensFlare.SetSize(aValue: integer);
begin
  if FSize <> aValue then
  begin
    FSize := aValue;
    FCurrSize := FSize;
    StructureChanged;
  end;
end;

procedure TGLTextureLensFlare.SetNumSecs(aValue: integer);
begin
  if FNumSecs <> aValue then
  begin
    FNumSecs := aValue;
    StructureChanged;
  end;
end;


procedure TGLTextureLensFlare.SetAutoZTest(aValue: boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;


procedure TGLTextureLensFlare.BuildList(var rci: TGLRenderContextInfo);
var
  v, rv, screenPos, posVector: TAffineVector;
  depth, rnd: Single;
  flag: Boolean;
  i: Integer;
  CurrentBuffer: TGLSceneBuffer;
begin
  CurrentBuffer := TGLSceneBuffer(rci.buffer);
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
  gl.MatrixMode(GL_MODELVIEW);
  gl.PushMatrix;
  gl.LoadMatrixf(@CurrentBuffer.BaseProjectionMatrix);

  gl.MatrixMode(GL_PROJECTION);
  gl.PushMatrix;
  gl.LoadIdentity;
  gl.Scalef(2 / rci.viewPortSize.cx, 2 / rci.viewPortSize.cy, 1);

  rci.GLStates.Disable(stLighting);
  rci.GLStates.Disable(stDepthTest);
  rci.GLStates.Enable(stBlend);
  rci.GLStates.SetBlendFunc(bfOne, bfOne);

  //Rays and Glow on Same Position
  gl.PushMatrix;
  gl.Translatef(posVector.X, posVector.Y, posVector.Z);

  if not ImgGlow.Disabled and Assigned(ImgGlow.Image) then
  begin
    ImgGlow.Apply(rci);
    gl.begin_(GL_QUADS);
    gl.TexCoord2f(0, 0);
    gl.Vertex3f(-FCurrSize, -FCurrSize, 0);
    gl.TexCoord2f(1, 0);
    gl.Vertex3f(FCurrSize, -FCurrSize, 0);
    gl.TexCoord2f(1, 1);
    gl.Vertex3f(FCurrSize, FCurrSize, 0);
    gl.TexCoord2f(0, 1);
    gl.Vertex3f(-FCurrSize, FCurrSize, 0);
    gl.end_;
    ImgGlow.UnApply(rci);
  end;

  if not ImgRays.Disabled and Assigned(ImgRays.Image) then
  begin
    ImgRays.Apply(rci);
    gl.begin_(GL_QUADS);
    gl.TexCoord2f(0, 0);
    gl.Vertex3f(-FCurrSize, -FCurrSize, 0);
    gl.TexCoord2f(1, 0);
    gl.Vertex3f(FCurrSize, -FCurrSize, 0);
    gl.TexCoord2f(1, 1);
    gl.Vertex3f(FCurrSize, FCurrSize, 0);
    gl.TexCoord2f(0, 1);
    gl.Vertex3f(-FCurrSize, FCurrSize, 0);
    gl.end_;
    ImgRays.UnApply(rci);
  end;
  gl.PopMatrix;

  if not ImgRing.Disabled and Assigned(ImgRing.Image) then
  begin
    gl.PushMatrix;
    gl.Translatef(posVector.X * 1.1, posVector.Y * 1.1, posVector.Z);
    ImgRing.Apply(rci);
    gl.begin_(GL_QUADS);
    gl.TexCoord2f(0, 0);
    gl.Vertex3f(-FCurrSize, -FCurrSize, 0);
    gl.TexCoord2f(1, 0);
    gl.Vertex3f(FCurrSize, -FCurrSize, 0);
    gl.TexCoord2f(1, 1);
    gl.Vertex3f(FCurrSize, FCurrSize, 0);
    gl.TexCoord2f(0, 1);
    gl.Vertex3f(-FCurrSize, FCurrSize, 0);
    gl.end_;
    ImgRing.UnApply(rci);
    gl.PopMatrix;
  end;

  if not ImgSecondaries.Disabled and Assigned(ImgSecondaries.Image) then
  begin
    RandSeed := FSeed;
    gl.PushMatrix;
    ImgSecondaries.Apply(rci);
    for i := 1 to FNumSecs do
    begin
      rnd := 2 * Random - 1;
      v := PosVector;
      if rnd < 0 then
        ScaleVector(V, rnd)
      else
        ScaleVector(V, 0.8 * rnd);
      gl.PushMatrix;
      gl.Translatef(v.X, v.Y, v.Z);

      rnd := random * 0.5 + 0.1;
      gl.begin_(GL_QUADS);
      gl.TexCoord2f(0, 0);
      gl.Vertex3f(-FCurrSize * rnd, -FCurrSize * rnd, 0);
      gl.TexCoord2f(1, 0);
      gl.Vertex3f(FCurrSize * rnd, -FCurrSize * rnd, 0);
      gl.TexCoord2f(1, 1);
      gl.Vertex3f(FCurrSize * rnd, FCurrSize * rnd, 0);
      gl.TexCoord2f(0, 1);
      gl.Vertex3f(-FCurrSize * rnd, FCurrSize * rnd, 0);
      gl.end_;
      gl.PopMatrix
    end;
    ImgSecondaries.UnApply(rci);
    gl.PopMatrix;
  end;

  // restore state

  gl.PopMatrix;
  gl.MatrixMode(GL_MODELVIEW);
  gl.PopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;


procedure TGLTextureLensFlare.DoProgress(const progressTime: TGLProgressTimes);
begin
  FDeltaTime := progressTime.deltaTime;
  inherited;
end;

procedure TGLTextureLensFlare.SetImgGlow(const Value: TGLTexture);
begin
  FImgGlow.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgRays(const Value: TGLTexture);
begin
  FImgRays.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgRing(const Value: TGLTexture);
begin
  FImgRing.Assign(Value);
  StructureChanged;
end;

procedure TGLTextureLensFlare.SetImgSecondaries(const Value: TGLTexture);
begin
  FImgSecondaries.Assign(Value);
  StructureChanged;
end;

destructor TGLTextureLensFlare.Destroy;
begin
  FImgRays.Free;
  FImgSecondaries.Free;
  FImgRing.Free;
  FImgGlow.Free;
  inherited;
end;

procedure TGLTextureLensFlare.SetSeed(const Value: Integer);
begin
  FSeed := Value;
  StructureChanged;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TGLTextureLensFlare]);

end.


