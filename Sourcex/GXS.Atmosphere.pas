//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Atmosphere;

(*
   This unit contains classes that imitate an atmosphere around a planet.

   Comments:
      1) Eats a lot of CPU (reduces FPS from 1240 to 520 on my PC with cSlices=100)
      2) Alpha in LowAtmColor, HighAtmColor is ignored.
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,

  GXS.Scene,
  GXS.Objects,
  GXS.Cadencer,
  GXS.VectorGeometry,
  GXS.Context,
  GXS.Strings,
  GXS.Color,
  GXS.RenderContextInfo,
  GXS.State,
  GXS.VectorTypes;

type
   EGLAtmosphereException = class(Exception);

   (* With aabmOneMinusSrcAlpha atmosphere is transparent to other objects,
   but has problems, which are best seen when the Atmosphere radius is big.
   With bmOneMinusDstColor atmosphere doesn't have these problems, but offers
   limited transparency (when you look closely on the side). *)
  TgxAtmosphereBlendingMode = (abmOneMinusDstColor, abmOneMinusSrcAlpha);

  (* This class imitates an atmosphere around a planet. *)
  TgxCustomAtmosphere = class(TgxBaseSceneObject)
  private
    // Used in DoRenderl
    cosCache, sinCache: array of Single;
    pVertex, pColor: PVectorArray;
    FSlices: Integer;
    FBlendingMode: TgxAtmosphereBlendingMode;
    FPlanetRadius: Single;
    FAtmosphereRadius: Single;
    FOpacity: Single;
    FLowAtmColor: TgxColor;
    FHighAtmColor: TgxColor;
    FSun: TgxBaseSceneObject;
    procedure SetSun(const Value: TgxBaseSceneObject);
    procedure SetAtmosphereRadius(const Value: Single);
    procedure SetPlanetRadius(const Value: Single);
    procedure EnableGLBlendingMode(StateCache: TgxStateCache);
    function StoreAtmosphereRadius: Boolean;
    function StoreOpacity: Boolean;
    function StorePlanetRadius: Boolean;
    procedure SetSlices(const Value: Integer);
    procedure SetLowAtmColor(const AValue: TgxColor);
    procedure SetHighAtmColor(const AValue: TgxColor);
    function StoreLowAtmColor: Boolean;
    function StoreHighAtmColor: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Sun: TgxBaseSceneObject read FSun write SetSun;
    property Slices: Integer read FSlices write SetSlices default 60;
    property Opacity: Single read FOpacity write FOpacity stored StoreOpacity;
    // AtmosphereRadius > PlanetRadius!!!
    property AtmosphereRadius: Single read FAtmosphereRadius write SetAtmosphereRadius stored StoreAtmosphereRadius;
    property PlanetRadius: Single read FPlanetRadius write SetPlanetRadius stored StorePlanetRadius;
    // Use value slightly lower than actual radius, for antialiasing effect.
    property LowAtmColor: TgxColor read FLowAtmColor write SetLowAtmColor stored StoreLowAtmColor;
    property HighAtmColor: TgxColor read FHighAtmColor write SetHighAtmColor stored StoreHighAtmColor;
    property BlendingMode: TgxAtmosphereBlendingMode read FBlendingMode
                               write FBlendingMode default abmOneMinusSrcAlpha;
    procedure SetOptimalAtmosphere(const ARadius: Single);  //absolute
    procedure SetOptimalAtmosphere2(const ARadius: Single); //relative
    procedure TogleBlendingMode; //changes between 2 blending modes
    // Standard component stuff.
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Main rendering procedure.
    procedure DoRender(var rci: TgxRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    // Used to determine extents.
    function AxisAlignedDimensionsUnscaled : TVector4f; override;
  end;

  TgxAtmosphere = class(TgxCustomAtmosphere)
  published
    property Sun;
    property Slices;
    property Opacity;
    property AtmosphereRadius;
    property PlanetRadius;
    property LowAtmColor;
    property HighAtmColor;
    property BlendingMode;
    property Position;
    property ObjectsSorting;
    property ShowAxes;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

//---------------------------------------------------------------------
implementation
//---------------------------------------------------------------------

const
  EPS = 0.0001;
  cIntDivTable: array [2..20] of Single =
    (1 / 2, 1 / 3, 1 / 4, 1 / 5, 1 / 6, 1 / 7, 1 / 8, 1 / 9, 1 / 10,
    1 / 11, 1 / 12, 1 / 13, 1 / 14, 1 / 15, 1 / 16, 1 / 17, 1 / 18, 1 / 19, 1 / 20);

procedure TgxCustomAtmosphere.SetOptimalAtmosphere(const ARadius: Single);
begin
  FAtmosphereRadius := ARadius + 0.25;
  FPlanetRadius := ARadius - 0.07;
end;


procedure TgxCustomAtmosphere.SetOptimalAtmosphere2(const ARadius: Single);
begin
  FAtmosphereRadius := ARadius + ARadius / 15;
  FPlanetRadius := ARadius - ARadius / 50;
end;

constructor TgxCustomAtmosphere.Create(AOwner: TComponent);
begin
  inherited;
  FLowAtmColor := TgxColor.Create(Self);
  FHighAtmColor := TgxColor.Create(Self);

  FOpacity := 2.1;
  SetSlices(60);
  FAtmosphereRadius := 3.55;
  FPlanetRadius := 3.395;
  FLowAtmColor.Color := VectorMake(1, 1, 1, 1);
  FHighAtmColor.Color := VectorMake(0, 0, 1, 1);

  FBlendingMode := abmOneMinusSrcAlpha;
end;


destructor TgxCustomAtmosphere.Destroy;
begin
  FLowAtmColor.Free;
  FHighAtmColor.Free;
  FreeMem(pVertex);
  FreeMem(pColor);
  inherited;
end;


procedure TgxCustomAtmosphere.DoRender(var rci: TgxRenderContextInfo; renderSelf, renderChildren: Boolean);
var
  radius, invAtmosphereHeight:    Single;
  sunPos, eyePos, lightingVector: TVector4f;
  diskNormal, diskRight, diskUp:  TVector4f;


  function AtmosphereColor(const rayStart, rayEnd: TVector4f): TgxColorVector;
  var
    I, n:     Integer;
    atmPoint, normal: TVector4f;
    altColor: TgxColorVector;
    alt, rayLength, contrib, decay, intensity, invN: Single;
  begin
    Result := clrTransparent;
    rayLength := VectorDistance(rayStart, rayEnd);
    n := Round(3 * rayLength * invAtmosphereHeight) + 2;
    if n > 10 then
      n := 10;
    invN := cIntDivTable[n];//1/n;
    contrib := rayLength * invN * Opacity;
    decay := 1 - contrib * 0.5;
    contrib := contrib * (1 / 1.1);
    for I := n - 1 downto 0 do
    begin
      VectorLerp(rayStart, rayEnd, I * invN, atmPoint);
      // diffuse lighting normal
      normal := VectorNormalize(atmPoint);
      // diffuse lighting intensity
      intensity := VectorDotProduct(normal, lightingVector) + 0.1;
      if PInteger(@intensity)^ > 0 then
      begin
        // sample on the lit side
        intensity := intensity * contrib;
        alt := (VectorLength(atmPoint) - FPlanetRadius) * invAtmosphereHeight;
        VectorLerp(LowAtmColor.Color, HighAtmColor.Color, alt, altColor);
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
    Result.W := n * contrib * Opacity * 0.1;
  end;


  function ComputeColor(var rayDest: TVector4f; mayHitGround: Boolean): TgxColorVector;
  var
    ai1, ai2, pi1, pi2: TVector4f;
    rayVector: TVector4f;
  begin
    rayVector := VectorNormalize(VectorSubtract(rayDest, eyePos));
    if RayCastSphereIntersect(eyePos, rayVector, NullHmgPoint,
      FAtmosphereRadius, ai1, ai2) > 1 then
    begin
      // atmosphere hit
      if mayHitGround and (RayCastSphereIntersect(eyePos, rayVector,
        NullHmgPoint, FPlanetRadius, pi1, pi2) > 0) then
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

var
  I, J, k0, k1:    Integer;
begin
  if FSun <> nil then
  begin
    Assert(FAtmosphereRadius > FPlanetRadius);

    sunPos := VectorSubtract(FSun.AbsolutePosition, AbsolutePosition);
    eyepos := VectorSubtract(rci.CameraPosition, AbsolutePosition);

    diskNormal := VectorNegate(eyePos);
    NormalizeVector(diskNormal);
    diskRight := VectorCrossProduct(rci.CameraUp, diskNormal);
    NormalizeVector(diskRight);
    diskUp := VectorCrossProduct(diskNormal, diskRight);
    NormalizeVector(diskUp);

    invAtmosphereHeight := 1 / (FAtmosphereRadius - FPlanetRadius);
    lightingVector := VectorNormalize(sunPos); // sun at infinity

    rci.gxStates.DepthWriteMask := False;
    rci.gxStates.Disable(stLighting);
    rci.gxStates.Enable(stBlend);
    EnableGLBlendingMode(rci.gxStates);
    for I := 0 to 13 do
    begin
      if I < 5 then
        radius := FPlanetRadius * Sqrt(I * (1 / 5))
      else
        radius := FPlanetRadius + (I - 5.1) * (FAtmosphereRadius - FPlanetRadius) * (1 / 6.9);
      radius := SphereVisibleRadius(VectorLength(eyePos), radius);
      k0 := (I and 1) * (FSlices + 1);
      k1 := (FSlices + 1) - k0;
      for J := 0 to FSlices do
      begin
        VectorCombine(diskRight, diskUp,
          cosCache[J] * radius, sinCache[J] * radius,
          pVertex[k0 + J]);
        if I < 13 then
          pColor[k0 + J] := ComputeColor(pVertex[k0 + J], I <= 7);
        if I = 0 then
          Break;
      end;

      if I > 1 then
      begin
        if I = 13 then
        begin
          // GL.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
          glBegin(GL_QUAD_STRIP);
          for J := FSlices downto 0 do
          begin
            glColor4fv(@pColor[k1 + J]);
            glVertex3fv(@pVertex[k1 + J]);
            glColor4fv(@clrTransparent);
            glVertex3fv(@pVertex[k0 + J]);
          end;
          glEnd;
        end
        else
        begin
          // glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_DST_COLOR);
          glBegin(GL_QUAD_STRIP);
          for J := FSlices downto 0 do
          begin
            glColor4fv(@pColor[k1 + J]);
            glVertex3fv(@pVertex[k1 + J]);
            glColor4fv(@pColor[k0 + J]);
            glVertex3fv(@pVertex[k0 + J]);
          end;
          glEnd;
        end;
      end
      else if I = 1 then
      begin
        //GL.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glBegin(GL_TRIANGLE_FAN);
        glColor4fv(@pColor[k1]);
        glVertex3fv(@pVertex[k1]);
        for J := k0 + FSlices downto k0 do
        begin
          glColor4fv(@pColor[J]);
          glVertex3fv(@pVertex[J]);
        end;
        glEnd;
      end;
    end;
  end;
  inherited;
end;

procedure TgxCustomAtmosphere.TogleBlendingMode;
begin
  if FBlendingMode = abmOneMinusSrcAlpha then
    FBlendingMode := abmOneMinusDstColor
  else
    FBlendingMode := abmOneMinusSrcAlpha;
end;

procedure TgxCustomAtmosphere.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TgxCustomAtmosphere then
  begin
    SetSlices(TgxCustomAtmosphere(Source).FSlices);
    FOpacity := TgxCustomAtmosphere(Source).FOpacity;
    FAtmosphereRadius := TgxCustomAtmosphere(Source).FAtmosphereRadius;
    FPlanetRadius := TgxCustomAtmosphere(Source).FPlanetRadius;
    FLowAtmColor.Color := TgxCustomAtmosphere(Source).FLowAtmColor.Color;
    FHighAtmColor.Color := TgxCustomAtmosphere(Source).FHighAtmColor.Color;
    FBlendingMode := TgxCustomAtmosphere(Source).FBlendingMode;
    SetSun(TgxCustomAtmosphere(Source).FSun);
  end;
end;

procedure TgxCustomAtmosphere.SetSun(const Value: TgxBaseSceneObject);
begin
  if FSun <> nil then FSun.RemoveFreeNotification(Self);
  FSun := Value;
  if FSun <> nil then FSun.FreeNotification(Self);
end;

function TgxCustomAtmosphere.AxisAlignedDimensionsUnscaled : TVector4f;
begin
  Result.X := FAtmosphereRadius;
  Result.Y := Result.X;
  Result.Z := Result.X;
  Result.W := 0;
end;

procedure TgxCustomAtmosphere.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSun) then
    FSun := nil;
end;

procedure TgxCustomAtmosphere.SetAtmosphereRadius(
  const Value: Single);
begin
  FAtmosphereRadius := Value;
  if Value <= FPlanetRadius then
    FPlanetRadius := FAtmosphereRadius / 1.01;
end;

procedure TgxCustomAtmosphere.SetPlanetRadius(const Value: Single);
begin
  FPlanetRadius := Value;
  if Value >= FAtmosphereRadius then
    FAtmosphereRadius := FPlanetRadius * 1.01;
end;

procedure TgxCustomAtmosphere.EnableGLBlendingMode(StateCache: TgxStateCache);
begin
  case FBlendingMode of
    abmOneMinusDstColor:
      StateCache.SetBlendFunc(bfDstAlpha, bfOneMinusDstColor);
    abmOneMinusSrcAlpha:
      StateCache.SetBlendFunc(bfDstAlpha, bfOneMinusSrcAlpha);
  else
    Assert(False, strErrorEx + strUnknownType);
  end;
  StateCache.Enable(stAlphaTest);
end;

function TgxCustomAtmosphere.StoreAtmosphereRadius: Boolean;
begin
  Result := Abs(FAtmosphereRadius - 3.55) > EPS;
end;

function TgxCustomAtmosphere.StoreOpacity: Boolean;
begin
  Result := Abs(FOpacity - 2.1) > EPS;
end;

function TgxCustomAtmosphere.StorePlanetRadius: Boolean;
begin
  Result := Abs(FPlanetRadius - 3.395) > EPS;
end;

procedure TgxCustomAtmosphere.SetSlices(const Value: Integer);
begin
  if Value > 0 then
  begin
    FSlices := Value;
    SetLength(cosCache, FSlices + 1);
    SetLength(sinCache, FSlices + 1);
    PrepareSinCosCache(sinCache, cosCache, 0, 360);

    GetMem(pVertex, 2 * (FSlices + 1) * SizeOf(TVector4f));
    GetMem(pColor, 2 * (FSlices + 1) * SizeOf(TVector4f));
  end
  else
    raise EGLAtmosphereException.Create('Slices must be more than 0!');
end;

procedure TgxCustomAtmosphere.SetHighAtmColor(const AValue: TgxColor);
begin
  FHighAtmColor.Assign(AValue);
end;

procedure TgxCustomAtmosphere.SetLowAtmColor(const AValue: TgxColor);
begin
  FLowAtmColor.Assign(AValue);
end;

function TgxCustomAtmosphere.StoreHighAtmColor: Boolean;
begin
  Result := not VectorEquals(FHighAtmColor.Color, VectorMake(0, 0, 1, 1));
end;

function TgxCustomAtmosphere.StoreLowAtmColor: Boolean;
begin
  Result := not VectorEquals(FLowAtmColor.Color, VectorMake(1, 1, 1, 1));
end;

initialization
  RegisterClasses([TgxCustomAtmosphere, TgxAtmosphere]);

end.

