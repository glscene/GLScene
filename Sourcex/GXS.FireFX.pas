//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FireFX;

(* Fire special effect *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,

  GXS.XCollection,
  GXS.BaseClasses,
  GXS.VectorLists,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.Manager,
  GXS.Scene,
  GXS.Context,
  GXS.Cadencer,
  GXS.Color,
  GXS.Coordinates,
  GXS.RenderContextInfo,
  GXS.State,
  GXS.PipelineTransformation,
  GXS.TextureFormat;

type

  PFireParticle = ^TFireParticle;
  TFireParticle = record
    Position: TVector4f;
    Speed: TVector4f;
    Alpha: Single;
    TimeToLive, LifeLength: Single;
  end;
  TFireParticleArray = array[0..MAXINT shr 6] of TFireParticle;
  PFireParticleArray = ^TFireParticleArray;

  TgxBFireFX = class;

  (* Fire special effect manager.
    Defines the looks and behaviour of a particle system that can be made
    to look fire-like. *)
  TgxFireFXManager = class(TgxCadenceAbleComponent)
  private
    FClients: TList;
    FFireParticles: PFireParticleArray;
    FFireDir, FInitialDir: TgxCoordinates;
    FCadencer: TgxCadencer;
    FMaxParticles, FParticleLife: Integer;
    FParticleSize, FFireDensity, FFireEvaporation: Single;
    FFireCrown, FParticleInterval, IntervalDelta: Single;
    NP: Integer;
    FInnerColor, FOuterColor: TgxColor;
    FFireBurst, FFireRadius: Single;
    FDisabled, FPaused, FUseInterval: Boolean;
    FReference: TgxBaseSceneObject;
    FNoZWrite: Boolean;
  protected
    procedure RegisterClient(aClient: TgxBFireFX);
    procedure DeRegisterClient(aClient: TgxBFireFX);
    procedure DeRegisterAllClients;
    procedure SetFireDir(const val: TgxCoordinates);
    procedure SetInitialDir(const val: TgxCoordinates);
    procedure SetCadencer(const val: TgxCadencer);
    function StoreParticleSize: Boolean;
    procedure SetInnerColor(const val: Tgxcolor);
    procedure SetOuterColor(const val: Tgxcolor);
    procedure SetReference(const val: TgxBaseSceneObject);
    procedure SetMaxParticles(const val: Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CalcFire(deltaTime: Double; ParticleInterval, ParticleLife: Single;
      FireAlpha: Single);
    procedure AffParticle3d(Color2: TgxColorVector; const mat: TMatrix4f);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Reinitializes the fire.
    procedure FireInit;
    (* Spawns a large quantity of particles to simulate an isotropic explosion.
       This method generates an isotropic explosion, i.e. there is no
       privilegied direction in the initial vector. *)
    procedure IsotropicExplosion(minInitialSpeed, maxInitialSpeed, lifeBoostFactor: Single;
      nbParticles: Integer = -1);
    (* Spawns a large quantity of particles to simulate a ring explosion.
       This method generates a ring explosion. The plane of the ring is described
       by ringVectorX/Y, which should be of unit length (but you may not
       make them of unit length if you want "elliptic" rings). *)
    procedure RingExplosion(minInitialSpeed, maxInitialSpeed, lifeBoostFactor: Single;
      const ringVectorX, ringVectorY: TAffineVector;
      nbParticles: Integer = -1);
    // Current Nb of particles.
    property ParticleCount: Integer read NP;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
  published
    // Adjusts the acceleration direction (abs coordinates).
    property FireDir: TgxCoordinates read FFireDir write SetFireDir;
    // Adjusts the initial direction (abs coordinates).
    property InitialDir: TgxCoordinates read FInitialDir write SetInitialDir;
    // The cadencer that will "drive" the animation of the system.
    property Cadencer: TgxCadencer read FCadencer write SetCadencer;
    // Maximum number of simultaneous particles in the system.
    property MaxParticles: Integer read FMaxParticles write SetMaxParticles default 256;
    // Size of the particle, in absolute units.
    property ParticleSize: Single read FParticleSize write FParticleSize stored StoreParticleSize;
    // Inner color of a particle.
    property InnerColor: Tgxcolor read FInnerColor write SetInnerColor;
    // Outer color of a particle.
    property OuterColor: Tgxcolor read FOuterColor write SetOuterColor; // default clrWhite;
    property FireDensity: Single read FFireDensity write FFireDensity;
    property FireEvaporation: Single read FFireEvaporation write FFireEvaporation;
    (* Adjust a crown (circular) radius on which particles are spawned.
       With a value of zero, the particles are spawned in the FireRadius
       cube around the origin, with a non zero value, they appear in
       a torus of major radius FireCrown, and minor radius FireRadius*1.73. *)
    property FireCrown: Single read FFireCrown write FFireCrown;
    // Life length of particle.
    property ParticleLife: Integer read FParticleLife write FParticleLife default 3;
    property FireBurst: Single read FFireBurst write FFireBurst;
    // Adjusts the random birth radius for particles (actually a birth cube).
    property FireRadius: Single read FFireRadius write FFireRadius;
    (* If true, no new particles are spawn.
       But current ones continue to live and die. *)
    property Disabled: Boolean read FDisabled write FDisabled;
    // When paused, the fire animation is freezed.
    property Paused: Boolean read FPaused write FPaused;
    (* Interval between particles births (in sec).
       The interval may not be honoured if MawParticles is reached. *)
    property ParticleInterval: Single read FParticleInterval write FParticleInterval;
    (* Enable/disable use of ParticleInterval.
       If true ParticleInterval is used, if False, the system will attempt
       to maintain a particle count of MaxParticles, by spawning new
       particles to replace the dead ones ASAP. *)
    property UseInterval: Boolean read FUseInterval write FUseInterval;
    // Particle's render won't write to Z-Buffer
    property NoZWrite: Boolean read FNoZWrite write FNoZWrite default True;
    (* Specifies an optional object whose position to use as reference.
       This property allows switching between static/shared fires (for
       fireplaces or static torches) and dynamic fire trails.
       The absolute position of the reference object is 'central' spawning
       point for new particles, usually, the object will be the one and only
       one on which the effect is applied. *)
    property Reference: TgxBaseSceneObject read FReference write SetReference;
  end;

  (* Fire special effect.
     This effect works as a client of TFireFXManager *)
  TgxBFireFX = class(TgxObjectPostEffect)
  private
    FManager: TgxFireFXManager;
    FManagerName: string; // NOT persistent, temporarily used for persistence
  protected
    procedure SetManager(const val: TgxFireFXManager);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    procedure Render(var rci: TgxRenderContextInfo); override;
  published
   // Refers the collision manager.
    property Manager: TgxFireFXManager read FManager write SetManager;
  end;

(* Returns or creates the TgxBFireFX within the given behaviours.
   This helper function is convenient way to access a TgxBFireFX. *)
function GetOrCreateFireFX(effects: TgxEffects): TgxBFireFX; overload;
(* Returns or creates the TgxBFireFX within the given object's behaviours.
 This helper function is convenient way to access a TgxBFireFX. *)
function GetOrCreateFireFX(obj: TgxBaseSceneObject): TgxBFireFX; overload;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

function GetOrCreateFireFX(effects: TgxEffects): TgxBFireFX;
var
  i: Integer;
begin
  i := effects.IndexOfClass(TgxBFireFX);
  if i >= 0 then
    Result := TgxBFireFX(effects[i])
  else
    Result := TgxBFireFX.Create(effects);
end;

function GetOrCreateFireFX(obj: TgxBaseSceneObject): TgxBFireFX;
begin
  Result := GetOrCreateFireFX(obj.Effects);
end;

// ------------------
// ------------------ TgxFireFXManager ------------------
// ------------------

constructor TgxFireFXManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
  RegisterManager(Self);
  FFireDir := TgxCoordinates.CreateInitialized(Self, VectorMake(0, 0.5, 0), csPoint);
  FInitialDir := TgxCoordinates.CreateInitialized(Self, YHmgVector, csPoint);
  FMaxParticles := 256;
  FParticleSize := 1.0;
  FInnerColor := TgxColor.Create(Self);
  FInnerColor.Initialize(clrYellow);
  FOuterColor := TgxColor.Create(Self);
  FOuterColor.Initialize(clrOrange);
  FFireDensity := 1;
  FFireEvaporation := 0.86;
  FFireCrown := 0;
  FParticleLife := 3;
  FFireBurst := 0;
  FFireRadius := 1;
  FParticleInterval := 0.1;
  FDisabled := false;
  Fpaused := false;
  FUseInterval := True;
  FNoZWrite := True;
  IntervalDelta := 0;
  FireInit;
end;

destructor TgxFireFXManager.Destroy;
begin
  DeRegisterAllClients;
  DeRegisterManager(Self);
  FreeMem(FFireParticles);
  FInnerColor.Free;
  FOuterColor.Free;
  FClients.Free;
  FFireDir.Free;
  FInitialDir.Free;
  inherited Destroy;
end;

procedure TgxFireFXManager.RegisterClient(aClient: TgxBFireFX);
begin
  if Assigned(aClient) then
    if FClients.IndexOf(aClient) < 0 then
    begin
      FClients.Add(aClient);
      aClient.FManager := Self;
    end;
end;

procedure TgxFireFXManager.DeRegisterClient(aClient: TgxBFireFX);
begin
  if Assigned(aClient) then
  begin
    aClient.FManager := nil;
    FClients.Remove(aClient);
  end;
end;

procedure TgxFireFXManager.DeRegisterAllClients;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to FClients.Count - 1 do
    TgxBFireFX(FClients[i]).FManager := nil;
  FClients.Clear;
end;

procedure TgxFireFXManager.SetFireDir(const val: TgxCoordinates);
begin
  FFireDir.Assign(val);
end;

procedure TgxFireFXManager.SetInitialDir(const val: TgxCoordinates);
begin
  FInitialDir.Assign(val);
end;

procedure TgxFireFXManager.SetCadencer(const val: TgxCadencer);
begin
  if FCadencer <> val then
  begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(Self);
    FCadencer := val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(Self);
  end;
end;

function TgxFireFXManager.StoreParticleSize: Boolean;
begin
  Result := (FParticleSize <> 1);
end;

procedure TgxFireFXManager.SetInnerColor(const val: Tgxcolor);
begin
  if FInnerColor <> val then
  begin
    FInnerColor.color := val.color;
    FireInit;
  end;
end;

procedure TgxFireFXManager.SetOuterColor(const val: Tgxcolor);
begin
  if FOuterColor <> val then
  begin
    FOuterColor.color := val.color;
    FireInit;
  end;
end;

procedure TgxFireFXManager.SetReference(const val: TgxBaseSceneObject);
begin
  // nothing more yet, maybe later
  FReference := val;
end;

procedure TgxFireFXManager.SetMaxParticles(const val: Integer);
begin
  if val <> MaxParticles then
  begin
    if val > 0 then
      FMaxParticles := val
    else
      FMaxParticles := 0;
    ReallocMem(FFireParticles, MaxParticles * Sizeof(TFireParticle));
    if NP > MaxParticles then
      NP := MaxParticles;
  end;
end;

procedure TgxFireFXManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FCadencer then
      Cadencer := nil
    else if AComponent = FReference then
      Reference := nil;
  end;
  inherited;
end;

procedure TgxFireFXManager.DoProgress(const progressTime: TgxProgressTimes);
var
  i: Integer;
begin
  // Progress the particles
  if (not FPaused) and (FParticleInterval > 0) then
    CalcFire(progressTime.deltaTime * (1.0 + Abs(FFireBurst)),
      FParticleInterval, FParticleLife, FFireDensity);

  // Invalidate all clients
  for i := 0 to FClients.Count - 1 do
    TgxBFireFX(FClients[i]).OwnerBaseSceneObject.NotifyChange(TgxBFireFX(FClients[i]));
end;

procedure TgxFireFXManager.FireInit;
begin
  IntervalDelta := 0;
  NP := 0;
  ReallocMem(FFireParticles, FMaxParticles * Sizeof(TFireParticle));
end;

procedure TgxFireFXManager.IsotropicExplosion(minInitialSpeed, maxInitialSpeed, lifeBoostFactor: Single;
  nbParticles: Integer = -1);
var
  n: Integer;
  tmp, refPos: TVector4f;
begin
  if nbParticles < 0 then
    n := MaxInt
  else
    n := nbParticles;
  if Assigned(Reference) then
    refPos := Reference.AbsolutePosition
  else
    refPos := NullHmgPoint;
  while (NP < MaxParticles) and (n > 0) do
  begin
    // okay, ain't exactly "isotropic"...
    SetVector(tmp, Random - 0.5, Random - 0.5, Random - 0.5, 0);
    NormalizeVector(tmp);
    ScaleVector(tmp, minInitialSpeed + Random * (maxInitialSpeed - minInitialSpeed));
    with FFireParticles^[NP] do
    begin
      Position := VectorAdd(refPos, VectorMake((2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius));
      Speed := tmp;
      TimeToLive := ParticleLife * (Random * 0.5 + 0.5) * lifeBoostFactor;
      LifeLength := TimeToLive;
      Alpha := FireDensity;
    end;
    Inc(NP);
    Dec(n);
  end;
end;

procedure TgxFireFXManager.RingExplosion(minInitialSpeed, maxInitialSpeed, lifeBoostFactor: Single;
  const ringVectorX, ringVectorY: TAffineVector;
  nbParticles: Integer = -1);
var
  n: Integer;
  tmp, refPos: TVector4f;
  fx, fy, d: Single;
begin
  if nbParticles < 0 then
    n := MaxInt
  else
    n := nbParticles;
  if Assigned(Reference) then
    refPos := Reference.AbsolutePosition
  else
    refPos := NullHmgPoint;
  while (NP < MaxParticles) and (n > 0) do
  begin
    // okay, ain't exactly and "isotropic" ring...
    fx := Random - 0.5;
    fy := Random - 0.5;
    d := RSqrt(Sqr(fx) + Sqr(fy));
    PAffineVector(@tmp)^ := VectorCombine(ringVectorX, ringVectorY, fx * d, fy * d);
    tmp.W := 1;
    ScaleVector(tmp, minInitialSpeed + Random * (maxInitialSpeed - minInitialSpeed));
    with FFireParticles^[NP] do
    begin
      Position := VectorAdd(refPos, VectorMake((2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius));
      Speed := tmp;
      TimeToLive := ParticleLife * (Random * 0.5 + 0.5) * lifeBoostFactor;
      LifeLength := TimeToLive;
      Alpha := FireDensity;
    end;
    Inc(NP);
    Dec(n);
  end;
end;

procedure TgxFireFXManager.CalcFire(deltaTime: Double;
  particleInterval, particleLife: Single; fireAlpha: Single);
var
  N, I: Integer;
  Fdelta: Single;
  tmp, refPos: TVector4f;
begin
  // Process live stuff
  N := 0;
  I := 0;
  while N < NP do
  begin
    FFireParticles^[I].TimeToLive := FFireParticles^[I].TimeToLive - deltaTime;
    if (FFireParticles^[I].TimeToLive <= 0) then
    begin
      //Get the prev element
      Dec(NP);
      FFireParticles^[I] := FFireParticles^[NP];
    end
    else
    begin
      //animate it
      with FFireParticles^[I] do
      begin
        Speed := VectorCombine(Speed, FireDir.AsVector, 1, deltaTime);
        Position := VectorCombine(Position, Speed, 1, deltaTime);
      end;
      Inc(N);
      Inc(I);
    end;
  end;
  // Spawn new particles
  if FDisabled then
    Exit;
  if Assigned(Reference) then
    refPos := Reference.AbsolutePosition
  else
    refPos := NullHmgPoint;
  IntervalDelta := IntervalDelta + deltaTime / ParticleInterval;
  if (not UseInterval) or (IntervalDelta > 1) then
  begin
    fDelta := Frac(IntervalDelta);
    while (NP < MaxParticles) do
    begin
      SetVector(tmp, (2 * Random - 1) * FireRadius, (2 * Random - 1) * FireRadius,
        FireCrown + (2 * Random - 1) * FireRadius);
      RotateVectorAroundY(PAffineVector(@tmp)^, Random * 2 * PI);
      AddVector(tmp, refPos);
      with FFireParticles^[NP] do
      begin
        Position := tmp;
        Speed := InitialDir.AsVector;
        TimeToLive := ParticleLife * (Random * 0.5 + 0.5);
        LifeLength := TimeToLive;
        Alpha := FireAlpha;
      end;
      Inc(NP);
      if UseInterval then
        Break;
    end;
    IntervalDelta := fDelta;
  end;
end;

procedure TgxFireFXManager.AffParticle3d(Color2: TgxColorVector; const mat: TMatrix4f);
var
  vx, vy: TVector4f;
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    vx.V[i] := mat.V[i].X * FParticleSize;
    vy.V[i] := mat.V[i].Y * FParticleSize;
  end;
  begin
    glBegin(GL_TRIANGLE_FAN);
    glVertex3fv(@NullVector);
    glColor4f(Color2.X, Color2.Y, Color2.Z, 0.0);
    glVertex3f(-vx.X, -vx.Y, -vx.Z);
    // those things should be composited in the model view matrix
    glVertex3f(-0.5 * vx.X + FFireEvaporation * vy.X,
      -0.5 * vx.Y + FFireEvaporation * vy.Y,
      -0.5 * vx.Z + FFireEvaporation * vy.Z);
    glVertex3f(+0.5 * vx.X + FFireEvaporation * vy.X,
      +0.5 * vx.Y + FFireEvaporation * vy.Y,
      +0.5 * vx.Z + FFireEvaporation * vy.Z);
    glVertex3f(+vx.X, +vx.Y, +vx.Z);
    glVertex3f(+0.5 * vx.X - FFireEvaporation * vy.X,
      +0.5 * vx.Y - FFireEvaporation * vy.Y,
      +0.5 * vx.Z - FFireEvaporation * vy.Z);
    glVertex3f(-0.5 * vx.X - FFireEvaporation * vy.X,
      -0.5 * vx.Y - FFireEvaporation * vy.Y,
      -0.5 * vx.Z - FFireEvaporation * vy.Z);
    glVertex3f(-vx.X, -vx.Y, -vx.Z);
    glEnd;
  end;
end;

// ------------------
// ------------------ TgxBFireFX ------------------
// ------------------

constructor TgxBFireFX.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
end;

destructor TgxBFireFX.Destroy;
begin
  Manager := nil;
  inherited Destroy;
end;

class function TgxBFireFX.FriendlyName: string;
begin
  Result := 'FireFX';
end;

class function TgxBFireFX.FriendlyDescription: string;
begin
  Result := 'Fire FX';
end;

procedure TgxBFireFX.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    // ArchiveVersion 1, added inherited call
    WriteInteger(1);
    inherited;
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
  end;
end;

procedure TgxBFireFX.ReadFromFiler(reader: TReader);
var
   archiveVersion : Integer;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion in [0..1]);
    if archiveVersion >= 1 then
      inherited;
    FManagerName := ReadString;
    Manager := nil;
  end;
end;

procedure TgxBFireFX.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TgxFireFXManager, FManagerName);
    if Assigned(mng) then
      Manager := TgxFireFXManager(mng);
    FManagerName := '';
  end;
end;

procedure TgxBFireFX.Assign(Source: TPersistent);
begin
  if Source is TgxBFireFX then
  begin
    Manager := TgxBFireFX(Source).Manager;
  end;
  inherited Assign(Source);
end;

procedure TgxBFireFX.SetManager(const val: TgxFireFXManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterClient(Self);
    if Assigned(val) then
      val.RegisterClient(Self);
  end;
end;

procedure TgxBFireFX.Render(var rci: TgxRenderContextInfo);
var
  n: Integer;
  i: Integer;
  innerColor: TVector4f;
  lastTr: TAffineVector;
  distList: TgxSingleList;
  objList: TList;
  fp: PFireParticle;
begin
  if Manager = nil then
    Exit;

  rci.PipelineTransformation.Push;
  // revert to the base model matrix in the case of a referenced fire
  if Assigned(Manager.Reference) then
    rci.PipelineTransformation.SetModelMatrix(IdentityHmgMatrix);

  rci.gxStates.CurrentProgram := 0;
  rci.gxStates.Disable(stCullFace);
  rci.gxStates.ActiveTextureEnabled[ttTexture2D] := False;
  rci.gxStates.Disable(stLighting);
  rci.gxStates.SetBlendFunc(bfSrcAlpha, bfOne);
  rci.gxStates.Enable(stBlend);
  rci.gxStates.Disable(stAlphaTest);
  rci.gxStates.Enable(stDepthTest);
  rci.gxStates.DepthFunc := cfLEqual;
  rci.gxStates.DepthWriteMask := not Manager.NoZWrite;

  n := Manager.NP;

  if n > 1 then
  begin
    distList := TgxSingleList.Create;
    objList := TList.Create;
    for i := 0 to n - 1 do
    begin
      fp := @(Manager.FFireParticles[i]);
      distList.Add(VectorDotProduct(rci.cameraDirection, fp^.Position));
      objList.Add(fp);
    end;
    QuickSortLists(0, N - 1, distList, objList);

      lastTr := NullVector;
      SetVector(innerColor, Manager.FInnerColor.Color);
      for i := n - 1 downto 0 do
      begin
        fp := PFireParticle(objList[i]);
        glTranslatef(fp^.Position.X - lastTr.X,
                      fp^.Position.Y - lastTr.Y,
                      fp^.Position.Z - lastTr.Z);
        SetVector(lastTr, fp^.Position);
        innerColor.W := fp^.Alpha * fp^.TimeToLive / Sqr(fp^.LifeLength);
        glColor4fv(@innerColor);
        Manager.AffParticle3d(Manager.FOuterColor.Color, rci.PipelineTransformation.ViewMatrix^);
      end;

    objList.Free;
    distList.Free;
  end;

  rci.PipelineTransformation.Pop;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

   
  RegisterXCollectionItemClass(TgxBFireFX);

finalization

  UnregisterXCollectionItemClass(TgxBFireFX);

end.

