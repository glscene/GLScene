//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.ShadowVolume;

(*
   Implements basic shadow volumes support.

   Be aware that only objects that support silhouette determination have a chance
   to cast correct shadows. Transparent/blended/shader objects among the receivers
   or the casters will be rendered incorrectly.
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.SysUtils,
  System.Classes,

  GXS.Scene,
  GXS.PipelineTransformation,
  GXS.VectorLists,
  GXS.State, 
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.Context,
  GXS.Silhouette,
  GXS.PersistentClasses,
  GXS.GeometryBB,
  GXS.Color,
  GXS.RenderContextInfo;

type

  TgxShadowVolume = class;

  (* Determines when a shadow volume should generate a cap at the beginning and
   end of the volume. This is ONLY necessary when there's a chance that the
   camera could end up inside the shadow _or_ between the light source and
   the camera. If those two situations can't occur then not using capping is
   the best option.
   Note that if you use the capping, you must either set the depth of view of
   your camera to something very large (f.i. 1e9), or you could use the infinite
   mode (csInfinitePerspective) of your camera.

      svcDefault : Default behaviour
      svcAlways : Always generates caps
      svcNever : Never generates caps  *)
  TgxShadowVolumeCapping = (svcDefault, svcAlways, svcNever);

  (* Determines when a caster should actually produce a shadow;

    scmAlways : Caster always produces a shadow, ignoring visibility
    scmVisible : Caster casts shadow if the object has visible=true
    scmRecursivelyVisible : Caster casts shadow if ancestors up the hierarchy
     all have visible=true
    scmParentVisible : Caster produces shadow if parent has visible=true
    scmParentRecursivelyVisible : Caster casts shadow if ancestors up the hierarchy
    all have visible=true, starting from the parent (ignoring own visible setting) *)
  TgxShadowCastingMode = (scmAlways, scmVisible, scmRecursivelyVisible,
    scmParentVisible, scmParentRecursivelyVisible);

  (* Specifies an individual shadow caster.
     Can be a light or an opaque object. *)
  TgxShadowVolumeCaster = class(TCollectionItem)
  private
    FCaster: TgxBaseSceneObject;
    FEffectiveRadius: Single;
    FCapping: TgxShadowVolumeCapping;
    FCastingMode: TgxShadowCastingMode;
  protected
    procedure SetCaster(const val: TgxBaseSceneObject);
    function GetGLShadowVolume: TgxShadowVolume;
    procedure RemoveNotification(aComponent: TComponent);
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    { Shadow casting object.  Can be an opaque object or a lightsource. }
    property Caster: TgxBaseSceneObject read FCaster write SetCaster;
    property GLShadowVolume: TgxShadowVolume read GetGLShadowVolume;
  published
    (* Radius beyond which the caster can be ignored.
      Zero (default value) means the caster can never be ignored. *)
    property EffectiveRadius: Single read FEffectiveRadius write  FEffectiveRadius;
    (* Specifies if the shadow volume should be capped.
       Capping helps solve shadowing artefacts, at the cost of performance. *)
    property Capping: TgxShadowVolumeCapping read FCapping write FCapping default  svcDefault;
    (* Determines when an object should cast a shadow or not. Typically, objects
    should only cast shadows when recursively visible. But if you're using
    dummy shadow casters which are less complex than their parent objects,
    you should use scmParentRecursivelyVisible.*)
    property CastingMode: TgxShadowCastingMode read FCastingMode write
      FCastingMode default scmRecursivelyVisible;
  end;

  // Specifies an individual shadow casting occluder.
  TgxShadowVolumeOccluder = class(TgxShadowVolumeCaster)
  published
    property Caster;
  end;

  // Specifies an individual shadow casting light.
  TgxShadowVolumeLight = class(TgxShadowVolumeCaster)
  private
    FSilhouettes: TgxPersistentObjectList;
  protected
    function GetLightSource: TgxLightSource;
    procedure SetLightSource(const ls: TgxLightSource);
    function GetCachedSilhouette(AIndex: Integer): TgxSilhouette; inline;
    procedure StoreCachedSilhouette(AIndex: Integer; ASil: TgxSilhouette);
    (* Compute and setup scissor clipping rect for the light.
       Returns true if a scissor rect was setup *)
    function SetupScissorRect(worldAABB: PAABB; var rci: TgxRenderContextInfo): Boolean;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure FlushSilhouetteCache;
  published
    // Shadow casting lightsource.
    property LightSource: TgxLightSource read GetLightSource write SetLightSource;
  end;

  // Collection of TgxShadowVolumeCaster.
  TgxShadowVolumeCasters = class(TOwnedCollection)
  protected
    function GetItems(index: Integer): TgxShadowVolumeCaster;
    procedure RemoveNotification(aComponent: TComponent);
  public
    function AddCaster(obj: TgxBaseSceneObject; effectiveRadius: Single = 0;
      CastingMode: TgxShadowCastingMode = scmRecursivelyVisible):
      TgxShadowVolumeCaster;
    procedure RemoveCaster(obj: TgxBaseSceneObject);
    function IndexOfCaster(obj: TgxBaseSceneObject): Integer;
    property Items[index: Integer]: TgxShadowVolumeCaster read GetItems; default;
  end;

  (* Shadow volume rendering options/optimizations.
      svoShowVolumes : make the shadow volumes visible
      svoDesignVisible : the shadow are visible at design-time
      svoCacheSilhouettes : cache shadow volume silhouettes, beneficial when
        some objects are static relatively to their light(s)
      svoScissorClips : use scissor clipping per light, beneficial when
        lights are attenuated and don't illuminate the whole scene
      svoWorldScissorClip : use scissor clipping for the world, beneficial
        when shadow receivers don't cover the whole viewer surface  *)
  TgxShadowVolumeOption = (svoShowVolumes, svoCacheSilhouettes, svoScissorClips,
    svoWorldScissorClip, svoDesignVisible);
  TgxShadowVolumeOptions = set of TgxShadowVolumeOption;

  (* Shadow rendering modes.
      svmAccurate : will render the scene with ambient lighting only, then
        for each light will make a diffuse+specular pass
      svmDarkening : renders the scene with lighting on as usual, then darkens
        shadowed areas (i.e. inaccurate lighting, but will "shadow" objects
        that don't honour to diffuse or specular lighting)
      svmOff : no shadowing will take place *)
  TgxShadowVolumeMode = (svmAccurate, svmDarkening, svmOff);

  (* Simple shadow volumes.
     Shadow receiving objects are the ShadowVolume's children, shadow casters
     (opaque objects or lights) must be explicitly specified in the Casters
     collection.
     Shadow volumes require that the buffer allows stencil buffers,
     GLXceneViewer.Buffer.ContextOptions contain roStencinBuffer. Without stencil
     buffers, shadow volumes will not work properly.
     Another issue to look out for is the fact that shadow volume capping requires
     that the camera depth of view is either very high (fi 1e9) or that the
     camera style is csInfinitePerspective. *)
  TgxShadowVolume = class(TgxImmaterialSceneObject)
  private
    FActive: Boolean;
    FRendering: Boolean;
    FLights: TgxShadowVolumeCasters;
    FOccluders: TgxShadowVolumeCasters;
    FCapping: TgxShadowVolumeCapping;
    FOptions: TgxShadowVolumeOptions;
    FMode: TgxShadowVolumeMode;
    FDarkeningColor: TgxColor;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;
    procedure SetActive(const val: Boolean);
    procedure SetLights(const val: TgxShadowVolumeCasters);
    procedure SetOccluders(const val: TgxShadowVolumeCasters);
    procedure SetOptions(const val: TgxShadowVolumeOptions);
    procedure SetMode(const val: TgxShadowVolumeMode);
    procedure SetDarkeningColor(const val: TgxColor);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TgxRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure Assign(Source: TPersistent); override;
    procedure FlushSilhouetteCache;
  published
    (* Determines if shadow volume rendering is active.
      When set to false, children will be rendered without any shadowing
             or multipass lighting. *)
    property Active: Boolean read FActive write SetActive default True;
    // Lights that cast shadow volumes.
    property Lights: TgxShadowVolumeCasters read FLights write SetLights;
    // Occluders that cast shadow volumes.
    property Occluders: TgxShadowVolumeCasters read FOccluders write SetOccluders;
    (* Specifies if the shadow volume should be capped.
      Capping helps solve shadowing artefacts, at the cost of performance. *)
    property Capping: TgxShadowVolumeCapping read FCapping write FCapping default
      svcAlways;
    // Shadow volume rendering options.
    property Options: TgxShadowVolumeOptions read FOptions write SetOptions
      default [svoCacheSilhouettes, svoScissorClips];
    // Shadow rendering mode.
    property Mode: TgxShadowVolumeMode read FMode write SetMode default svmAccurate;
    // Darkening color used in svmDarkening mode.
    property DarkeningColor: TgxColor read FDarkeningColor write SetDarkeningColor;
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

// ------------------
// ------------------ TgxShadowVolumeCaster ------------------
// ------------------

constructor TgxShadowVolumeCaster.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FCapping := svcDefault;
  FCastingMode := scmRecursivelyVisible;
end;

type
  // Required for Delphi 5 support.
  THackOwnedCollection = class(TOwnedCollection);

function TgxShadowVolumeCaster.GetGLShadowVolume: TgxShadowVolume;
begin
  Result := TgxShadowVolume(THackOwnedCollection(Collection).GetOwner);
end;

destructor TgxShadowVolumeCaster.Destroy;
begin
  if Assigned(FCaster) then
    FCaster.RemoveFreeNotification(GLShadowVolume);
  inherited;
end;

procedure TgxShadowVolumeCaster.Assign(Source: TPersistent);
begin
  if Source is TgxShadowVolumeCaster then
  begin
    FCaster := TgxShadowVolumeCaster(Source).FCaster;
    FEffectiveRadius := TgxShadowVolumeCaster(Source).FEffectiveRadius;
    FCapping := TgxShadowVolumeCaster(Source).FCapping;
    GetGLShadowVolume.StructureChanged;
  end;
  inherited;
end;

procedure TgxShadowVolumeCaster.SetCaster(const val: TgxBaseSceneObject);
begin
  if FCaster <> val then
  begin
    if FCaster <> nil then
      FCaster.RemoveFreeNotification(GLShadowVolume);
    FCaster := val;
    if FCaster <> nil then
      FCaster.FreeNotification(GLShadowVolume);
    GetGLShadowVolume.StructureChanged;
  end;
end;

procedure TgxShadowVolumeCaster.RemoveNotification(aComponent: TComponent);
begin
  if aComponent = FCaster then
  begin
    // No point in keeping the TgxShadowVolumeCaster once the FCaster has been
    // destroyed.
    FCaster := nil;
    Free;
  end;
end;

function TgxShadowVolumeCaster.GetDisplayName: string;
begin
  if Assigned(FCaster) then
  begin
    if FCaster is TgxLightSource then
      Result := '[Light]'
    else
      Result := '[Object]';
    Result := Result + ' ' + FCaster.Name;
    if EffectiveRadius > 0 then
      Result := Result + Format(' (%.1f)', [EffectiveRadius]);
  end
  else
    Result := 'nil';
end;

// ------------------
// ------------------ TgxShadowVolumeLight ------------------
// ------------------

constructor TgxShadowVolumeLight.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FSilhouettes := TgxPersistentObjectList.Create;
end;

destructor TgxShadowVolumeLight.Destroy;
begin
  FlushSilhouetteCache;
  FSilhouettes.Free;
  inherited;
end;

procedure TgxShadowVolumeLight.FlushSilhouetteCache;
begin
  FSilhouettes.Clean;
end;

function TgxShadowVolumeLight.GetLightSource: TgxLightSource;
begin
  Result := TgxLightSource(Caster);
end;

procedure TgxShadowVolumeLight.SetLightSource(const ls: TgxLightSource);
begin
  SetCaster(ls);
end;

function TgxShadowVolumeLight.GetCachedSilhouette(AIndex: Integer):
  TgxSilhouette;
begin
  if AIndex < FSilhouettes.Count then
    Result := TgxSilhouette(FSilhouettes[AIndex])
  else
    Result := nil;
end;

procedure TgxShadowVolumeLight.StoreCachedSilhouette(AIndex: Integer; ASil:
  TgxSilhouette);
begin
  while AIndex >= FSilhouettes.Count do
    FSilhouettes.Add(nil);
  if ASil <> FSilhouettes[AIndex] then
  begin
    if assigned(FSilhouettes[AIndex]) then
      FSilhouettes[AIndex].Free;
    FSilhouettes[AIndex] := ASil;
  end;
end;

function TgxShadowVolumeLight.SetupScissorRect(worldAABB: PAABB; var rci:
  TgxRenderContextInfo): Boolean;
var
  mvp: TMatrix4f;
  ls: TgxLightSource;
  aabb: TAABB;
  clipRect: TClipRect;
begin
  ls := LightSource;
  if (EffectiveRadius <= 0) or (not ls.Attenuated) then
  begin
    // non attenuated lights can't be clipped
    if not Assigned(worldAABB) then
    begin
      Result := False;
      Exit;
    end
    else
      aabb := worldAABB^;
  end
  else
  begin
    aabb := BSphereToAABB(ls.AbsolutePosition, EffectiveRadius);
    if Assigned(worldAABB) then
      aabb := AABBIntersection(aabb, worldAABB^);
  end;

  if PointInAABB(rci.cameraPosition, aabb) then
  begin
    // camera inside light volume radius, can't clip
    Result := False;
    Exit;
  end;

  // Calculate the window-space bounds of the light's bounding box.
  mvp := rci.PipelineTransformation.ViewProjectionMatrix^;

  clipRect := AABBToClipRect(aabb, mvp, rci.viewPortSize.cx,
    rci.viewPortSize.cy);

  if (clipRect.Right < 0) or (clipRect.Left > rci.viewPortSize.cx)
    or (clipRect.Top < 0) or (clipRect.Bottom > rci.viewPortSize.cy) then
  begin
    Result := False;
    Exit;
  end;

  with clipRect do
    glScissor(Round(Left), Round(Top), Round(Right - Left), Round(Bottom -
      Top));
  Result := True;
end;

// ------------------
// ------------------ TgxShadowVolumeCasters ------------------
// ------------------

procedure TgxShadowVolumeCasters.RemoveNotification(aComponent: TComponent);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].RemoveNotification(aComponent);
end;

function TgxShadowVolumeCasters.GetItems(index: Integer): TgxShadowVolumeCaster;
begin
  Result := TgxShadowVolumeCaster(inherited Items[index]);
end;

function TgxShadowVolumeCasters.AddCaster(obj: TgxBaseSceneObject;
  effectiveRadius: Single = 0;
  CastingMode: TgxShadowCastingMode = scmRecursivelyVisible):
  TgxShadowVolumeCaster;
var
  newCaster: TgxShadowVolumeCaster;
begin
  newCaster := TgxShadowVolumeCaster(Add);
  newCaster.Caster := obj;
  newCaster.EffectiveRadius := effectiveRadius;
  newCaster.CastingMode := CastingMode;

  result := newCaster;
end;

procedure TgxShadowVolumeCasters.RemoveCaster(obj: TgxBaseSceneObject);
var
  i: Integer;
begin
  i := IndexOfCaster(obj);
  if i >= 0 then
    Delete(i);
end;

function TgxShadowVolumeCasters.IndexOfCaster(obj: TgxBaseSceneObject): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Caster = obj then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

// ------------------
// ------------------ TgxShadowVolume ------------------
// ------------------

constructor TgxShadowVolume.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle - [osDirectDraw] + [osNoVisibilityCulling];
  FActive := True;
  FLights := TgxShadowVolumeCasters.Create(self, TgxShadowVolumeLight);
  FOccluders := TgxShadowVolumeCasters.Create(self, TgxShadowVolumeOccluder);
  FCapping := svcAlways;
  FMode := svmAccurate;
  FOptions := [svoCacheSilhouettes, svoScissorClips];
  FDarkeningColor := TgxColor.CreateInitialized(Self, VectorMake(0, 0, 0, 0.5));
end;

destructor TgxShadowVolume.Destroy;
begin
  inherited;
  FDarkeningColor.Free;
  FLights.Free;
  FOccluders.Free;
end;

procedure TgxShadowVolume.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    FLights.RemoveNotification(AComponent);
    FOccluders.RemoveNotification(AComponent);
  end;
  inherited;
end;

procedure TgxShadowVolume.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TgxShadowVolume) then
  begin
    FLights.Assign(TgxShadowVolume(Source).Lights);
    FOccluders.Assign(TgxShadowVolume(Source).Occluders);
    FCapping := TgxShadowVolume(Source).FCapping;
    StructureChanged;
  end;
  inherited Assign(Source);
end;

procedure TgxShadowVolume.FlushSilhouetteCache;
var
  i: Integer;
begin
  for i := 0 to Lights.Count - 1 do
    (Lights[i] as TgxShadowVolumeLight).FlushSilhouetteCache;
end;

procedure TgxShadowVolume.SetActive(const val: Boolean);
begin
  if FActive <> val then
  begin
    FActive := val;
    StructureChanged;
  end;
end;

procedure TgxShadowVolume.SetLights(const val: TgxShadowVolumeCasters);
begin
  Assert(val.ItemClass = TgxShadowVolumeLight);
  FLights.Assign(val);
  StructureChanged;
end;

procedure TgxShadowVolume.SetOccluders(const val: TgxShadowVolumeCasters);
begin
  Assert(val.ItemClass = TgxShadowVolumeOccluder);
  FOccluders.Assign(val);
  StructureChanged;
end;

procedure TgxShadowVolume.SetOptions(const val: TgxShadowVolumeOptions);
begin
  if FOptions <> val then
  begin
    FOptions := val;
    if not (svoCacheSilhouettes in FOptions) then
      FlushSilhouetteCache;
    StructureChanged;
  end;
end;

procedure TgxShadowVolume.SetMode(const val: TgxShadowVolumeMode);
begin
  if FMode <> val then
  begin
    FMode := val;
    StructureChanged;
  end;
end;

procedure TgxShadowVolume.SetDarkeningColor(const val: TgxColor);
begin
  FDarkeningColor.Assign(val);
end;

procedure TgxShadowVolume.DoRender(var ARci: TgxRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);

// Function that determines if an object is "recursively visible". It halts when
// * it finds an invisible ancestor (=> invisible)
// * it finds the root (=> visible)
// * it finds the shadow volume as an ancestor (=> visible)
//
// This does _not_ mean that the object is actually visible on the screen

function DirectHierarchicalVisibility(obj: TgxBaseSceneObject): boolean;
  var
    p: TgxBaseSceneObject;
  begin
    if not Assigned(obj) then
    begin
      Result := True;
      exit;
    end;
    if not obj.Visible then
    begin
      Result := False;
      Exit;
    end;
    p := obj.Parent;
    while Assigned(p) and (p <> obj) and (p <> Self) do
    begin
      if not p.Visible then
      begin
        Result := False;
        Exit;
      end;
      p := p.Parent;
    end;
    Result := True;
  end;

var
  i, k: Integer;
  lightSource: TgxLightSource;
  lightCaster: TgxShadowVolumeLight;
  sil: TgxSilhouette;
  lightID: Cardinal;
  obj: TgxBaseSceneObject;
  caster: TgxShadowVolumeCaster;
  opaques, opaqueCapping: TList;
  silParams: TgxSilhouetteParameters;
  worldAABB: TAABB;
  pWorldAABB: PAABB;
  PM: TMatrix4f;
begin
  if not Active then
  begin
    inherited;
    Exit;
  end;
  if FRendering then
    Exit;
  if not (ARenderSelf or ARenderChildren) then
    Exit;
  ClearStructureChanged;
  if ((csDesigning in ComponentState) and not (svoDesignVisible in Options))
    or (Mode = svmOff)
    or (ARci.drawState = dsPicking) then
  begin
    inherited;
    Exit;
  end;
  if svoWorldScissorClip in Options then
  begin
    // compute shadow receiving world AABB in absolute coordinates
    worldAABB := Self.AxisAlignedBoundingBox;
    AABBTransform(worldAABB, AbsoluteMatrix);
    pWorldAABB := @worldAABB;
  end
  else
    pWorldAABB := nil;
  opaques := TList.Create;
  opaqueCapping := TList.Create;
  FRendering := True;
  try
    // collect visible casters
    for i := 0 to Occluders.Count - 1 do
    begin
      caster := Occluders[i];
      obj := caster.Caster;
      if Assigned(obj)
        and
        // Determine when to render this object or not
      (
        (Caster.CastingMode = scmAlways) or
        ((Caster.CastingMode = scmVisible) and obj.Visible) or
        ((Caster.CastingMode = scmRecursivelyVisible) and
        DirectHierarchicalVisibility(obj)) or
        ((Caster.CastingMode = scmParentRecursivelyVisible) and
        DirectHierarchicalVisibility(obj.Parent)) or
        ((Caster.CastingMode = scmParentVisible) and (not Assigned(obj.Parent)
          or
        obj.Parent.Visible))
        )
        and ((caster.EffectiveRadius <= 0)
        or (obj.DistanceTo(ARci.cameraPosition) < caster.EffectiveRadius)) then
      begin
        opaques.Add(obj);
        opaqueCapping.Add(Pointer(Cardinal(ord((caster.Capping = svcAlways)
          or ((caster.Capping = svcDefault)
          and (Capping = svcAlways))))));
      end
      else
      begin
        opaques.Add(nil);
        opaqueCapping.Add(nil);
      end;
    end;

    // render the shadow volumes
    with ARci.gxStates do
    begin

      if Mode = svmAccurate then
      begin
        // first turn off all the shadow casting lights diffuse and specular
        for i := 0 to Lights.Count - 1 do
        begin
          lightCaster := TgxShadowVolumeLight(Lights[i]);
          lightSource := lightCaster.LightSource;
          if Assigned(lightSource) and (lightSource.Shining) then
          begin
            lightID := lightSource.LightID;
            LightDiffuse[lightID] := NullHmgVector;
            LightSpecular[lightID] := NullHmgVector;
          end;
        end;
      end;
      // render shadow receivers with ambient lighting

      // DanB - not sure why this doesn't render properly with these statements
      // where they were originally (after the RenderChildren call).

      Self.RenderChildren(0, Count - 1, ARci);

      ARci.ignoreBlendingRequests := True;
      ARci.ignoreDepthRequests := True;
      DepthWriteMask := False;
      Enable(stDepthTest);
      SetBlendFunc(bfSrcAlpha, bfOne);
      Disable(stAlphaTest);
      Enable(stStencilTest);

      // Disable all client states
      /// if GL_ARB_vertex_buffer_object then
      begin
        VertexArrayBinding := 0;
        ArrayBufferBinding := 0;
        ElementBufferBinding := 0;
      end;

      // turn off *all* lights
      for i := 0 to TgxScene(ARci.scene).Lights.Count - 1 do
      begin
        lightSource := (TgxScene(ARci.scene).Lights.Items[i]) as TgxLightSource;
        if Assigned(lightSource) and lightSource.Shining then
          LightEnabling[lightSource.LightID] := False;
      end;

      glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @NullHmgPoint);
      ARci.PipelineTransformation.Push;

      // render contribution of all shadow casting lights
      for i := 0 to Lights.Count - 1 do
      begin
        lightCaster := TgxShadowVolumeLight(lights[i]);
        lightSource := lightCaster.LightSource;

        if (not Assigned(lightSource)) or (not lightSource.Shining) then
          Continue;

        lightID := lightSource.LightID;

        SetVector(silParams.LightDirection,
          lightSource.SpotDirection.DirectVector);
        case lightSource.LightStyle of
          lsParallel: silParams.Style := ssParallel
        else
          silParams.Style := ssOmni;
        end;
        silParams.CappingRequired := True;

        if Assigned(pWorldAABB) or (svoScissorClips in Options) then
        begin
          if lightCaster.SetupScissorRect(pWorldAABB, ARci) then
            Enable(stScissorTest)
          else
            Disable(stScissorTest);
        end;

        // clear the stencil and prepare for shadow volume pass
        glClear(GL_STENCIL_BUFFER_BIT);
        SetStencilFunc(cfAlways, 0, 255);
        DepthFunc := cfLess;

        if svoShowVolumes in Options then
        begin
          glColor3f(0.05 * i, 0.1, 0);
          Enable(stBlend);
        end
        else
        begin
          SetColorWriting(False);
          Disable(stBlend);
        end;
        Enable(stCullFace);

        Disable(stLighting);
        glEnableClientState(GL_VERTEX_ARRAY);
        SetPolygonOffset(1, 1);

        // for all opaque shadow casters
        for k := 0 to opaques.Count - 1 do
        begin
          obj := TgxBaseSceneObject(opaques[k]);
          if obj = nil then
            Continue;

          SetVector(silParams.SeenFrom,
            obj.AbsoluteToLocal(lightSource.AbsolutePosition));

          sil := lightCaster.GetCachedSilhouette(k);
          if (not Assigned(sil)) or (not CompareMem(@sil.Parameters, @silParams,
            SizeOf(silParams))) then
          begin
            sil := obj.GenerateSilhouette(silParams);
            sil.Parameters := silParams;
            // extrude vertices to infinity
            sil.ExtrudeVerticesToInfinity(silParams.SeenFrom);
          end;
          if Assigned(sil) then
            try
              // render the silhouette
              ARci.PipelineTransformation.SetModelMatrix(obj.AbsoluteMatrix);
              glVertexPointer(4, GL_FLOAT, 0, sil.Vertices.List);

              if Boolean(Cardinal(opaqueCapping[k])) then
              begin
                // z-fail
                /// if GL_EXT_compiled_vertex_array then
                  glLockArraysEXT(0, sil.Vertices.Count);

                CullFaceMode := cmFront;
                SetStencilOp(soKeep, soIncr, soKeep);

                with sil do
                begin
                  glDrawElements(GL_QUADS, Indices.Count, GL_UNSIGNED_INT,
                    Indices.List);
                  Enable(stPolygonOffsetFill);
                  glDrawElements(GL_TRIANGLES, CapIndices.Count,
                    GL_UNSIGNED_INT,
                    CapIndices.List);
                  Disable(stPolygonOffsetFill);
                end;

                CullFaceMode := cmBack;
                SetStencilOp(soKeep, soDecr, soKeep);

                with sil do
                begin
                  glDrawElements(GL_QUADS, Indices.Count, GL_UNSIGNED_INT,
                    Indices.List);
                  Enable(stPolygonOffsetFill);
                  glDrawElements(GL_TRIANGLES, CapIndices.Count,
                    GL_UNSIGNED_INT,
                    CapIndices.List);
                  Disable(stPolygonOffsetFill);
                end;

                /// if GL_EXT_compiled_vertex_array then
                  glUnlockArraysEXT;
              end
              else
              begin
                // z-pass
                CullFaceMode := cmBack;
                SetStencilOp(soKeep, soKeep, soIncr);

                glDrawElements(GL_QUADS, sil.Indices.Count, GL_UNSIGNED_INT,
                  sil.Indices.List);

                CullFaceMode := cmFront;
                SetStencilOp(soKeep, soKeep, soDecr);

                glDrawElements(GL_QUADS, sil.Indices.Count, GL_UNSIGNED_INT,
                  sil.Indices.List);
              end;

            finally
              if (svoCacheSilhouettes in Options) and (not (osDirectDraw in
                ObjectStyle)) then
                lightCaster.StoreCachedSilhouette(k, sil)
              else
                sil.Free;
            end;
        end;

        glDisableClientState(GL_VERTEX_ARRAY);

        // re-enable light's diffuse and specular, but no ambient
        LightEnabling[LightID] := True;
        LightAmbient[LightID] := NullHmgVector;
        LightDiffuse[LightID] := lightSource.Diffuse.Color;
        LightSpecular[LightID] := lightSource.Specular.Color;

        SetColorWriting(True);
        SetStencilOp(soKeep, soKeep, soKeep);

        Enable(stBlend);

        CullFaceMode := cmBack;

        if Mode = svmAccurate then
        begin
          SetStencilFunc(cfEqual, 0, 255);
          DepthFunc := cfEqual;
          Self.RenderChildren(0, Count - 1, ARci);
        end
        else
        begin
          SetStencilFunc(cfNotEqual, 0, 255);

          DepthFunc := cfAlways;
          SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);

          glPushMatrix;
          glLoadIdentity;
          glMatrixMode(GL_PROJECTION);
          glPushMatrix;
          PM := CreateOrthoMatrix(0, 1, 1, 0, -1, 1);
          glLoadMatrixf(PGLFloat(@PM));

          glColor4fv(FDarkeningColor.AsAddress);
          glBegin(GL_QUADS);
          glVertex2f(0, 0);
          glVertex2f(0, 1);
          glVertex2f(1, 1);
          glVertex2f(1, 0);
          glEnd;

          glPopMatrix;
          glMatrixMode(GL_MODELVIEW);
          glPopMatrix;

          SetBlendFunc(bfSrcAlpha, bfOne);
        end;
        // disable light, but restore its ambient component
        LightEnabling[lightID] := False;
        LightAmbient[lightID] := lightSource.Ambient.Color;
      end; // for i
      ARci.PipelineTransformation.Pop;

      // restore OpenGL state
      glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @ARci.sceneAmbientColor);
      Scene.SetupLights(ARci.gxStates.MaxLights);
      Disable(stStencilTest);
      SetPolygonOffset(0, 0);
      ARci.ignoreBlendingRequests := False;
      ARci.ignoreDepthRequests := False;
    end; // of with
  finally
    FRendering := False;
    opaques.Free;
    opaqueCapping.Free;
  end;
end;

//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

  RegisterClasses([TgxShadowVolume]);

end.

