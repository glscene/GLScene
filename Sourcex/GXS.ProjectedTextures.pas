//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.ProjectedTextures;

(* Implements projected textures through a GLScene object *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  System.Classes,

  GXS.XOpenGL,
  GXS.VectorTypes,
  GXS.Scene,
  GXS.PersistentClasses,
  GXS.Texture,
  GXS.VectorGeometry,
  GXS.RenderContextInfo,
  GXS.State,
  GXS.Context;

type

  (* Possible styles of texture projection. Possible values:
    ptsOriginal: Original projection method (first pass,
    is default scene render, second pass is texture  projection).
    ptsInverse: Inverse projection method (first pass
    is texture projection, sencond pass is regular scene render).
    This method is useful if you want to simulate
    lighting only through projected textures (the textures
    of the scene are "masked" into the white areas of
    the projection textures). *)
  TgxProjectedTexturesStyle = (ptsOriginal, ptsInverse);

  TgxProjectedTextures = class;

  (* A projected texture emmiter.
     It's material property will be used as the projected texture.
     Can be places anywhere in the scene. *)
  TgxTextureEmitter = class(TgxSceneObject)
  private
    FFOVy: single;
    FAspect: single;
  protected
    (* Sets up the base texture matrix for this emitter
       Should be called whenever a change on its properties is made.*)
    procedure SetupTexMatrix(var ARci: TgxRenderContextInfo);
  public
    constructor Create(AOwner: TComponent); override;
  published
    // Indicates the field-of-view of the projection frustum.
    property FOVy: single read FFOVy write FFOVy;
    (* x/y ratio. For no distortion, this should be set to
       texture.width/texture.height.*)
    property Aspect: single read FAspect write FAspect;
  end;

  // Specifies an item on the TgxTextureEmitters collection.
  TgxTextureEmitterItem = class(TCollectionItem)
  private
    FEmitter: TgxTextureEmitter;
  protected
    procedure SetEmitter(const val: TgxTextureEmitter);
    procedure RemoveNotification(aComponent: TComponent);
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Emitter: TgxTextureEmitter read FEmitter write SetEmitter;
  end;

  // Collection of TgxTextureEmitter.
  TgxTextureEmitters = class(TCollection)
  private
    FOwner: TgxProjectedTextures;
  protected
    function GetOwner: TPersistent; override;
    function GetItems(index: Integer): TgxTextureEmitterItem;
    procedure RemoveNotification(aComponent: TComponent);
  public
    procedure AddEmitter(texEmitter: TgxTextureEmitter);
    property Items[index: Integer]: TgxTextureEmitterItem read GetItems; default;
  end;

  (* Projected Textures Manager.
    Specifies active texture Emitters (whose texture will be projected)
    and receivers (children of this object). *)
  TgxProjectedTextures = class(TgxImmaterialSceneObject)
  private
    FEmitters: TgxTextureEmitters;
    FStyle: TgxProjectedTexturesStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TgxRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
    // List of texture emitters.
    property Emitters: TgxTextureEmitters read FEmitters write FEmitters;
    // Indicates the style of the projected textures.
    property Style: TgxProjectedTexturesStyle read FStyle write FStyle;
  end;

//==============================================================
implementation
//==============================================================

// ------------------
// ------------------ TgxTextureEmitter ------------------
// ------------------

constructor TgxTextureEmitter.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FFOVy := 90;
  FAspect := 1;
end;

procedure TgxTextureEmitter.SetupTexMatrix(var ARci: TgxRenderContextInfo);
const
  cBaseMat: TMatrix4f =
  (V:((X:0.5; Y:0;   Z:0; W:0),
          (X:0;   Y:0.5; Z:0; W:0),
          (X:0;   Y:0; Z:1; W:0),
          (X:0.5; Y:0.5; Z:0; W:1)));

var
  PM: TMatrix4f;
begin
  // Set the projector's "perspective" (i.e. the "spotlight cone"):.
  PM := MatrixMultiply(CreatePerspectiveMatrix(FFOVy, FAspect, 0.1, 1), cBaseMat);
  PM := MatrixMultiply(invAbsoluteMatrix, PM);
  Arci.gxStates.SetTextureMatrix(PM);
end;

// ------------------
// ------------------ TgxTextureEmitterItem ------------------
// ------------------

constructor TgxTextureEmitterItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
end;

procedure TgxTextureEmitterItem.Assign(Source: TPersistent);
begin
  if Source is TgxTextureEmitterItem then
  begin
    FEmitter := TgxTextureEmitterItem(Source).FEmitter;
    TgxProjectedTextures(TgxTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
  inherited;
end;

procedure TgxTextureEmitterItem.SetEmitter(const val: TgxTextureEmitter);
begin
  if FEmitter <> val then
  begin
    FEmitter := val;
    TgxProjectedTextures(TgxTextureEmitters(Collection).GetOwner).StructureChanged;
  end;
end;

procedure TgxTextureEmitterItem.RemoveNotification(aComponent: TComponent);
begin
  if aComponent = FEmitter then
    FEmitter := nil;
end;

function TgxTextureEmitterItem.GetDisplayName: string;
begin
  if Assigned(FEmitter) then
  begin
    Result := '[TexEmitter] ' + FEmitter.Name;
  end
  else
    Result := 'nil';
end;

// ------------------
// ------------------ TgxTextureEmitters ------------------
// ------------------

function TgxTextureEmitters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TgxTextureEmitters.GetItems(index: Integer): TgxTextureEmitterItem;
begin
  Result := TgxTextureEmitterItem(inherited Items[index]);
end;

procedure TgxTextureEmitters.RemoveNotification(aComponent: TComponent);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].RemoveNotification(aComponent);
end;

procedure TgxTextureEmitters.AddEmitter(texEmitter: TgxTextureEmitter);
var
  item: TgxTextureEmitterItem;
begin
  item := TgxTextureEmitterItem(self.Add);
  item.Emitter := texEmitter;
end;

// ------------------
// ------------------ TgxProjectedTextures ------------------
// ------------------

constructor TgxProjectedTextures.Create(AOwner: TComponent);
begin
  inherited Create(aOWner);
  FEmitters := TgxTextureEmitters.Create(TgxTextureEmitterItem);
  FEmitters.FOwner := self;
end;

destructor TgxProjectedTextures.Destroy;
begin
  FEmitters.Free;
  inherited destroy;
end;

procedure TgxProjectedTextures.DoRender(var ARci: TgxRenderContextInfo;
  ARenderSelf, ARenderChildren: boolean);
const
  PS: array[0..3] of Single = (1, 0, 0, 0);
  PT: array[0..3] of Single = (0, 1, 0, 0);
  PR: array[0..3] of Single = (0, 0, 1, 0);
  PQ: array[0..3] of Single = (0, 0, 0, 1);
var
  i: integer;
  emitter: TgxTextureEmitter;
begin
  if not (ARenderSelf or ARenderChildren) then
    Exit;
  if (csDesigning in ComponentState) then
  begin
    inherited;
    Exit;
  end;

  //First pass of original style: render regular scene
  if Style = ptsOriginal then
    self.RenderChildren(0, Count - 1, ARci);

  //generate planes
  glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
  glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);

  glTexGenfv(GL_S, GL_EYE_PLANE, @PS);
  glTexGenfv(GL_T, GL_EYE_PLANE, @PT);
  glTexGenfv(GL_R, GL_EYE_PLANE, @PR);
  glTexGenfv(GL_Q, GL_EYE_PLANE, @PQ);

  //options
  Arci.gxStates.Disable(stLighting);
  Arci.gxStates.DepthFunc := cfLEqual;
  Arci.gxStates.Enable(stBlend);
  glEnable(GL_TEXTURE_GEN_S);
  glEnable(GL_TEXTURE_GEN_T);
  glEnable(GL_TEXTURE_GEN_R);
  glEnable(GL_TEXTURE_GEN_Q);

  //second pass (original) first pass (inverse): for each emiter,
  //render projecting the texture summing all emitters
  for i := 0 to Emitters.Count - 1 do
  begin
    emitter := Emitters[i].Emitter;
    if not assigned(emitter) then
      continue;
    if not emitter.Visible then
      continue;

    emitter.Material.Apply(ARci);

    ARci.gxStates.Enable(stBlend);
    if Style = ptsOriginal then
    begin
      //on the original style, render blending the textures
      if emitter.Material.Texture.TextureMode <> tmBlend then
        ARci.gxStates.SetBlendFunc(bfDstColor, bfOne)
      else
        ARci.gxStates.SetBlendFunc(bfDstColor, bfZero);
    end
    else
    begin
      //on inverse style: the first texture projector should
      //be a regular rendering (i.e. no blending). All others
      //are "added" together creating an "illumination mask"
      if i = 0 then
        Arci.gxStates.SetBlendFunc(bfOne, bfZero)
      else
        ARci.gxStates.SetBlendFunc(bfOne, bfOne);
    end;

    //get this emitter's tex matrix
    emitter.SetupTexMatrix(ARci);
    repeat
      ARci.ignoreMaterials := true;
      Self.RenderChildren(0, Count - 1, ARci);
      ARci.ignoreMaterials := false;
    until not emitter.Material.UnApply(ARci);
  end;

  // LoseTexMatrix
  ARci.gxStates.SetBlendFunc(bfOne, bfZero);
  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);
  glDisable(GL_TEXTURE_GEN_R);
  glDisable(GL_TEXTURE_GEN_Q);

  glMatrixMode(GL_TEXTURE);
  glLoadIdentity;
  glMatrixMode(GL_MODELVIEW);

  ARci.gxStates.DepthFunc := cfLEqual;

  //second pass (inverse): render regular scene, blending it
  //with the "mask"
  if Style = ptsInverse then
  begin

    Arci.gxStates.Enable(stBlend);
    ARci.gxStates.SetBlendFunc(bfDstColor, bfSrcColor);

    //second pass: render everything, blending with what is
    //already there
    ARci.ignoreBlendingRequests := true;
    self.RenderChildren(0, Count - 1, ARci);
    ARci.ignoreBlendingRequests := false;

  end;
end;

//------------------------------------------
initialization
//------------------------------------------

  RegisterClasses([TgxTextureEmitter, TgxProjectedTextures]);

end.

