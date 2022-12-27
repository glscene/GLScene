//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Mirror;
(*
   Implements a basic, stencil-based mirror (as in Mark Kilgard's demo).
   It is strongly recommended to read and understand the explanations in the
   materials/mirror demo before using this component.
*)
interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  
  GLS.OpenGLTokens,
  GLS.OpenGLAdapter,
  GLS.Scene,
  GLS.VectorGeometry,
  GLS.Context,
  GLS.Material,
  GLS.Color,
  GLS.RenderContextInfo,
  GLS.State,
  GLS.VectorTypes,
  GLS.PersistentClasses,
  GLS.PipelineTransformation,
  GLS.XCollection,
  GLS.Texture;


type

  TGLMirrorOption = (moUseStencil, moOpaque, moMirrorPlaneClip, moClearZBuffer);
  TGLMirrorOptions = set of TGLMirrorOption;

const
  cDefaultMirrorOptions = [moUseStencil];

type

  TMirrorShapes = (msRect, msDisk);

  (* A simple plane mirror. 
     This mirror requires a stencil buffer for optimal rendering! 
     The object is a mix between a plane and a proxy object, in that the plane
     defines the mirror's surface, while the proxy part is used to reference
     the objects that should be mirrored (it is legal to self-mirror, but no
     self-mirror visuals will be rendered). 
     It is strongly recommended to read and understand the explanations in the
     materials/mirror demo before using this component. *)
  TGLMirror = class(TGLSceneObject)
  private
    FRendering: Boolean;
    FMirrorObject: TGLBaseSceneObject;
    FWidth, FHeight: Single;
    FMirrorOptions: TGLMirrorOptions;
    FOnBeginRenderingMirrors, FOnEndRenderingMirrors: TNotifyEvent;
    FShape: TMirrorShapes; //ORL
    FRadius: Single; //ORL
    FSlices: Integer; //ORL
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetMirrorObject(const val: TGLBaseSceneObject);
    procedure SetMirrorOptions(const val: TGLMirrorOptions);
    procedure ClearZBufferArea(aBuffer: TGLSceneBuffer);
    procedure SetHeight(AValue: Single);
    procedure SetWidth(AValue: Single);
    procedure SetRadius(const aValue: Single); //ORL
    procedure SetSlices(const aValue: Integer); //ORL
    procedure SetShape(aValue: TMirrorShapes); //ORL
    function GetRadius: Single; //ORL
    function GetSlices: Integer; //ORL
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure BuildList(var ARci: TGLRenderContextInfo); override;
    procedure Assign(Source: TPersistent); override;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
  published
    // Selects the object to mirror. If nil, the whole scene is mirrored 
    property MirrorObject: TGLBaseSceneObject read FMirrorObject write
      SetMirrorObject;
    (* Controls rendering options. 
        moUseStencil: mirror area is stenciled, prevents reflected
          objects to be visible on the sides of the mirror (stencil buffer
          must be active in the viewer)
        moOpaque: mirror is opaque (ie. painted with background color)
        moMirrorPlaneClip: a ClipPlane is defined to prevent reflections
          from popping out of the mirror (for objects behind or halfway through)
        moClearZBuffer: mirror area's ZBuffer is cleared so that background
          objects don't interfere with reflected objects (reflected objects
          must be rendered AFTER the mirror in the hierarchy). Works only
          along with stenciling. *)
    property MirrorOptions: TGLMirrorOptions read FMirrorOptions write
      SetMirrorOptions default cDefaultMirrorOptions;
    property Height: Single read FHeight write SetHeight;
    property Width: Single read FWidth write SetWidth;
    // Fired before the object's mirror images are rendered. 
    property OnBeginRenderingMirrors: TNotifyEvent read FOnBeginRenderingMirrors
      write FOnBeginRenderingMirrors;
    // Fired after the object's mirror images are rendered. 
    property OnEndRenderingMirrors: TNotifyEvent read FOnEndRenderingMirrors
      write FOnEndRenderingMirrors;
    property Radius: Single read FRadius write SetRadius; //ORL
    property Slices: Integer read FSlices write SetSlices default 16; //ORL
    property Shape: TMirrorShapes read FShape write SetShape default msRect;
    //ORL
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

// ------------------
// ------------------ TGLMirror ------------------
// ------------------

constructor TGLMirror.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FWidth := 1;
  FHeight := 1;
  FMirrorOptions := cDefaultMirrorOptions;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  Material.FrontProperties.Diffuse.Initialize(VectorMake(1, 1, 1, 0.1));
  Material.BlendingMode := bmTransparency;

  FRadius := 1; //ORL
  FSlices := 16; //ORL
  Shape := msRect; //ORL
end;


procedure TGLMirror.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  oldProxySubObject: Boolean;
  refMat, curMat, ModelMat: TGLMatrix;
  clipPlane: TDoubleHmgPlane;
  bgColor: TGLColorVector;
  cameraPosBackup, cameraDirectionBackup: TGLVector;
  CurrentBuffer: TGLSceneBuffer;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    oldProxySubObject := ARci.proxySubObject;
    ARci.proxySubObject := True;
    CurrentBuffer := TGLSceneBuffer(ARci.buffer);

    if VectorDotProduct(VectorSubtract(ARci.cameraPosition, AbsolutePosition),
      AbsoluteDirection) > 0 then
      with ARci.GLStates do
      begin

        // "Render" stencil mask
        if MirrorOptions <> [] then
        begin
          if (moUseStencil in MirrorOptions) then
          begin
            Enable(stStencilTest);
            ARci.GLStates.StencilClearValue := 0;
            gl.Clear(GL_STENCIL_BUFFER_BIT);
            SetStencilFunc(cfAlways, 1, 1);
            SetStencilOp(soReplace, soZero, soReplace);
          end;
          if (moOpaque in MirrorOptions) then
          begin
            bgColor := ConvertWinColor(CurrentBuffer.BackgroundColor);
            ARci.GLStates.SetGLMaterialColors(cmFront, bgColor, clrBlack,
              clrBlack, clrBlack, 0);
          end
          else
            SetGLColorWriting(False);

          Enable(stDepthTest);
          DepthWriteMask := False;

          BuildList(ARci);

          DepthWriteMask := True;
          if (moUseStencil in MirrorOptions) then
          begin
            SetStencilFunc(cfEqual, 1, 1);
            SetStencilOp(soKeep, soKeep, soKeep);
          end;

          if (moClearZBuffer in MirrorOptions) then
            ClearZBufferArea(CurrentBuffer);

          if not (moOpaque in MirrorOptions) then
            SetGLColorWriting(True);
        end;

        ARci.PipelineTransformation.Push;
        ARci.PipelineTransformation.SetModelMatrix(IdentityHmgMatrix);

        Disable(stCullFace);
        Enable(stNormalize);

        if moMirrorPlaneClip in MirrorOptions then
        begin
          gl.Enable(GL_CLIP_PLANE0);
          SetPlane(clipPlane, PlaneMake(AffineVectorMake(AbsolutePosition),
            VectorNegate(AffineVectorMake(AbsoluteDirection))));
          gl.ClipPlane(GL_CLIP_PLANE0, @clipPlane);
        end;

        // Mirror lights
        refMat := MakeReflectionMatrix(
          AffineVectorMake(AbsolutePosition),
          AffineVectorMake(AbsoluteDirection));
        curMat := MatrixMultiply(refMat, ARci.PipelineTransformation.ViewMatrix^);
        ARci.PipelineTransformation.SetViewMatrix(curMat);
        Scene.SetupLights(CurrentBuffer.LimitOf[limLights]);

        // mirror geometry and render master
        cameraPosBackup := ARci.cameraPosition;
        cameraDirectionBackup := ARci.cameraDirection;
        ARci.cameraPosition := VectorTransform(ARci.cameraPosition, refMat);
        ARci.cameraDirection := VectorTransform(ARci.cameraDirection, refMat);

        // temporary fix? (some objects don't respect culling options, or ?)
        CullFaceMode := cmFront;
        if Assigned(FOnBeginRenderingMirrors) then
          FOnBeginRenderingMirrors(Self);
        if Assigned(FMirrorObject) then
        begin
          ModelMat := IdentityHmgMatrix;
          if FMirrorObject.Parent <> nil then
            MatrixMultiply(ModelMat, FMirrorObject.Parent.AbsoluteMatrix, ModelMat);
          MatrixMultiply(ModelMat, FMirrorObject.LocalMatrix^, ModelMat);
          ARci.PipelineTransformation.SetModelMatrix(ModelMat);
          FMirrorObject.DoRender(ARci, ARenderSelf, FMirrorObject.Count > 0);
        end
        else
        begin
          Scene.Objects.DoRender(ARci, ARenderSelf, True);
        end;
        if Assigned(FOnEndRenderingMirrors) then
          FOnEndRenderingMirrors(Self);

        // Restore to "normal"
        ARci.cameraPosition := cameraPosBackup;
        ARci.cameraDirection := cameraDirectionBackup;
        ARci.GLStates.CullFaceMode := cmBack;
        ARci.PipelineTransformation.ReplaceFromStack;
        Scene.SetupLights(CurrentBuffer.LimitOf[limLights]);
        ARci.PipelineTransformation.Pop;
        if moMirrorPlaneClip in MirrorOptions then
          gl.Disable(GL_CLIP_PLANE0);
        ARci.GLStates.Disable(stStencilTest);

        ARci.proxySubObject := oldProxySubObject;

        // start rendering self
        if ARenderSelf then
        begin
          Material.Apply(ARci);
          repeat
            BuildList(ARci);
          until not Material.UnApply(ARci);
        end;

      end;

    if ARenderChildren then
      Self.RenderChildren(0, Count - 1, ARci);

    if Assigned(FMirrorObject) then
      FMirrorObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
end;


procedure TGLMirror.BuildList(var ARci: TGLRenderContextInfo);
var
  hw, hh: Single;
  quadric: PGLUquadricObj;
begin
  if msRect = FShape then
  begin
    hw := FWidth * 0.5;
    hh := FHeight * 0.5;
    gl.Normal3fv(@ZVector);
    gl.Begin_(GL_QUADS);
    gl.Vertex3f(hw, hh, 0);
    gl.Vertex3f(-hw, hh, 0);
    gl.Vertex3f(-hw, -hh, 0);
    gl.Vertex3f(hw, -hh, 0);
    gl.End_;
  end
  else
  begin
    quadric := gluNewQuadric;
    gluDisk(Quadric, 0, FRadius, FSlices, 1); //radius. slices, loops
  end;
end;


procedure TGLMirror.ClearZBufferArea(aBuffer: TGLSceneBuffer);
var
  worldMat: TGLMatrix;
  p: TAffineVector;
begin
  with aBuffer do
  begin
    gl.PushMatrix;
    worldMat := Self.AbsoluteMatrix;
    gl.MatrixMode(GL_PROJECTION);
    gl.PushMatrix;
    gl.LoadIdentity;
    gl.Ortho(0, Width, 0, Height, 1, -1);
    gl.MatrixMode(GL_MODELVIEW);
    gl.LoadIdentity;

    with aBuffer.RenderingContext.GLStates do
    begin
      DepthFunc := cfAlways;
      SetGLColorWriting(False);
    end;

    gl.Begin_(GL_QUADS);
    p := WorldToScreen(VectorTransform(AffineVectorMake(Self.Width * 0.5,
      Self.Height * 0.5, 0), worldMat));
    gl.Vertex3f(p.X, p.Y, 0.999);
    p := WorldToScreen(VectorTransform(AffineVectorMake(-Self.Width * 0.5,
      Self.Height * 0.5, 0), worldMat));
    gl.Vertex3f(p.X, p.Y, 0.999);
    p := WorldToScreen(VectorTransform(AffineVectorMake(-Self.Width * 0.5,
      -Self.Height * 0.5, 0), worldMat));
    gl.Vertex3f(p.X, p.Y, 0.999);
    p := WorldToScreen(VectorTransform(AffineVectorMake(Self.Width * 0.5,
      -Self.Height * 0.5, 0), worldMat));
    gl.Vertex3f(p.X, p.Y, 0.999);
    gl.End_;

    with aBuffer.RenderingContext.GLStates do
    begin
      DepthFunc := cfLess;
      SetGLColorWriting(True);
    end;

    gl.MatrixMode(GL_PROJECTION);
    gl.PopMatrix;
    gl.MatrixMode(GL_MODELVIEW);
    gl.PopMatrix;
  end;
end;


procedure TGLMirror.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMirrorObject) then
    MirrorObject := nil;
  inherited;
end;


procedure TGLMirror.SetMirrorObject(const val: TGLBaseSceneObject);
begin
  if FMirrorObject <> val then
  begin
    if Assigned(FMirrorObject) then
      FMirrorObject.RemoveFreeNotification(Self);
    FMirrorObject := val;
    if Assigned(FMirrorObject) then
      FMirrorObject.FreeNotification(Self);
    NotifyChange(Self);
  end;
end;


procedure TGLMirror.SetWidth(AValue: Single);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    NotifyChange(Self);
  end;
end;


procedure TGLMirror.SetHeight(AValue: Single);
begin
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    NotifyChange(Self);
  end;
end;


procedure TGLMirror.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLMirror) then
  begin
    FWidth := TGLMirror(Source).FWidth;
    FHeight := TGLMirror(Source).FHeight;
    FMirrorOptions := TGLMirror(Source).FMirrorOptions;
    MirrorObject := TGLMirror(Source).MirrorObject;
  end;
  inherited Assign(Source);
end;


function TGLMirror.AxisAlignedDimensionsUnscaled: TGLVector;
begin
  Result := VectorMake(0.5 * Abs(FWidth),
    0.5 * Abs(FHeight), 0);
end;


procedure TGLMirror.SetMirrorOptions(const val: TGLMirrorOptions);
begin
  if FMirrorOptions <> val then
  begin
    FMirrorOptions := val;
    NotifyChange(Self);
  end;
end;

//ORL add-ons


procedure TGLMirror.SetRadius(const aValue: Single);
begin
  if aValue <> FRadius then
  begin
    FRadius := aValue;
    StructureChanged;
  end;
end;


function TGLMirror.GetRadius: single;
begin
  result := FRadius;
end;


procedure TGLMirror.SetSlices(const aValue: Integer);
begin
  if aValue <> FSlices then
  begin
    if aValue > 2 then
      FSlices := aValue;
    StructureChanged;
  end
  else
  begin
  end;
end;


function TGLMirror.GetSlices: Integer;
begin
  result := FSlices;
end;


procedure TGLMirror.SetShape(aValue: TMirrorShapes);
begin
  if aValue <> FShape then
  begin
    FShape := aValue;
    StructureChanged;
  end;
end;

//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

  RegisterClasses([TGLMirror]);

end.

