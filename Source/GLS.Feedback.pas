//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Feedback;

(*
   A scene object encapsulating the OpenGL feedback buffer.
   This object, when Active, will render it's children using
   the GL_FEEDBACK render mode. This will render the children
   into the feedback Buffer rather than into the frame buffer.

   Mesh data can be extracted from the buffer using the
   BuildMeshFromBuffer procedure. For custom parsing of the
   buffer use the Buffer SingleList. The Buffered property
   will indicate if there is valid data in the buffer.
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.VectorGeometry,
  GLS.PersistentClasses,
  GLS.PipelineTransformation,
  GLS.VectorLists,
  GLS.Scene,
  GLS.VectorFileObjects,
  GLS.Texture,
  GLS.RenderContextInfo,
  GLS.Context,
  GLS.State,
  GLS.MeshUtils,
  GLS.VectorTypes;

type
  TGLFeedbackMode = (fm2D, fm3D, fm3DColor, fm3DColorTexture, fm4DColorTexture);

  // An object encapsulating the OpenGL feedback rendering mode.
  TGLFeedback = class(TGLBaseSceneObject)
  private
    FActive: Boolean;
    FBuffer: TGLSingleList;
    FMaxBufferSize: Cardinal;
    FBuffered: Boolean;
    FCorrectionScaling: Single;
    FMode: TGLFeedbackMode;
  protected
    procedure SetMaxBufferSize(const Value: Cardinal);
    procedure SetMode(const Value: TGLFeedbackMode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    (* Parse the the feedback buffer for polygon data and build
       a mesh into the assigned lists. *)
    procedure BuildMeshFromBuffer(
      Vertices: TGLAffineVectorList = nil;
      Normals: TGLAffineVectorList = nil;
      Colors: TGLVectorList = nil;
      TexCoords: TGLAffineVectorList = nil;
      VertexIndices: TGLIntegerList = nil);
    // True when there is data in the buffer ready for parsing
    property Buffered: Boolean read FBuffered;
    // The feedback buffer
    property Buffer: TGLSingleList read FBuffer;
    (* Vertex positions in the buffer needs to be scaled by
       CorrectionScaling to get correct coordinates. *)
    property CorrectionScaling: Single read FCorrectionScaling;
  published
    // Maximum size allocated for the feedback buffer
    property MaxBufferSize: Cardinal read FMaxBufferSize write SetMaxBufferSize;
    // Toggles the feedback rendering
    property Active: Boolean read FActive write FActive;
    // The type of data that is collected in the feedback buffer
    property Mode: TGLFeedbackMode read FMode write SetMode;
    property Visible;
  end;

// ----------------------------------------------------------------------
implementation
// ----------------------------------------------------------------------

// ----------
// ---------- TGLFeedback ----------
// ----------

constructor TGLFeedback.Create(AOwner: TComponent);
begin
  inherited;
  FMaxBufferSize := $100000;
  FBuffer := TGLSingleList.Create;
  FBuffer.Capacity := FMaxBufferSize div SizeOf(Single);
  FBuffered := False;
  FActive := False;
  FMode := fm3DColorTexture;
end;

destructor TGLFeedback.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TGLFeedback.DoRender(var ARci: TglRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);

  function RecursChildRadius(obj: TglBaseSceneObject): Single;
  var
    i: Integer;
    childRadius: Single;
  begin
    childRadius := 0;
    Result := obj.BoundingSphereRadius + VectorLength(obj.AbsolutePosition);
    for i := 0 to obj.Count - 1 do
      childRadius := RecursChildRadius(obj.Children[i]);
    if childRadius > Result then
      Result := childRadius;
  end;

var
  i: integer;
  radius: Single;
  atype: cardinal;
begin
  FBuffer.Count := 0;
  try
    if (csDesigning in ComponentState) or not Active then
      exit;
    if not ARenderChildren then
      exit;

    FCorrectionScaling := 1.0;
    for i := 0 to Count - 1 do
    begin
      radius := RecursChildRadius(Children[i]);
      if radius > FCorrectionScaling then
        FCorrectionScaling := radius + 1e-5;
    end;

    case FMode of
      fm2D: aType := GL_2D;
      fm3D: aType := GL_3D;
      fm3DColor: aType := GL_3D_COLOR;
      fm3DColorTexture: aType := GL_3D_COLOR_TEXTURE;
      fm4DColorTexture: aType := GL_4D_COLOR_TEXTURE;
    else
      aType := GL_3D_COLOR_TEXTURE;
    end;

    FBuffer.Count := FMaxBufferSize div SizeOf(Single);
    gl.FeedBackBuffer(FMaxBufferSize, atype, @FBuffer.List[0]);
    ARci.GLStates.Disable(stCullFace);
    ARci.ignoreMaterials := FMode < fm3DColor;
    ARci.PipelineTransformation.Push;
    ARci.PipelineTransformation.SetProjectionMatrix(IdentityHmgMatrix);
    ARci.PipelineTransformation.SetViewMatrix(
      CreateScaleMatrix(VectorMake(
        1.0 / FCorrectionScaling,
        1.0 / FCorrectionScaling,
        1.0 / FCorrectionScaling)));
    ARci.GLStates.ViewPort := Vector4iMake(-1, -1, 2, 2);
    gl.RenderMode(GL_FEEDBACK);

    Self.RenderChildren(0, Count - 1, ARci);

    FBuffer.Count := gl.RenderMode(GL_RENDER);
    ARci.PipelineTransformation.Pop;

  finally
    ARci.ignoreMaterials := False;
    FBuffered := (FBuffer.Count > 0);
    if ARenderChildren then
      Self.RenderChildren(0, Count - 1, ARci);
  end;
  ARci.GLStates.ViewPort :=
    Vector4iMake(0, 0, ARci.viewPortSize.cx, ARci.viewPortSize.cy);
end;

procedure TGLFeedback.BuildMeshFromBuffer(
  Vertices: TGLAffineVectorList = nil;
  Normals: TGLAffineVectorList = nil;
  Colors: TGLVectorList = nil;
  TexCoords: TGLAffineVectorList = nil;
  VertexIndices: TGLIntegerList = nil);
var
  value: Single;
  i, j, LCount, skip: Integer;
  vertex, color, texcoord: TGLVector;
  tempVertices, tempNormals, tempTexCoords: TGLAffineVectorList;
  tempColors: TGLVectorList;
  tempIndices: TGLIntegerList;
  ColorBuffered, TexCoordBuffered: Boolean;
begin
  Assert(FMode <> fm2D, 'Cannot build mesh from fm2D feedback mode.');

  tempVertices := TGLAffineVectorList.Create;
  tempColors := TGLVectorList.Create;
  tempTexCoords := TGLAffineVectorList.Create;

  ColorBuffered := (FMode = fm3DColor) or
    (FMode = fm3DColorTexture) or
    (FMode = fm4DColorTexture);
  TexCoordBuffered := (FMode = fm3DColorTexture) or
    (FMode = fm4DColorTexture);

  i := 0;

  skip := 3;
  if FMode = fm4DColorTexture then
    Inc(skip, 1);
  if ColorBuffered then
    Inc(skip, 4);
  if TexCoordBuffered then
    Inc(skip, 4);

  while i < FBuffer.Count - 1 do
  begin
    value := FBuffer[i];
    if value = GL_POLYGON_TOKEN then
    begin
      Inc(i);
      value := FBuffer[i];
      LCount := Round(value);
      Inc(i);
      if LCount = 3 then
      begin
        for j := 0 to 2 do
        begin
          vertex.X := FBuffer[i];
          Inc(i);
          vertex.Y := FBuffer[i];
          Inc(i);
          vertex.Z := FBuffer[i];
          Inc(i);
          if FMode = fm4DColorTexture then
            Inc(i);
          if ColorBuffered then
          begin
            color.X := FBuffer[i];
            Inc(i);
            color.Y := FBuffer[i];
            Inc(i);
            color.Z := FBuffer[i];
            Inc(i);
            color.W := FBuffer[i];
            Inc(i);
          end;
          if TexCoordBuffered then
          begin
            texcoord.X := FBuffer[i];
            Inc(i);
            texcoord.Y := FBuffer[i];
            Inc(i);
            texcoord.Z := FBuffer[i];
            Inc(i);
            texcoord.W := FBuffer[i];
            Inc(i);
          end;

          vertex.Z := 2 * vertex.Z - 1;
          ScaleVector(vertex, FCorrectionScaling);

          tempVertices.Add(AffineVectorMake(vertex));
          tempColors.Add(color);
          tempTexCoords.Add(AffineVectorMake(texcoord));
        end;
      end
      else
      begin
        Inc(i, skip * LCount);
      end;
    end
    else
    begin
      Inc(i);
    end;
  end;

  if Assigned(VertexIndices) then
  begin
    tempIndices := BuildVectorCountOptimizedIndices(tempVertices, nil, nil);
    RemapAndCleanupReferences(tempVertices, tempIndices);
    VertexIndices.Assign(tempIndices);
  end
  else
  begin
    tempIndices := TGLIntegerList.Create;
    tempIndices.AddSerie(0, 1, tempVertices.Count);
  end;

  tempNormals := BuildNormals(tempVertices, tempIndices);

  if Assigned(Vertices) then
    Vertices.Assign(tempVertices);
  if Assigned(Normals) then
    Normals.Assign(tempNormals);
  if Assigned(Colors) and ColorBuffered then
    Colors.Assign(tempColors);
  if Assigned(TexCoords) and TexCoordBuffered then
    TexCoords.Assign(tempTexCoords);

  tempVertices.Destroy;
  tempNormals.Destroy;
  tempColors.Destroy;
  tempTexCoords.Destroy;
  tempIndices.Destroy;
end;

procedure TGLFeedback.SetMaxBufferSize(const Value: Cardinal);
begin
  if Value <> FMaxBufferSize then
  begin
    FMaxBufferSize := Value;
    FBuffered := False;
    FBuffer.Count := 0;
    FBuffer.Capacity := FMaxBufferSize div SizeOf(Single);
  end;
end;

procedure TGLFeedback.SetMode(const Value: TGLFeedbackMode);
begin
  if Value <> FMode then
  begin
    FMode := Value;
    FBuffered := False;
    FBuffer.Count := 0;
  end;
end;

//------------------------------------------------
initialization
//------------------------------------------------

  RegisterClasses([TGLFeedback]);

end.


