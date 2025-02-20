//
// The graphics engine GLXEngine. The unit of GXScene for Delphi
//
unit GXS.CUDA.Graphics;

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Classes,
  FMX.Dialogs,

  CUDA.Import,
  GXS.CUDA.API,

  GXS.Context,
  GXS.State,
  GXS.Scene,
  GXS.Graphics,
  GXS.Material,
  GXS.Texture,
  GXSL.Shader,
  GXSL.Parameter,
  GXS.RenderContextInfo;

type

  TCUDAVertexAttribute = class;
  TCUDAVertexAttributes = class;

  TOnBeforeKernelLaunch = procedure(Sender: TCUDAVertexAttribute) of object;

  TCUDAVertexAttribute = class(TCollectionItem)
  private
    FName: string;
    FType: TgxSLDataType;
    FFunc: TCUDAFunction;
    FLocation: Integer;
    FOnBeforeKernelLaunch: TOnBeforeKernelLaunch;
    procedure SetName(const AName: string);
    procedure SetType(AType: TgxSLDataType);
    procedure SetFunc(AFunc: TCUDAFunction);
    function GetLocation: Integer;
    function GetOwner: TCUDAVertexAttributes; reintroduce;
  public
    constructor Create(ACollection: TCollection); override;
    procedure NotifyChange(Sender: TObject);
    property Location: Integer read GetLocation;
  published
    property Name: string read FName write SetName;
    property GLSLType: TgxSLDataType read FType write SetType;
    property KernelFunction: TCUDAFunction read FFunc write SetFunc;
    property OnBeforeKernelLaunch: TOnBeforeKernelLaunch read
      FOnBeforeKernelLaunch write FOnBeforeKernelLaunch;
  end;

  TCUDAVertexAttributes = class(TOwnedCollection)
  private
    procedure SetItems(Index: Integer; const AValue: TCUDAVertexAttribute);
    function GetItems(Index: Integer): TCUDAVertexAttribute;
  public
   constructor Create(AOwner: TComponent);
    procedure NotifyChange(Sender: TObject);
    function MakeUniqueName(const ANameRoot: string): string;
    function GetAttributeByName(const AName: string): TCUDAVertexAttribute;
    function Add: TCUDAVertexAttribute;
    property Attributes[Index: Integer]: TCUDAVertexAttribute read GetItems
      write SetItems; default;
  end;

  TFeedBackMeshPrimitive = (fbmpPoint, fbmpLine, fbmpTriangle);
  TFeedBackMeshLaunching = (fblCommon, fblOnePerAtttribute);

  TgxCustomFeedBackMesh = class(TgxBaseSceneObject)
  private
    FGeometryResource: TCUDAGraphicResource;
    FAttributes: TCUDAVertexAttributes;
    FVAO: TgxVertexArrayHandle;
    FVBO: TgxVBOArrayBufferHandle;
    FEBO: TgxVBOElementArrayHandle;
    FPrimitiveType: TFeedBackMeshPrimitive;
    FVertexNumber: Integer;
    FElementNumber: Integer;
    FShader: TgxSLShader;
    FCommonFunc: TCUDAFunction;
    FLaunching: TFeedBackMeshLaunching;
    FBlend: Boolean;
    procedure SetAttributes(AValue: TCUDAVertexAttributes);
    procedure SetPrimitiveType(AValue: TFeedBackMeshPrimitive);
    procedure SetVertexNumber(AValue: Integer);
    procedure SetElementNumber(AValue: Integer);
    procedure SetShader(AShader: TgxSLShader);
    procedure SetCommonFunc(AFunc: TCUDAFunction);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure RefreshAttributes;
    procedure AllocateHandles;
    procedure LaunchKernels;
  protected
    property Attributes: TCUDAVertexAttributes read FAttributes write SetAttributes;
    // GXSL shader as material. If it absent or disabled - nothing be drawen.
    property Shader: TgxSLShader read FShader write SetShader;
    // Primitive type.
    property PrimitiveType: TFeedBackMeshPrimitive read FPrimitiveType
      write SetPrimitiveType default fbmpPoint;
    // Number of vertexes in array buffer.
    property VertexNumber: Integer read FVertexNumber
      write SetVertexNumber default 1;
    // Number of indexes in element buffer. Zero to disable.
    property ElementNumber: Integer read FElementNumber
      write SetElementNumber default 0;
    (* Used for all attributes and elements if Launching = fblCommon
       otherwise used own attribute function and this for elements. *)
    property CommonKernelFunction: TCUDAFunction read FCommonFunc
      write SetCommonFunc;
    (* Define mode of manufacturer launching:
      fblCommon - single launch for all,
      flOnePerAtttribute - one launch per attribute and elements *)
    property Launching: TFeedBackMeshLaunching read FLaunching
      write FLaunching default fblCommon;
    // Defines if the object uses blending for object sorting purposes.
    property Blend: Boolean read FBlend write FBlend default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TgxRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    property ArrayBufferHandle: TgxVBOArrayBufferHandle read FVBO;
    property ElementArrayHandle: TgxVBOElementArrayHandle read FEBO;
  end;

  TgxFeedBackMesh = class(TgxCustomFeedBackMesh)
  published
    property Attributes;
    property Shader;
    property PrimitiveType;
    property VertexNumber;
    property ElementNumber;
    property CommonKernelFunction;
    property Launching;
    property Blend;
    property ObjectsSorting;
    property VisibilityCulling;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property Pickable;
    property OnProgress;
    property OnPicked;
    property Behaviours;
    property Effects;
  end;

  TCUDAImageResource = class(TCUDAGraphicResource)
  private
    fMaterialLibrary: TgxMaterialLibrary;
    fTextureName: TgxLibMaterialName;
    procedure SetMaterialLibrary(const Value: TgxMaterialLibrary);
    procedure SetTextureName(const Value: TgxLibMaterialName);
  protected
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MapResources; override;
    procedure UnMapResources; override;
    procedure BindArrayToTexture(var cudaArray: TCUDAMemData;
      ALeyer, ALevel: LOngWord); override;
  published
    property TextureName: TgxLibMaterialName read fTextureName write
      SetTextureName;
    property MaterialLibrary: TgxMaterialLibrary read fMaterialLibrary write
      SetMaterialLibrary;
    property Mapping;
  end;

  TCUDAGeometryResource = class(TCUDAGraphicResource)
  private
    FFeedBackMesh: TgxCustomFeedBackMesh;
    procedure SetFeedBackMesh(const Value: TgxCustomFeedBackMesh);
    function GetAttribArraySize(AAttr: TCUDAVertexAttribute): LongWord;
  protected
    procedure AllocateHandles; override;
    procedure DestroyHandles; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function GetAttributeArraySize(const AName: string): LongWord; override;
    function GetAttributeArrayAddress(const AName: string): Pointer; override;
    function GetElementArrayDataSize: LongWord; override;
    function GetElementArrayAddress: Pointer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MapResources; override;
    procedure UnMapResources; override;
    property AttributeDataSize[const AttribName: string]: LongWord read
      GetAttributeArraySize;
    property AttributeDataAddress[const AttribName: string]: Pointer read
      GetAttributeArrayAddress;
    property IndexDataSize: LongWord read GetElementArrayDataSize;
    property IndexDataAddress: Pointer read GetElementArrayAddress;
  published
    property FeedBackMesh: TgxCustomFeedBackMesh read FFeedBackMesh write
      SetFeedBackMesh;
    property Mapping;
  end;

//-----------------------------------------
implementation
//-----------------------------------------

uses
  Stage.Strings,
  Stage.TextureFormat;


// ------------------
// ------------------ TCUDAImageResource ------------------
// ------------------

constructor TCUDAImageResource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fHandle[0] := nil;
  fResourceType := rtTexture;
  FGLContextHandle := TgxVirtualHandle.Create;
  FGLContextHandle.OnAllocate := OnGLHandleAllocate;
  FGLContextHandle.OnDestroy := OnGLHandleDestroy;
end;

destructor TCUDAImageResource.Destroy;
begin
  FGLContextHandle.Destroy;
  inherited;
end;

procedure TCUDAImageResource.SetMaterialLibrary(const Value:
  TgxMaterialLibrary);
begin
  if fMaterialLibrary <> Value then
  begin
    if Assigned(fMaterialLibrary) then
      fMaterialLibrary.RemoveFreeNotification(Self);
    fMaterialLibrary := Value;
    if Assigned(fMaterialLibrary) then
    begin
      fMaterialLibrary.FreeNotification(Self);
      if fMaterialLibrary.TextureByName(fTextureName) <> nil then
        DestroyHandles;
    end;
  end;
end;

procedure TCUDAImageResource.SetTextureName(const Value: TgxLibMaterialName);
begin
  if fTextureName <> Value then
  begin
    fTextureName := Value;
    DestroyHandles;
  end;
end;

procedure TCUDAImageResource.UnMapResources;
begin
  if FMapCounter > 0 then
    Dec(FMapCounter);

  if FMapCounter = 0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      Context.Requires;
      FStatus := cuGraphicsUnMapResources(1, @FHandle[0], nil);
      Context.Release;
      if FStatus <> CUDA_SUCCESS then
        Abort;
    end;
  end;
end;

procedure TCUDAImageResource.AllocateHandles;
const
  cMapping: array[TCUDAMapping] of TCUgraphicsMapResourceFlags = (
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD);
var
  LTexture: TgxTexture;
  glHandle: LongWord;
begin
  FGLContextHandle.AllocateHandle;

  if FGLContextHandle.IsDataNeedUpdate
    and Assigned(FMaterialLibrary)
    and (Length(FTextureName) > 0) then
  begin
    inherited;

    LTexture := FMaterialLibrary.TextureByName(FTextureName);
    if Assigned(LTexture) then
    begin
      glHandle := LTexture.AllocateHandle;
      if glHandle = 0 then
        Abort;

      Context.Requires;
      DestroyHandles;

      FStatus := cuGraphicsGLRegisterImage(
        FHandle[0],
        glHandle,
        DecodeTextureTarget(LTexture.Image.NativeTextureTarget),
        cMapping[fMapping]);

      Context.Release;

      if FStatus <> CUDA_SUCCESS then
        Abort;

      FGLContextHandle.NotifyDataUpdated;
    end;
  end;
end;

procedure TCUDAImageResource.DestroyHandles;
begin
  if Assigned(FHandle[0]) then
  begin
    inherited;
    Context.Requires;
    FStatus := cuGraphicsUnregisterResource(FHandle[0]);
    Context.Release;
    FHandle[0] := nil;
    FGLContextHandle.NotifyChangesOfData;
  end;
end;

procedure TCUDAImageResource.MapResources;
begin
  AllocateHandles;

  if FMapCounter = 0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      Context.Requires;
      FStatus := cuGraphicsMapResources(1, @FHandle[0], nil);
      Context.Release;
      if FStatus <> CUDA_SUCCESS then
        Abort;
    end;
  end;
  Inc(FMapCounter);
end;

procedure TCUDAImageResource.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  inherited;
  if (AComponent = fMaterialLibrary) and (Operation = opRemove) then
  begin
    fMaterialLibrary := nil;
    fTextureName := '';
    DestroyHandles;
  end;
end;

procedure TCUDAImageResource.BindArrayToTexture(var cudaArray: TCUDAMemData;
  ALeyer, ALevel: LOngWord);
var
  LTexture: TgxTexture;
  newArray: PCUarray;
begin
  if FMapCounter = 0 then
  begin
    ShowMessage(strFailToBindArrayToTex);
    Abort;
  end;

  Context.Requires;
  FStatus := cuGraphicsSubResourceGetMappedArray(
    newArray, FHandle[0], ALeyer, ALevel);
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  LTexture := FMaterialLibrary.TextureByName(FTextureName);
  SetArray(cudaArray, newArray, True, LTexture.TexDepth > 0);
end;

// ------------------
// ------------------ TCUDAGeometryResource ------------------
// ------------------

constructor TCUDAGeometryResource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle[0] := nil;
  FHandle[1] := nil;
  FResourceType := rtBuffer;
  FMapCounter := 0;
  FGLContextHandle := TgxVirtualHandle.Create;
  FGLContextHandle.OnAllocate := OnGLHandleAllocate;
  FGLContextHandle.OnDestroy := OnGLHandleDestroy;
end;

destructor TCUDAGeometryResource.Destroy;
begin
  FeedBackMesh := nil;
  FGLContextHandle.Destroy;
  inherited;
end;

procedure TCUDAGeometryResource.SetFeedBackMesh(const Value:
  TgxCustomFeedBackMesh);
begin
  if FFeedBackMesh <> Value then
  begin
    if Assigned(FFeedBackMesh) then
    begin
      FFeedBackMesh.RemoveFreeNotification(Self);
      FFeedBackMesh.FGeometryResource := nil;
    end;
    FFeedBackMesh := Value;
    if Assigned(FFeedBackMesh) then
    begin
      FFeedBackMesh.FreeNotification(Self);
      FFeedBackMesh.FGeometryResource := Self;
    end;
    DestroyHandles;
  end;
end;

procedure TCUDAGeometryResource.AllocateHandles;
const
  cMapping: array[TCUDAMapping] of TCUgraphicsMapResourceFlags = (
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_NONE,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_READ_ONLY,
    CU_GRAPHICS_MAP_RESOURCE_FLAGS_WRITE_DISCARD);

begin
  inherited;
  FGLContextHandle.AllocateHandle;
  if FGLContextHandle.IsDataNeedUpdate then
  begin
    if FFeedBackMesh.FVBO.IsDataNeedUpdate then
      FFeedBackMesh.AllocateHandles;

    Context.Requires;

    DestroyHandles;

    // Register vertex array
    FStatus := cuGraphicsGLRegisterBuffer(
      FHandle[0],
      FFeedBackMesh.FVBO.Handle,
      cMapping[FMapping]);

    // Register element array
    if FFeedBackMesh.ElementNumber > 0 then
      CollectStatus(
        cuGraphicsGLRegisterBuffer(
          FHandle[1],
          FFeedBackMesh.FEBO.Handle,
          cMapping[FMapping]));

    Context.Release;

    if FStatus <> CUDA_SUCCESS then
      Abort;

    FGLContextHandle.NotifyDataUpdated;
  end;
end;

procedure TCUDAGeometryResource.DestroyHandles;
begin
  if Assigned(fHandle[0]) or Assigned(fHandle[1]) then
  begin
    inherited;

    Context.Requires;

    while FMapCounter > 0 do
      UnMapResources;

    FStatus := CUDA_SUCCESS;

    if Assigned(fHandle[0]) then
    begin
      CollectStatus(cuGraphicsUnregisterResource(fHandle[0]));
      fHandle[0] := nil;
    end;

    if Assigned(fHandle[1]) then
    begin
      CollectStatus(cuGraphicsUnregisterResource(fHandle[1]));
      fHandle[1] := nil;
    end;

    Context.Release;
    FGLContextHandle.NotifyChangesOfData;
  end;
end;

procedure TCUDAGeometryResource.Notification(AComponent: TComponent;
  Operation:
  TOperation);
begin
  inherited;
  if (AComponent = FFeedBackMesh) and (Operation = opRemove) then
  begin
    FeedBackMesh := nil;
    DestroyHandles;
  end;
end;

procedure TCUDAGeometryResource.MapResources;
var
  count: Integer;
begin
  AllocateHandles;

  if FMapCounter = 0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      count := 1;
      if Assigned(FHandle[1]) then
        Inc(count);
      Context.Requires;
      FStatus := cuGraphicsMapResources(count, @FHandle[0], nil);
      Context.Release;
      if FStatus <> CUDA_SUCCESS then
        Abort;
    end;
  end;
  Inc(FMapCounter);
end;

procedure TCUDAGeometryResource.UnMapResources;
var
  count: Integer;
begin
  if FMapCounter > 0 then
    Dec(FMapCounter);

  if FMapCounter = 0 then
  begin
    if Assigned(FHandle[0]) then
    begin
      count := 1;
      if Assigned(FHandle[1]) then
        Inc(count);
      Context.Requires;
      FStatus := cuGraphicsUnMapResources(count, @FHandle[0], nil);
      Context.Release;
      if FStatus <> CUDA_SUCCESS then
        Abort;
    end;
  end;
end;

function TCUDAGeometryResource.GetAttribArraySize(AAttr: TCUDAVertexAttribute): LongWord;
var
  typeSize: LongWord;
begin
  case AAttr.GLSLType of
    GLSLType1F: typeSize := SizeOf(Single);
    GLSLType2F: typeSize := 2 * SizeOf(Single);
    GLSLType3F: typeSize := 3 * SizeOf(Single);
    GLSLType4F: typeSize := 4 * SizeOf(Single);
    GLSLType1I: typeSize := SizeOf(Integer);
    GLSLType2I: typeSize := 2 * SizeOf(Integer);
    GLSLType3I: typeSize := 3 * SizeOf(Integer);
    GLSLType4I: typeSize := 4 * SizeOf(Integer);
    GLSLType1UI: typeSize := SizeOf(Integer);
    GLSLType2UI: typeSize := 2 * SizeOf(Integer);
    GLSLType3UI: typeSize := 3 * SizeOf(Integer);
    GLSLType4UI: typeSize := 4 * SizeOf(Integer);
    GLSLTypeMat2F: typeSize := 4 * SizeOf(Single);
    GLSLTypeMat3F: typeSize := 9 * SizeOf(Single);
    GLSLTypeMat4F: typeSize := 16 * SizeOf(Single);
  else
    begin
      Assert(False, strErrorEx + strUnknownType);
      typeSize := 0;
    end;
  end;
  Result := Cardinal(FFeedBackMesh.VertexNumber) * typeSize;
end;

function TCUDAGeometryResource.GetAttributeArraySize(
  const AName: string): LongWord;
var
  LAttr: TCUDAVertexAttribute;
begin
  Result := 0;
  LAttr := FFeedBackMesh.Attributes.GetAttributeByName(AName);
  if not Assigned(LAttr) then
    exit;
  if LAttr.GLSLType = GLSLTypeUndefined then
    exit;
  Result := GetAttribArraySize(LAttr);
end;

function TCUDAGeometryResource.GetAttributeArrayAddress(
  const AName: string): Pointer;
var
  i: Integer;
  Size: Cardinal;
  MapPtr: Pointer;
  LAttr: TCUDAVertexAttribute;
begin
  Result := nil;
  if FMapCounter = 0 then
    exit;
  LAttr := FFeedBackMesh.Attributes.GetAttributeByName(AName);
  if not Assigned(LAttr) then
    exit;

  for i := 0 to LAttr.Index - 1 do
    Inc(PByte(Result), GetAttribArraySize(FFeedBackMesh.Attributes[i]));

  Context.Requires;
  MapPtr := nil;
  FStatus := cuGraphicsResourceGetMappedPointer(
    MapPtr, Size, FHandle[0]);
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  if Cardinal(Result) + GetAttribArraySize(LAttr) > Size then
  begin
    ShowMessage(strOutOfAttribSize);
    Abort;
  end;

  Inc(Pbyte(Result), Cardinal(MapPtr));
end;

function TCUDAGeometryResource.GetElementArrayDataSize: LongWord;
begin
  Result := FFeedBackMesh.ElementNumber * SizeOf(LongWord);
end;

function TCUDAGeometryResource.GetElementArrayAddress: Pointer;
var
  Size: Cardinal;
  MapPtr: Pointer;
begin
  Result := nil;
  if (FHandle[1] = nil) and (FMapCounter = 0) then
    exit;

  Context.Requires;
  MapPtr := nil;
  FStatus := cuGraphicsResourceGetMappedPointer(MapPtr, Size, FHandle[1]);
  Context.Release;

  if FStatus <> CUDA_SUCCESS then
    Abort;

  if GetElementArrayDataSize > Size then
  begin
    ShowMessage(strOutOfElementSize);
    Abort;
  end;

  Inc(Pbyte(Result), Cardinal(MapPtr));
end;

// -----------------------
// ----------------------- TCUDAVertexAttribute -------------------
// -----------------------

constructor TCUDAVertexAttribute.Create(ACollection: TCollection);
begin
  inherited;
  FName := GetOwner.MakeUniqueName('Attrib');
  FType := GLSLTypeUndefined;
  FLocation := -1;
end;

procedure TCUDAVertexAttribute.SetFunc(AFunc: TCUDAFunction);
var
  LMesh: TgxCustomFeedBackMesh;
begin
  LMesh := TgxCustomFeedBackMesh(GetOwner.GetOwner);
  if Assigned(FFunc) then
    FFunc.RemoveFreeNotification(LMesh);
  FFunc := AFunc;
  if Assigned(FFunc) then
    FFunc.FreeNotification(LMesh);
end;

procedure TCUDAVertexAttribute.SetName(const AName: string);
begin
  if AName <> FName then
  begin
    FName := '';
    FName := GetOwner.MakeUniqueName(AName);
    NotifyChange(Self);
  end;
end;

procedure TCUDAVertexAttribute.SetType(AType: TgxSLDataType);
begin
  if AType <> FType then
  begin
    FType := AType;
    NotifyChange(Self);
  end;
end;

function TCUDAVertexAttribute.GetLocation: Integer;
begin
  if FLocation < 0 then
    FLocation := glGetAttribLocation(
      CurrentContext.gxStates.CurrentProgram,
      PGLChar(String(FName)));
  Result := FLocation;
end;

function TCUDAVertexAttribute.GetOwner: TCUDAVertexAttributes;
begin
  Result := TCUDAVertexAttributes(Collection);
end;

procedure TCUDAVertexAttribute.NotifyChange(Sender: TObject);
begin
  GetOwner.NotifyChange(Self);
end;

// -----------------------
// ----------------------- TCUDAVertexAttributes -------------------
// -----------------------

function TCUDAVertexAttributes.Add: TCUDAVertexAttribute;
begin
  Result := (inherited Add) as TCUDAVertexAttribute;
end;

constructor TCUDAVertexAttributes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCUDAVertexAttribute);
end;

function TCUDAVertexAttributes.GetAttributeByName(
  const AName: string): TCUDAVertexAttribute;
var
  I: Integer;
  A: TCUDAVertexAttribute;
begin
  // Brute-force, there no need optimization
  for I := 0 to Count - 1 do
  begin
    A := TCUDAVertexAttribute(Items[i]);
    if A.Name = AName then
      Exit(A);
  end;
  Result := nil;
end;

function TCUDAVertexAttributes.GetItems(Index: Integer): TCUDAVertexAttribute;
begin
  Result := TCUDAVertexAttribute(inherited Items[index]);
end;

function TCUDAVertexAttributes.MakeUniqueName(const ANameRoot: string): string;
var
  I: Integer;
begin
  Result := ANameRoot;
  I := 1;
  while GetAttributeByName(Result) <> nil do
  begin
    Result := ANameRoot + IntToStr(I);
    Inc(I);
  end;
end;

procedure TCUDAVertexAttributes.NotifyChange(Sender: TObject);
begin
  TgxCustomFeedBackMesh(GetOwner).NotifyChange(Self);
end;

procedure TCUDAVertexAttributes.SetItems(Index: Integer;
  const AValue: TCUDAVertexAttribute);
begin
  inherited Items[index] := AValue;
end;

// -----------------------
// ----------------------- TgxCustomFeedBackMesh -------------------
// -----------------------

procedure TgxCustomFeedBackMesh.AllocateHandles;
var
  I, L: Integer;
  Size, Offset: Cardinal;
  GR: TCUDAGeometryResource;
  EnabledLocations: array[0..VERTEX_ATTR_NUM - 1] of Boolean;
begin
  FVAO.AllocateHandle;
  FVBO.AllocateHandle;
  FEBO.AllocateHandle;

  if Assigned(FGeometryResource) then
  begin
    GR := TCUDAGeometryResource(FGeometryResource);
    size := 0;
    for I := 0 to Attributes.Count - 1 do
      Inc(size, GR.GetAttribArraySize(Attributes[I]));

    FVAO.Bind;
    FVBO.BindBufferData(nil, size, GL_STREAM_DRAW);
    if FElementNumber > 0 then
      FEBO.BindBufferData(nil, GR.GetElementArrayDataSize, GL_STREAM_DRAW)
    else
      FEBO.UnBind; // Just in case

    // Predisable attributes
    for I := 0 to VERTEX_ATTR_NUM - 1 do
      EnabledLocations[I] := false;

    Offset := 0;
    for I := 0 to Attributes.Count - 1 do
    begin
      L := Attributes[I].Location;
      if L > -1 then
      begin
        EnabledLocations[I] := True;
        case Attributes[I].GLSLType of
            GLSLType1F:
              glVertexAttribPointer(L, 1, GL_FLOAT, 0, 0, pointer(Offset));

            GLSLType2F:
              glVertexAttribPointer(L, 2, GL_FLOAT, 0, 0, pointer(Offset));

            GLSLType3F:
              glVertexAttribPointer(L, 3, GL_FLOAT, 0, 0, pointer(Offset));

            GLSLType4F:
              glVertexAttribPointer(L, 4, GL_FLOAT, 0, 0, pointer(Offset));

            GLSLType1I:
              glVertexAttribIPointer(L, 1, GL_INT, 0, pointer(Offset));

            GLSLType2I:
              glVertexAttribIPointer(L, 2, GL_INT, 0, pointer(Offset));

            GLSLType3I:
              glVertexAttribIPointer(L, 3, GL_INT, 0, pointer(Offset));

            GLSLType4I:
              glVertexAttribIPointer(L, 4, GL_INT, 0, pointer(Offset));

            GLSLType1UI:
              glVertexAttribIPointer(L, 1, GL_UNSIGNED_INT, 0, pointer(Offset));

            GLSLType2UI:
              glVertexAttribIPointer(L, 2, GL_UNSIGNED_INT, 0, pointer(Offset));

            GLSLType3UI:
              glVertexAttribIPointer(L, 3, GL_UNSIGNED_INT, 0, pointer(Offset));

            GLSLType4UI:
              glVertexAttribIPointer(L, 4, GL_UNSIGNED_INT, 0, pointer(Offset));

            GLSLTypeMat2F:
              glVertexAttribPointer(L, 4, GL_FLOAT, 0, 0, pointer(Offset));

            GLSLTypeMat3F:
              glVertexAttribPointer(L, 9, GL_FLOAT, 0, 0, pointer(Offset));

            GLSLTypeMat4F:
              glVertexAttribPointer(L, 16, GL_FLOAT, 0, 0, pointer(Offset));

        end; // of case
      end;
      Inc(Offset, GR.GetAttribArraySize(Attributes[I]));
    end;

    // Enable engagement attributes array
    begin
      for I := VERTEX_ATTR_NUM - 1 downto 0 do
        if EnabledLocations[I] then
          glEnableVertexAttribArray(I)
        else
          glDisableVertexAttribArray(I);
    end;
    FVAO.UnBind;
    FVAO.NotifyDataUpdated;
  end;
end;

constructor TgxCustomFeedBackMesh.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FAttributes := TCUDAVertexAttributes.Create(Self);
  FVAO := TgxVertexArrayHandle.Create;
  FVBO := TgxVBOArrayBufferHandle.Create;
  FEBO := TgxVBOElementArrayHandle.Create;
  FPrimitiveType := fbmpPoint;
  FLaunching := fblCommon;
  FVertexNumber := 1;
  FElementNumber := 0;
  FBlend := False;
end;

destructor TgxCustomFeedBackMesh.Destroy;
begin
  Shader := nil;
  FAttributes.Destroy;
  FVAO.Destroy;
  FVBO.Destroy;
  FEBO.Destroy;
  inherited;
end;

procedure TgxCustomFeedBackMesh.LaunchKernels;
var
  i: Integer;
  GR: TCUDAGeometryResource;
//  IR: TCUDAGLImageResource;
begin

  if Assigned(FGeometryResource) then
  begin
    // Produce geometry resource
    GR := TCUDAGeometryResource(FGeometryResource);
    GR.MapResources;
    // Produce vertex attributes
    case Launching of
      fblCommon:
        begin
          for I := 0 to FAttributes.Count - 1 do
            with FAttributes.Attributes[I] do
              if Assigned(OnBeforeKernelLaunch) then
                OnBeforeKernelLaunch(FAttributes.Attributes[I]);
          if Assigned(FCommonFunc) then
            FCommonFunc.Launch;
        end;
      fblOnePerAtttribute:
        begin
          for I := 0 to FAttributes.Count - 1 do
            with FAttributes.Attributes[I] do
            begin
              if Assigned(OnBeforeKernelLaunch) then
                OnBeforeKernelLaunch(FAttributes.Attributes[I]);
              if Assigned(KernelFunction) then
                KernelFunction.Launch;
            end;
        end;
    else
      Assert(False, strErrorEx + strUnknownType);
    end;
    // Produce indexes
    if (GR.GetElementArrayDataSize > 0)
      and Assigned(FCommonFunc) then
        FCommonFunc.Launch;

    GR.UnMapResources;
  end;
end;
//    // Produce image resource
//  else if FGLResource is TCUDAGLImageResource then
//  begin
//    IR := TCUDAGLImageResource(FGLResource);
//    IR.MapResources;
//    if Assigned(FBeforeLaunch) then
//      FBeforeLaunch(Self, 0);
//    if Assigned(FManufacturer) then
//      FManufacturer.Launch;
//    IR.UnMapResources;
//  end;

procedure TgxCustomFeedBackMesh.DoRender(var ARci: TgxRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
const
  cPrimitives: array[TFeedBackMeshPrimitive] of GLEnum =
    (GL_POINTS, GL_LINES, GL_TRIANGLES);
begin
  if ARenderSelf
    and not (csDesigning in ComponentState)
    and Assigned(FShader)
    and Assigned(FGeometryResource) then
    try
      FShader.Apply(ARci, Self);
      if FVAO.IsDataNeedUpdate then
        AllocateHandles;

      // Produce mesh data
      LaunchKernels;
      // Draw mesh
      FVAO.Bind;
      // Multipass Shader Loop
      repeat
        // Render mesh
        if FElementNumber > 0 then
        begin
          glDrawElements(
            cPrimitives[FPrimitiveType],
            FElementNumber,
            GL_UNSIGNED_INT,
            nil);
        end
        else
        begin
          glDrawArrays(
            cPrimitives[FPrimitiveType],
            0,
            FVertexNumber);
        end;
      until not FShader.UnApply(ARci);
      FVAO.UnBind;
    except
      Visible := False;
    end;

  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TgxCustomFeedBackMesh.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if Operation = opRemove then
  begin
    if AComponent = Shader then
      Shader := nil
    else if AComponent = FCommonFunc then
      CommonKernelFunction := nil
    else if AComponent is TCUDAFunction then
    begin
      for I := 0 to FAttributes.Count - 1  do
        if FAttributes[I].KernelFunction = AComponent then
          FAttributes[I].KernelFunction := nil;
    end;
  end;
  inherited;
end;

procedure TgxCustomFeedBackMesh.RefreshAttributes;
var
  I: Integer;
  AttribInfo: TgxActiveAttribArray;
begin
  if Assigned(FShader) and FShader.Enabled then
  begin
    FShader.FailedInitAction := fiaSilentDisable;
    Scene.CurrentBuffer.RenderingContext.Activate;
    try
      AttribInfo := FShader.GetActiveAttribs;
    except
      FShader.Enabled := False;
      Scene.CurrentBuffer.RenderingContext.Deactivate;
      exit;
    end;
    Scene.CurrentBuffer.RenderingContext.Deactivate;
    FAttributes.Clear;
    for I := 0 to High(AttribInfo) do
    begin
      with FAttributes.Add do
      begin
        Name := AttribInfo[I].Name;
        GLSLType := AttribInfo[I].AType;
        FLocation := AttribInfo[I].Location;
      end;
    end;
    FVAO.NotifyChangesOfData;
  end;
end;

procedure TgxCustomFeedBackMesh.SetAttributes(AValue: TCUDAVertexAttributes);
begin
  FAttributes.Assign(AValue);
end;

procedure TgxCustomFeedBackMesh.SetCommonFunc(AFunc: TCUDAFunction);
begin
  if AFunc <> FCommonFunc then
  begin
    if Assigned(FCommonFunc) then
      FCommonFunc.RemoveFreeNotification(Self);
    FCommonFunc := AFunc;
    if Assigned(FCommonFunc) then
      FCommonFunc.FreeNotification(Self);
  end;
end;

procedure TgxCustomFeedBackMesh.SetElementNumber(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  FElementNumber := AValue;
  FVAO.NotifyChangesOfData;
end;

procedure TgxCustomFeedBackMesh.SetPrimitiveType(AValue: TFeedBackMeshPrimitive);
begin
  FPrimitiveType := AValue;
end;

procedure TgxCustomFeedBackMesh.SetShader(AShader: TgxslShader);
begin
  if AShader <> FShader then
  begin
    if Assigned(FShader) then
      FShader.RemoveFreeNotification(Self);
    FShader := AShader;
    if Assigned(FShader) then
      FShader.FreeNotification(Self);
    if not (csLoading in ComponentState) then
      RefreshAttributes;
  end;
end;

procedure TgxCustomFeedBackMesh.SetVertexNumber(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  FVertexNumber := AValue;
  FVAO.NotifyChangesOfData;
end;

//----------------------------------
initialization
//----------------------------------

  RegisterClasses([TCUDAImageResource, TCUDAGeometryResource,
    TgxCustomFeedBackMesh, TgxFeedBackMesh]);

end.

