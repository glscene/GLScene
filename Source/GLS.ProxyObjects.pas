//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.ProxyObjects;

(* Implements specific proxying classes *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.XCollection,
  GLS.PipelineTransformation,
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.VectorFileObjects,
  GLS.Strings,
  GLS.RenderContextInfo,
  GLS.BaseClasses,
  GLS.Material,
  GLS.Context,
  GLS.PersistentClasses,
  GLS.VectorTypes;

type
  EGLProxyException = class(Exception);

  (* A proxy object with its own color.
     This proxy object can have a unique color. Note that multi-material
     objects (Freeforms linked to a material library f.i.) won't honour
     the color. *)
  TGLColorProxy = class(TGLProxyObject)
  private
    FFrontColor: TGLFaceProperties;
    function GetMasterMaterialObject: TGLCustomSceneObject;
    procedure SetMasterMaterialObject(const Value: TGLCustomSceneObject);
    procedure SetFrontColor(AValue: TGLFaceProperties);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
    property FrontColor: TGLFaceProperties read FFrontColor write
      SetFrontColor;
    // Redeclare as TGLCustomSceneObject.
    property MasterObject: TGLCustomSceneObject read GetMasterMaterialObject
      write SetMasterMaterialObject;
  end;

  (* A proxy object with its own material.
     This proxy object can take a mesh from one master and a materia from
     a material library. *)
  TGLMaterialProxy = class(TGLProxyObject, IGLMaterialLibrarySupported)
  private
    FTempLibMaterialName: string;
    FMasterLibMaterial: TGLLibMaterial;
    FMaterialLibrary: TGLMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    function GetMasterLibMaterialName: TGLLibMaterialName;
    procedure SetMasterLibMaterialName(const Value: TGLLibMaterialName);
    function GetMasterMaterialObject: TGLCustomSceneObject;
    procedure SetMasterMaterialObject(const Value: TGLCustomSceneObject);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    (* Specifies the Material, that current master object will use.
       Provides a faster way to access FMasterLibMaterial, compared to
       MasterLibMaterialName *)
    property MasterLibMaterial: TGLLibMaterial read FMasterLibMaterial write
      FMasterLibMaterial stored False;
  published
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write
      SetMaterialLibrary;
    // Specifies the Material, that current master object will use. 
    property MasterLibMaterialName: TGLLibMaterialName read
      GetMasterLibMaterialName write SetMasterLibMaterialName;
    // Redeclare as TGLCustomSceneObject. 
    property MasterObject: TGLCustomSceneObject read GetMasterMaterialObject
      write SetMasterMaterialObject;
  end;

  // A proxy object specialized for FreeForms.  
  TGLFreeFormProxy = class(TGLProxyObject)
  private
    function GetMasterFreeFormObject: TGLFreeForm;
    procedure SetMasterFreeFormObject(const Value: TGLFreeForm);
  public
    (* If the MasterObject is a FreeForm, you can raycast against the Octree,
       which is alot faster.  You must build the octree before using. *)
    function OctreeRayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil;  intersectNormal: PGLVector = nil): Boolean;
    // WARNING: This function is not yet 100% reliable with scale+rotation. 
    function OctreeSphereSweepIntersect(const rayStart, rayVector: TGLVector;
      const velocity, radius, modelscale: Single;
      intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil): Boolean;
  published

   // Redeclare as TGLFreeForm.
    property MasterObject: TGLFreeForm read GetMasterFreeFormObject write
      SetMasterFreeFormObject;
  end;

  // An object containing the bone matrix for TGLActorProxy.  
  TBoneMatrixObj = class
  public
    Matrix: TGLMatrix;
    BoneName: string;
    BoneIndex: integer;
  end;

  (* pamLoop mode was too difficalt to implement, so it was discarded ...for now.
   pamPlayOnce only works if Actor.AnimationMode <> aamNone. *)
  TGLActorProxyAnimationMode = (pamInherited, pamNone, pamPlayOnce);

  // A proxy object specialized for Actors.  
  TGLActorProxy = class(TGLProxyObject, IGLMaterialLibrarySupported)
  private
    FCurrentFrame: Integer;
    FStartFrame: Integer;
    FEndFrame: Integer;
    FLastFrame: Integer;
    FCurrentFrameDelta: Single;
    FCurrentTime: TGLProgressTimes;
    FAnimation: TGLActorAnimationName;
    FTempLibMaterialName: string;
    FMasterLibMaterial: TGLLibMaterial;
    FMaterialLibrary: TGLMaterialLibrary;
    FBonesMatrices: TStringList;
    FStoreBonesMatrix: boolean;
    FStoredBoneNames: TStrings;
    FOnBeforeRender: TGLProgressEvent;
    FAnimationMode: TGLActorProxyAnimationMode;
    procedure SetAnimation(const Value: TGLActorAnimationName);
    procedure SetMasterActorObject(const Value: TGLActor);
    function GetMasterActorObject: TGLActor;
    function GetLibMaterialName: TGLLibMaterialName;
    procedure SetLibMaterialName(const Value: TGLLibMaterialName);
    procedure SetMaterialLibrary(const Value: TGLMaterialLibrary);
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    procedure SetStoreBonesMatrix(const Value: boolean);
    procedure SetStoredBoneNames(const Value: TStrings);
    procedure SetOnBeforeRender(const Value: TGLProgressEvent);
  protected
    procedure DoStoreBonesMatrices; // stores matrices of bones of the current frame rendered
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure DoProgress(const progressTime: TGLProgressTimes); override;
    property CurrentFrame: Integer read FCurrentFrame;
    property StartFrame: Integer read FStartFrame;
    property EndFrame: Integer read FEndFrame;
    property CurrentFrameDelta: Single read FCurrentFrameDelta;
    property CurrentTime: TGLProgressTimes read FCurrentTime;
    (* Gets the Bones Matrix in the current animation frame.
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) *)
    function BoneMatrix(BoneIndex: integer): TGLMatrix; overload;
    function BoneMatrix(BoneName: string): TGLMatrix; overload;
    procedure BoneMatricesClear;
    // A standard version of the RayCastIntersect function. 
    function RayCastIntersect(const rayStart, rayVector: TGLVector;
      intersectPoint: PGLVector = nil;
      intersectNormal: PGLVector = nil): Boolean; override;
    (* Raycasts on self, but actually on the "RefActor" Actor.
       Note that the "RefActor" parameter does not necessarily have to be
       the same Actor refernced by the MasterObject property:
       This allows to pass a low-low-low-poly Actor to raycast in the "RefActor" parameter,
       while using a high-poly Actor in the "MasterObject" property,
       of course we assume that the two Masterobject Actors have same animations.  *)
    function RayCastIntersectEx(RefActor: TGLActor; const rayStart, rayVector:
      TGLVector;
      intersectPoint: PGLVector = nil;
      intersectNormal: PGLVector = nil): Boolean; overload;
  published
    property AnimationMode: TGLActorProxyAnimationMode read FAnimationMode write FAnimationMode default pamInherited;
    property Animation: TGLActorAnimationName read FAnimation write SetAnimation;
    // Redeclare as TGLActor.
    property MasterObject: TGLActor read GetMasterActorObject write SetMasterActorObject;
    (* Redeclare without pooTransformation
       (Don't know why it causes the object to be oriented incorrecly.) *)
    property ProxyOptions default [pooEffects, pooObjects];
    // Specifies the MaterialLibrary, that current proxy will use. 
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    // Specifies the Material, that current proxy will use. 
    property LibMaterialName: TGLLibMaterialName read GetLibMaterialName write SetLibMaterialName;
    (* Specifies if it will store the Bones Matrices, accessible via the BoneMatrix function
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) *)
    property StoreBonesMatrix: boolean read FStoreBonesMatrix write SetStoreBonesMatrix;
    (* Specifies the names of the bones we want the matrices to be stored. If empty, all bones will be stored
     (since the masterobject is shared between all proxies, each proxy will have it's bones matrices) *)
    property StoredBoneNames: TStrings read FStoredBoneNames write SetStoredBoneNames;
    (* Event allowing to apply extra transformations (f.ex: bone rotations) to the referenced
       Actor on order to have the proxy render these changes. *)
    property OnBeforeRender: TGLProgressEvent read FOnBeforeRender write SetOnBeforeRender;
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------


// ------------------
// ------------------ TGLColorProxy ------------------
// ------------------


constructor TGLColorProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFrontColor := TGLFaceProperties.Create(Self);
end;

destructor TGLColorProxy.Destroy;
begin
  FFrontColor.Free;

  inherited Destroy;
end;

procedure TGLColorProxy.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    gotMaster := Assigned(MasterObject);
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions)
      and (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          gl.MultMatrixf(PGLFloat(MasterObject.Matrix));
        GetMasterMaterialObject.Material.FrontProperties.Assign(FFrontColor);
        MasterObject.DoRender(ARci, ARenderSelf, MasterObject.Count > 0);
        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

function TGLColorProxy.GetMasterMaterialObject: TGLCustomSceneObject;
begin
  Result := TGLCustomSceneObject(inherited MasterObject);
end;

procedure TGLColorProxy.SetFrontColor(AValue: TGLFaceProperties);
begin
  FFrontColor.Assign(AValue);
end;

procedure TGLColorProxy.SetMasterMaterialObject(
  const Value: TGLCustomSceneObject);
begin
  inherited SetMasterObject(Value);
end;

// ------------------
// ------------------ TGLFreeFormProxy ------------------
// ------------------

function TGLFreeFormProxy.OctreeRayCastIntersect(const rayStart, rayVector:
  TGLVector;
  intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): Boolean;
var
  localRayStart, localRayVector: TGLVector;
begin
  if Assigned(MasterObject) then
  begin
    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := GetMasterFreeFormObject.OctreeRayCastIntersect(localRayStart,
      localRayVector,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^,
          MasterObject.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^,
          MasterObject.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;
  end
  else
    Result := False;
end;

function TGLFreeFormProxy.OctreeSphereSweepIntersect(const rayStart, rayVector:
  TGLVector;
  const velocity, radius, modelscale: Single;
  intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): Boolean;
var
  localRayStart, localRayVector: TGLVector;
  localVelocity, localRadius: single;
begin
  Result := False;
  if Assigned(MasterObject) then
  begin
    localVelocity := velocity * modelscale;
    localRadius := radius * modelscale;

    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, MasterObject.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, MasterObject.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := GetMasterFreeFormObject.OctreeSphereSweepIntersect(localRayStart,
      localRayVector,
      localVelocity, localRadius,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^,
          MasterObject.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^,
          MasterObject.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;

  end;
end;

function TGLFreeFormProxy.GetMasterFreeFormObject: TGLFreeForm;
begin
  Result := TGLFreeForm(inherited MasterObject);
end;

procedure TGLFreeFormProxy.SetMasterFreeFormObject(
  const Value: TGLFreeForm);
begin
  inherited SetMasterObject(Value);
end;

// ------------------
// ------------------ TGLActorProxy ------------------
// ------------------

function TGLActorProxy.BoneMatrix(BoneIndex: integer): TGLMatrix;
begin
  if BoneIndex < FBonesMatrices.count then
    result := TBoneMatrixObj(FBonesMatrices.Objects[BoneIndex]).Matrix;
end;

function TGLActorProxy.BoneMatrix(BoneName: string): TGLMatrix;
var
  i: Integer;
begin
  i := FBonesMatrices.IndexOf(BoneName);
  if i > -1 then
    result := TBoneMatrixObj(FBonesMatrices.Objects[i]).Matrix;
end;

procedure TGLActorProxy.BoneMatricesClear;
var
  i: Integer;
begin
  for i := 0 to FBonesMatrices.Count - 1 do
  begin
    TBoneMatrixObj(FBonesMatrices.Objects[i]).free;
  end;
  FBonesMatrices.Clear;
end;

constructor TGLActorProxy.Create(AOwner: TComponent);
begin
  inherited;
  FAnimationMode := pamInherited;
  ProxyOptions := ProxyOptions - [pooTransformation];
  FBonesMatrices := TStringList.create;
  FStoredBoneNames := TStringList.create;
  FStoreBonesMatrix := false;
    // default is false to speed up a little if we don't need bones info
end;

destructor TGLActorProxy.Destroy;
begin
  BoneMatricesClear;
  FBonesMatrices.free;
  FStoredBoneNames.free;
  inherited;
end;

procedure TGLActorProxy.DoProgress(const progressTime: TGLProgressTimes);
begin
  inherited;
  FCurrentTime := progressTime;
end;

procedure TGLActorProxy.DoRender(var ARci: TGLRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
var
  // TGLActorProxy specific
  cf, sf, ef: Integer;
  cfd: Single;
  // General proxy stuff.
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
  MasterActor: TGLActor;
begin
  try
    MasterActor := GetMasterActorObject;
    gotMaster := MasterActor <> nil;
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions) and
      (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          with ARci.PipelineTransformation do
            SetModelMatrix(MatrixMultiply(MasterActor.Matrix^, ModelMatrix^));

        // At last TGLActorProxy specific stuff!
        with MasterActor do
        begin
          cfd := CurrentFrameDelta;
          cf := CurrentFrame;
          sf := startframe;
          ef := endframe;

          case FAnimationMode of
            pamInherited: CurrentFrameDelta := FCurrentFrameDelta;
            pamPlayOnce:
              begin
                if (FLastFrame <> FEndFrame - 1) then
                  CurrentFrameDelta := FCurrentFrameDelta
                else
                begin
                  FCurrentFrameDelta := 0;
                  FAnimationMode := pamNone;
                end;
              end;
            pamNone: CurrentFrameDelta := 0;
          else
            Assert(False, strUnknownType);
          end;

          SetCurrentFrameDirect(FCurrentFrame);
          FLastFrame := FCurrentFrame;
          StartFrame := FStartFrame;
          EndFrame := FEndFrame;

          if (FMasterLibMaterial <> nil) and (FMaterialLibrary <> nil) then
            MasterActor.Material.QuickAssignMaterial(
              FMaterialLibrary, FMasterLibMaterial);

          DoProgress(FCurrentTime);

          if Assigned(FOnBeforeRender) then
            FOnBeforeRender(self, FCurrentTime.deltaTime, FCurrentTime.newTime);

          DoRender(ARci, ARenderSelf, Count > 0);

          // Stores Bones matrices of the current frame
          if (FStoreBonesMatrix) and (MasterActor.Skeleton <> nil) then
            DoStoreBonesMatrices;

          FCurrentFrameDelta := CurrentFrameDelta;
          FCurrentFrame := CurrentFrame;
          CurrentFrameDelta := cfd;
          SetCurrentFrameDirect(cf);
          startframe := sf;
          endframe := ef;
        end;

        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    oldProxySubObject := ARci.proxySubObject;
    ARci.proxySubObject := True;
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterActor.Effects.RenderPostEffects(ARci);
    ARci.proxySubObject := oldProxySubObject;
  finally
    ClearStructureChanged;
  end;
end;

procedure TGLActorProxy.DoStoreBonesMatrices;
var
  i, n: integer;
  Bmo: TBoneMatrixObj;
  Bone: TGLSkeletonBone;
begin
  if FStoredBoneNames.count > 0 then
  begin
    // If we specified some bone names, only those bones matrices will be stored (save some cpu)
    if FBonesMatrices.Count < FStoredBoneNames.Count then
    begin
      n := FBonesMatrices.Count;
      for i := n to FStoredBoneNames.Count - 1 do
      begin
        Bone := MasterObject.Skeleton.BoneByName(FStoredBoneNames[i]);
        if Bone <> nil then
        begin
          Bmo := TBoneMatrixObj.Create;
          Bmo.BoneName := Bone.Name;
          Bmo.BoneIndex := Bone.BoneID;
          FBonesMatrices.AddObject(Bone.Name, Bmo);
        end;

      end;
    end;
  end
  else
  begin
    // Add (missing) TBoneMatrixObjects (actually ony 1st time) from all bones in skeleton
    if FBonesMatrices.Count < MasterObject.Skeleton.BoneCount - 1 then
      // note : BoneCount actually returns 1 count more.
    begin
      n := FBonesMatrices.Count;
      for i := n to MasterObject.Skeleton.BoneCount - 2 do
        // note : BoneCount actually returns 1 count more.
      begin
        Bone := MasterObject.Skeleton.BoneByID(i);
        if Bone <> nil then
        begin
          Bmo := TBoneMatrixObj.Create;
          Bmo.BoneName := Bone.Name;
          Bmo.BoneIndex := Bone.BoneID;
          FBonesMatrices.AddObject(Bone.Name, Bmo);
        end;

      end;
    end;
  end;

  // fill FBonesMatrices list
  for i := 0 to FBonesMatrices.count - 1 do
  begin
    Bmo := TBoneMatrixObj(FBonesMatrices.Objects[i]);
    Bmo.Matrix := MasterObject.Skeleton.BoneByID(Bmo.BoneIndex).GlobalMatrix;
  end;
end;

function TGLActorProxy.GetMasterActorObject: TGLActor;
begin
  Result := TGLActor(inherited MasterObject);
end;

function TGLActorProxy.GetLibMaterialName: TGLLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;

function TGLActorProxy.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLActorProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil;
  end;
end;

function TGLActorProxy.RayCastIntersect(const rayStart, rayVector: TGLVector;
  intersectPoint, intersectNormal: PGLVector): Boolean;
begin
  if MasterObject <> nil then
    Result := RayCastIntersectEx(GetMasterActorObject, rayStart, rayVector,
      intersectPoint, intersectNormal)
  else
    Result := inherited RayCastIntersect(rayStart, rayVector, intersectPoint,
      intersectNormal);
end;

// Gain access to TGLDummyActor.DoAnimate().
type
  TGLDummyActor = class(TGLActor);

function TGLActorProxy.RayCastIntersectEx(RefActor: TGLActor; const rayStart,
  rayVector: TGLVector; intersectPoint, intersectNormal: PGLVector): Boolean;
var
  localRayStart, localRayVector: TGLVector;
  cf, sf, ef: Integer;
  cfd: Single;
  HaspooTransformation: boolean;
begin
  // Set RefObject frame as current ActorProxy frame
  with RefActor do
  begin
    // VARS FOR ACTOR TO ASSUME ACTORPROXY CURRENT ANIMATION FRAME
    cfd := RefActor.CurrentFrameDelta;
    cf := RefActor.CurrentFrame;
    sf := RefActor.startframe;
    ef := RefActor.endframe;
    RefActor.CurrentFrameDelta := self.CurrentFrameDelta;
    RefActor.SetCurrentFrameDirect(self.CurrentFrame);
    RefActor.StartFrame := self.StartFrame;
    RefActor.EndFrame := self.EndFrame;
    RefActor.CurrentFrame := self.CurrentFrame;

    // FORCE ACTOR TO ASSUME ACTORPROXY CURRENT ANIMATION FRAME
    TGLDummyActor(RefActor).DoAnimate();

    HaspooTransformation := pooTransformation in self.ProxyOptions;

    // transform RAYSTART
    SetVector(localRayStart, self.AbsoluteToLocal(rayStart));
    if not HaspooTransformation then
      SetVector(localRayStart, RefActor.LocalToAbsolute(localRayStart));

    // transform RAYVECTOR
    SetVector(localRayVector, self.AbsoluteToLocal(rayVector));
    if not HaspooTransformation then
      SetVector(localRayVector, RefActor.LocalToAbsolute(localRayVector));

    NormalizeVector(localRayVector);

    Result := RefActor.RayCastIntersect(localRayStart, localRayVector,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        if not HaspooTransformation then
          SetVector(intersectPoint^, RefActor.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, self.LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        if not HaspooTransformation then
          SetVector(intersectNormal^,
            RefActor.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, self.LocalToAbsolute(intersectNormal^));
      end;
    end;

    // Return RefObject to it's old time
    CurrentFrameDelta := cfd;
    SetCurrentFrameDirect(cf);
    CurrentFrame := cf;
    startframe := sf;
    endframe := ef;

    // REVERT ACTOR TO ASSUME ORIGINAL ANIMATION FRAME
    TGLDummyActor(RefActor).DoAnimate();
  end;
end;

procedure TGLActorProxy.SetAnimation(const Value: TGLActorAnimationName);
var
  anAnimation: TGLActorAnimation;
begin
  // We first assign the value (for persistency support), then check it.
  FAnimation := Value;

  if Assigned(MasterObject) then
  begin
    anAnimation := GetMasterActorObject.Animations.FindName(Value);
    if Assigned(anAnimation) then
    begin
      FStartFrame := anAnimation.StartFrame;
      FEndFrame := anAnimation.EndFrame;
      FCurrentFrame := FStartFrame;
      FLastFrame := FCurrentFrame;
    end;
  end;
end;

procedure TGLActorProxy.SetStoredBoneNames(const Value: TStrings);
begin
  if value <> nil then
    FStoredBoneNames.Assign(Value);
end;

procedure TGLActorProxy.SetMasterActorObject(const Value: TGLActor);
begin
  inherited SetMasterObject(Value);
  BoneMatricesClear;
end;

procedure TGLActorProxy.SetLibMaterialName(
  const Value: TGLLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    if not (csLoading in ComponentState) then
      raise ETexture.Create(strErrorEx + strMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

procedure TGLActorProxy.SetMaterialLibrary(const Value: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FTempLibMaterialName <> '' then
        SetLibMaterialName(FTempLibMaterialName);
    end
    else
    begin
      FTempLibMaterialName := '';
    end;
  end;
end;

procedure TGLActorProxy.SetOnBeforeRender(const Value: TGLProgressEvent);
begin
  FOnBeforeRender := Value;
end;

procedure TGLActorProxy.SetStoreBonesMatrix(const Value: boolean);
begin
  FStoreBonesMatrix := Value;
end;

{ TGLMaterialProxy }

constructor TGLMaterialProxy.Create(AOwner: TComponent);
begin
  inherited;
  // Nothing here.
end;

destructor TGLMaterialProxy.Destroy;
begin
  // Nothing here.
  inherited;
end;

procedure TGLMaterialProxy.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  gotMaster, masterGotEffects, oldProxySubObject: Boolean;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    gotMaster := Assigned(MasterObject);
    masterGotEffects := gotMaster and (pooEffects in ProxyOptions)
      and (MasterObject.Effects.Count > 0);
    if gotMaster then
    begin
      if pooObjects in ProxyOptions then
      begin
        oldProxySubObject := ARci.proxySubObject;
        ARci.proxySubObject := True;
        if pooTransformation in ProxyOptions then
          gl.MultMatrixf(PGLFloat(MasterObject.Matrix));

        if (FMasterLibMaterial <> nil) and (FMaterialLibrary <> nil) then
          GetMasterMaterialObject.Material.QuickAssignMaterial(
            FMaterialLibrary, FMasterLibMaterial);

        MasterObject.DoRender(ARci, ARenderSelf, MasterObject.Count > 0);
        ARci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if ARenderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, ARci);
    if masterGotEffects then
      MasterObject.Effects.RenderPostEffects(ARci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

function TGLMaterialProxy.GetMasterLibMaterialName: TGLLibMaterialName;
begin
  Result := FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;

function TGLMaterialProxy.GetMasterMaterialObject: TGLCustomSceneObject;
begin
  Result := TGLCustomSceneObject(inherited MasterObject);
end;

function TGLMaterialProxy.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLMaterialProxy.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      FMaterialLibrary := nil;
  end;
end;

procedure TGLMaterialProxy.SetMasterLibMaterialName(
  const Value: TGLLibMaterialName);
begin
  if FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    if not (csLoading in ComponentState) then
      raise ETexture.Create(strErrorEx + strMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

procedure TGLMaterialProxy.SetMasterMaterialObject(
  const Value: TGLCustomSceneObject);
begin
  inherited SetMasterObject(Value);
end;

procedure TGLMaterialProxy.SetMaterialLibrary(
  const Value: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FTempLibMaterialName <> '' then
        SetMasterLibMaterialName(FTempLibMaterialName);
    end
    else
    begin
      FTempLibMaterialName := '';
    end;
  end;
end;

//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

  RegisterClasses([TGLColorProxy, TGLFreeFormProxy, TGLActorProxy,
    TGLMaterialProxy]);

end.

