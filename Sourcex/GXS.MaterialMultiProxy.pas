//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.MaterialMultiProxy;

(*
   Implements a multi-proxy object, useful for discreet LOD.
   Allows assign a unique material for each proxy master.

  What changed compared to GLMultiProxy:
    1) Allows assign a unique material for each proxy master
    2) TgxMaterialMultiProxyMaster: FDistanceMin, FDistanceMax removed
    3) TgxMaterialMultiProxy = class(TgxBaseSceneObject)!!!
    4) TgxMaterialMultiProxyMaster.Visible removed
    5) TgxMaterialMultiProxy.MaterialLibrary added
    6) TgxMaterialMultiProxyMaster.MasterLibMaterial added
    7) TgxMaterialMultiProxyMasters.Add overloaded
    8) Implemented a new mechanizm of connecting TgxLibMaterial and TgxLibMaterialName
      (they are connected on assigning, not while rendering; full persistency support;
       allows to assign directly to TgxLibMaterial)
    9) FMX-style code formating
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,

  GXS.BaseClasses,
  GXS.PersistentClasses,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.Strings,
  GXS.Texture,
  GXS.Material,
  GXS.Silhouette,
  GXS.Scene,
  GXS.RenderContextInfo,
  GXS.Context,
  GXS.PipelineTransformation;

type

  TgxMaterialMultiProxy = class;

  // MasterObject description for a MultiProxy object.
  TgxMaterialMultiProxyMaster = class(TgxInterfacedCollectionItem, IgxMaterialLibrarySupported)
  private
    FMasterObject: TgxBaseSceneObject;
    FMasterLibMaterial: TgxLibMaterial;
    FTempLibMaterialName: TgxLibMaterialName;
    FDistanceMin2, FDistanceMax2: Single;
    procedure SetMasterLibMaterialName(const Value: TgxLibMaterialName);
    function GetMasterLibMaterialName: TgxLibMaterialName;
    // Implementing IGLMaterialLibrarySupported.
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
  protected
    function GetDisplayName: string; override;
    procedure SetMasterObject(const Val: TgxBaseSceneObject);
    procedure SetDistanceMin(const Val: Single);
    procedure SetDistanceMax(const Val: Single);
    function GetDistanceMin: Single;
    function GetDistanceMax: Single;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function OwnerObject: TgxMaterialMultiProxy;
    procedure NotifyChange;
    { Specifies the Material, that current master object will use.
       Provides a faster way to access FMasterLibMaterial, compared to
       MasterLibMaterialName }
    property MasterLibMaterial: TgxLibMaterial read FMasterLibMaterial write FMasterLibMaterial stored False;
  published
    { Specifies the Master object which will be proxy'ed. }
    property MasterObject: TgxBaseSceneObject read FMasterObject write SetMasterObject;
    { Specifies the Material, that current master object will use. }
    property MasterLibMaterialName: TgxLibMaterialName read GetMasterLibMaterialName write SetMasterLibMaterialName;
    { Minimum visibility Distance (inclusive). }
    property DistanceMin: Single read GetDistanceMin write SetDistanceMin;
    { Maximum visibility Distance (exclusive). }
    property DistanceMax: Single read GetDistanceMax write SetDistanceMax;
  end;

  { Collection of TgxMaterialMultiProxyMaster. }
  TgxMaterialMultiProxyMasters = class(TOwnedCollection)
  protected
    procedure SetItems(index: Integer; const Val: TgxMaterialMultiProxyMaster);
    function GetItems(index: Integer): TgxMaterialMultiProxyMaster;
    procedure Update(Item: TCollectionItem); override;
    procedure Notification(AComponent: TComponent); virtual;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TgxMaterialMultiProxyMaster; overload;
    function Add(Master: TgxBaseSceneObject; DistanceMin, DistanceMax: Single): TgxMaterialMultiProxyMaster; overload;
    function Add(Master: TgxBaseSceneObject; MasterLibMaterial: TgxLibMaterial; DistanceMin, DistanceMax: Single): TgxMaterialMultiProxyMaster; overload;
    property Items[index: Integer]: TgxMaterialMultiProxyMaster read GetItems write SetItems; default;
    procedure NotifyChange;
    procedure EndUpdate; override;
  end;

   { Multiple Proxy object. 
      This proxy has multiple Master objects, which are individually made visible
      depending on a Distance to the camera criterion. It can be used to implement
      discreet level of detail directly for static objects, or objects that
      go through cyclic animation. 
      For dimensionsn raycasting and silhouette purposes, the first Master is used
      (item zero in the MasterObjects collection). }
  TgxMaterialMultiProxy = class(TgxBaseSceneObject)
  private
    FMasterObjects: TgxMaterialMultiProxyMasters;
    FRendering: Boolean; // internal use (loop protection)
    FMaterialLibrary: TgxMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TgxMaterialLibrary);
  protected
    procedure SetMasterObjects(const Val: TgxMaterialMultiProxyMasters);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function PrimaryMaster: TgxBaseSceneObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var rci: TgxRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    function RayCastIntersect(const rayStart, rayVector: TVector4f; intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean; override;
    function GenerateSilhouette(const silhouetteParameters: TgxSilhouetteParameters): TgxSilhouette; override;
  published
    property MasterObjects: TgxMaterialMultiProxyMasters read FMasterObjects write SetMasterObjects;
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property ObjectsSorting;
    property Direction;
    property PitchAngle;
    property Position;
    property RollAngle;
    property Scale;
    property ShowAxes;
    property TurnAngle;
    property Up;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

// ------------------
// ------------------ TgxMaterialMultiProxyMaster ------------------
// ------------------

constructor TgxMaterialMultiProxyMaster.Create(Collection: TCollection);
begin
  inherited Create(Collection);

end;

destructor TgxMaterialMultiProxyMaster.Destroy;
begin
  MasterObject := nil;
  inherited Destroy;
end;

procedure TgxMaterialMultiProxyMaster.Assign(Source: TPersistent);
begin
  if Source is TgxMaterialMultiProxyMaster then
  begin
    FMasterObject := TgxMaterialMultiProxyMaster(Source).FMasterObject;
    FTempLibMaterialName := TgxMaterialMultiProxyMaster(Source).FTempLibMaterialName;
    FDistanceMin2 := TgxMaterialMultiProxyMaster(Source).FDistanceMin2;
    FDistanceMax2 := TgxMaterialMultiProxyMaster(Source).FDistanceMax2;
    NotifyChange;
  end
  else
    inherited;
end;

function TgxMaterialMultiProxyMaster.OwnerObject: TgxMaterialMultiProxy;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TgxMaterialMultiProxy(TgxMaterialMultiProxyMasters(Collection).GetOwner);
end;

procedure TgxMaterialMultiProxyMaster.NotifyChange;
begin
  TgxMaterialMultiProxyMasters(Collection).NotifyChange;
end;

function TgxMaterialMultiProxyMaster.GetDisplayName: string;
begin
  if MasterObject <> nil then
    Result := MasterObject.Name
  else
    Result := '???';
  Result := Result + Format(' [%.2f; %.2f[', [DistanceMin, DistanceMax]);
end;

procedure TgxMaterialMultiProxyMaster.SetMasterObject(const Val: TgxBaseSceneObject);
begin
  if FMasterObject <> Val then
  begin
    if Assigned(FMasterObject) then
      FMasterObject.RemoveFreeNotification(OwnerObject);
    FMasterObject := Val;
    if Assigned(FMasterObject) then
      FMasterObject.FreeNotification(OwnerObject);
    NotifyChange;
  end;
end;

procedure TgxMaterialMultiProxyMaster.SetDistanceMin(const Val: Single);
var
  tmp: Single;
begin
  tmp := Sqr(Val);
  if FDistanceMin2 <> tmp then
  begin
    FDistanceMin2 := tmp;
    NotifyChange;
  end;
end;

procedure TgxMaterialMultiProxyMaster.SetDistanceMax(const Val: Single);
var
  tmp: Single;
begin
  tmp := Sqr(Val);
  if FDistanceMax2 <> tmp then
  begin
    FDistanceMax2 := tmp;
    NotifyChange;
  end;
end;

function TgxMaterialMultiProxyMaster.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  if OwnerObject = nil then
    Result := nil
  else
    Result := OwnerObject.FMaterialLibrary;
end;

function TgxMaterialMultiProxyMaster.GetDistanceMax: Single;
begin
  Result := sqrt(FDistanceMax2);
end;

function TgxMaterialMultiProxyMaster.GetDistanceMin: Single;
begin
  Result := sqrt(FDistanceMin2);
end;

procedure TgxMaterialMultiProxyMaster.SetMasterLibMaterialName(
  const Value: TgxLibMaterialName);
begin
  if OwnerObject.FMaterialLibrary = nil then
  begin
    FTempLibMaterialName := Value;
    if not (csLoading in OwnerObject.ComponentState) then
      raise ETexture.Create(strErrorEx + strMatLibNotDefined);
  end
  else
  begin
    FMasterLibMaterial := OwnerObject.FMaterialLibrary.LibMaterialByName(Value);
    FTempLibMaterialName := '';
  end;
end;

function TgxMaterialMultiProxyMaster.GetMasterLibMaterialName: TgxLibMaterialName;
begin
  Result := OwnerObject.FMaterialLibrary.GetNameOfLibMaterial(FMasterLibMaterial);
  if Result = '' then
    Result := FTempLibMaterialName;
end;


// ------------------
// ------------------ TgxMaterialMultiProxyMasters ------------------
// ------------------

constructor TgxMaterialMultiProxyMasters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TgxMaterialMultiProxyMaster);
end;

procedure TgxMaterialMultiProxyMasters.SetItems(index: Integer;
  const Val: TgxMaterialMultiProxyMaster);
begin
  inherited Items[index] := Val;
end;

function TgxMaterialMultiProxyMasters.GetItems(index: Integer): TgxMaterialMultiProxyMaster;
begin
  Result := TgxMaterialMultiProxyMaster(inherited Items[index]);
end;

procedure TgxMaterialMultiProxyMasters.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

function TgxMaterialMultiProxyMasters.Add: TgxMaterialMultiProxyMaster;
begin
  Result := (inherited Add) as TgxMaterialMultiProxyMaster;
end;

function TgxMaterialMultiProxyMasters.Add(Master: TgxBaseSceneObject;
  DistanceMin, DistanceMax: Single): TgxMaterialMultiProxyMaster;
begin
  BeginUpdate;
  Result := (inherited Add) as TgxMaterialMultiProxyMaster;
  Result.MasterObject := Master;
  Result.DistanceMin := DistanceMin;
  Result.DistanceMax := DistanceMax;
  EndUpdate;
end;

procedure TgxMaterialMultiProxyMasters.Notification(AComponent: TComponent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      if FMasterObject = AComponent then
        FMasterObject := nil;
end;

procedure TgxMaterialMultiProxyMasters.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and (GetOwner is TgxUpdateAbleComponent) then
    TgxUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

procedure TgxMaterialMultiProxyMasters.EndUpdate;
begin
  inherited EndUpdate;
  // Workaround for a bug in VCL's EndUpdate
  if UpdateCount = 0 then
    NotifyChange;
end;


function TgxMaterialMultiProxyMasters.Add(Master: TgxBaseSceneObject;
  MasterLibMaterial: TgxLibMaterial;
  DistanceMin, DistanceMax: Single): TgxMaterialMultiProxyMaster;
begin
  BeginUpdate;
  Result := (inherited Add) as TgxMaterialMultiProxyMaster;
  Result.MasterObject := Master;
  Result.FMasterLibMaterial := MasterLibMaterial;
  Result.DistanceMin := DistanceMin;
  Result.DistanceMax := DistanceMax;
  EndUpdate;
end;

// ------------------
// ------------------ TgxMaterialMultiProxy ------------------
// ------------------

constructor TgxMaterialMultiProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FMasterObjects := TgxMaterialMultiProxyMasters.Create(Self);
end;

destructor TgxMaterialMultiProxy.Destroy;
begin
  inherited Destroy;
  FMasterObjects.Free;
end;

procedure TgxMaterialMultiProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    FMasterObjects.Notification(AComponent);
  end;
  inherited;
end;

procedure TgxMaterialMultiProxy.SetMasterObjects(const Val: TgxMaterialMultiProxyMasters);
begin
  FMasterObjects.Assign(Val);
  StructureChanged;
end;

procedure TgxMaterialMultiProxy.Assign(Source: TPersistent);
begin
  if Source is TgxMaterialMultiProxy then
    MasterObjects := TgxMaterialMultiProxy(Source).MasterObjects;
  inherited;
end;

procedure TgxMaterialMultiProxy.DoRender(var rci: TgxRenderContextInfo;
  renderSelf, renderChildren: Boolean);
var
  I:  Integer;
  oldProxySubObject: Boolean;
  mpMaster: TgxMaterialMultiProxyMaster;
  d2: Single;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    d2 := VectorDistance2(rci.cameraPosition, AbsolutePosition);
    for I := 0 to MasterObjects.Count - 1 do
    begin
      mpMaster := MasterObjects[I];
      if (mpMaster.MasterObject <> nil) and (d2 >= mpMaster.FDistanceMin2) and
         (d2 < mpMaster.FDistanceMax2) then
      begin
        oldProxySubObject := rci.proxySubObject;
        rci.proxySubObject := True;
        with rci.PipelineTransformation do
          SetModelMatrix(MatrixMultiply(mpMaster.MasterObject.Matrix^, ModelMatrix^));
        if (mpMaster.MasterObject is TgxCustomSceneObject) and (FMaterialLibrary <> nil) then
        begin
          TgxCustomSceneObject(mpMaster.MasterObject).Material.QuickAssignMaterial(
            FMaterialLibrary, mpMaster.FMasterLibMaterial);
        end;
        mpMaster.MasterObject.DoRender(rci, renderSelf, (mpMaster.MasterObject.Count > 0));
        rci.proxySubObject := oldProxySubObject;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if renderChildren and (Count > 0) then
      Self.RenderChildren(0, Count - 1, rci);
    //      if MasterGotEffects then
    //         FMasterObject.Effects.RenderPostEffects(Scene.CurrentBuffer, rci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

function TgxMaterialMultiProxy.PrimaryMaster: TgxBaseSceneObject;
begin
  if MasterObjects.Count > 0 then
    Result := MasterObjects[0].MasterObject
  else
    Result := nil;
end;

function TgxMaterialMultiProxy.AxisAlignedDimensionsUnscaled: TVector4f;
var
  Master: TgxBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
    Result := Master.AxisAlignedDimensionsUnscaled
  else
    Result := inherited AxisAlignedDimensionsUnscaled;
end;

function TgxMaterialMultiProxy.RayCastIntersect(const rayStart, rayVector: TVector4f;
  intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil): Boolean;
var
  localRayStart, localRayVector: TVector4f;
  Master: TgxBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
  begin
    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, Master.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, Master.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := Master.RayCastIntersect(localRayStart, localRayVector,
      intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^, Master.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^, Master.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;
  end
  else
    Result := False;
end;

function TgxMaterialMultiProxy.GenerateSilhouette(
  const silhouetteParameters: TgxSilhouetteParameters): TgxSilhouette;
var
  Master: TgxBaseSceneObject;
begin
  Master := PrimaryMaster;
  if Assigned(Master) then
    Result := Master.GenerateSilhouette(silhouetteParameters)
  else
    Result := nil;
end;

procedure TgxMaterialMultiProxy.SetMaterialLibrary(
  const Value: TgxMaterialLibrary);
var
  I: Integer;
begin
  if FMaterialLibrary <> Value then
  begin
    if FMaterialLibrary <> nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
    FMaterialLibrary := Value;

    if FMaterialLibrary <> nil then
    begin
      FMaterialLibrary.FreeNotification(Self);
      if FMasterObjects.Count <> 0 then
        for I := 0 to FMasterObjects.Count - 1 do
          with FMasterObjects.GetItems(I) do
          begin
            if FTempLibMaterialName <> '' then
              SetMasterLibMaterialName(FTempLibMaterialName);
          end;
    end
    else
    begin
      if FMasterObjects.Count <> 0 then
        for I := 0 to FMasterObjects.Count - 1 do
          FMasterObjects.GetItems(I).FTempLibMaterialName := '';
    end;
  end;
end;


//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

  RegisterClasses([TgxMaterialMultiProxyMaster, TgxMaterialMultiProxyMasters,
                   TgxMaterialMultiProxy]);

end.

