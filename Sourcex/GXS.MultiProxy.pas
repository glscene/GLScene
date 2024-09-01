//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.MultiProxy;

(* Implements a multi-proxy objects, useful for discreet LOD *)

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,

  GXS.PersistentClasses,
  GXS.Context,
  GXS.Scene,
  GXS.VectorGeometry,
  GXS.Silhouette,
  GXS.RenderContextInfo,
  GXS.BaseClasses,
  GXS.VectorTypes;

type

  TgxMultiProxy = class;

  // MasterObject description for a MultiProxy object.
  TgxMultiProxyMaster = class(TCollectionItem)
  private
    FMasterObject: TgxBaseSceneObject;
    FDistanceMin, FDistanceMin2: Single;
    FDistanceMax, FDistanceMax2: Single;
    FVisible: Boolean;
  protected
    function GetDisplayName: String; override;
    procedure SetMasterObject(const val: TgxBaseSceneObject);
    procedure SetDistanceMin(const val: Single);
    procedure SetDistanceMax(const val: Single);
    procedure SetVisible(const val: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function OwnerObject: TgxMultiProxy;
    procedure NotifyChange;
  published
    // Specifies the Master object which will be proxy'ed.
    property MasterObject: TgxBaseSceneObject read FMasterObject write SetMasterObject;
    // Minimum visibility distance (inclusive).
    property DistanceMin: Single read FDistanceMin write SetDistanceMin;
    // Maximum visibility distance (exclusive).
    property DistanceMax: Single read FDistanceMax write SetDistanceMax;
    (* Determines if the master object can be visible (proxy'ed).
      Note: the master object's distance also has to be within DistanceMin
      and DistanceMax. *)
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  // Collection of TgxMultiProxyMaster.
  TgxMultiProxyMasters = class(TOwnedCollection)
  protected
    procedure SetItems(index: Integer; const val: TgxMultiProxyMaster);
    function GetItems(index: Integer): TgxMultiProxyMaster;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TgxMultiProxyMaster; overload;
    function Add(master: TgxBaseSceneObject; DistanceMin, DistanceMax: Single): TgxMultiProxyMaster; overload;
    property Items[index: Integer]: TgxMultiProxyMaster read GetItems write SetItems; default;
    procedure Notification(AComponent: TComponent);
    procedure NotifyChange;
    procedure EndUpdate; override;
  end;

  (* Multiple Proxy object.
    This proxy has multiple master objects, which are individually made visible
    depending on a distance to the camera criterion. It can be used to implement
    discreet level of detail directly for static objects, or objects that
    go through cyclic animation.
    For dimensionsn raycasting and silhouette purposes, the first master is used
    (item zero in the MasterObjects collection). *)
  TgxMultiProxy = class(TgxSceneObject)
  private
    FMasterObjects: TgxMultiProxyMasters;
    FRendering: Boolean; // internal use (loop protection)
  protected
    procedure SetMasterObjects(const val: TgxMultiProxyMasters);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function PrimaryMaster: TgxBaseSceneObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DoRender(var rci: TgxRenderContextInfo; renderSelf, renderChildren: Boolean); override;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    function RayCastIntersect(const rayStart, rayVector: TVector4f; intersectPoint: PVector4f = nil; intersectNormal: PVector4f = nil)
      : Boolean; override;
    function GenerateSilhouette(const silhouetteParameters: TgxSilhouetteParameters): TgxSilhouette; override;
  published
    property MasterObjects: TgxMultiProxyMasters read FMasterObjects write SetMasterObjects;
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
  end;

// -------------------------------------------------------------
implementation
// -------------------------------------------------------------

// ------------------
// ------------------ TgxMultiProxyMaster ------------------
// ------------------

constructor TgxMultiProxyMaster.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
end;

destructor TgxMultiProxyMaster.Destroy;
begin
  MasterObject := nil;
  inherited Destroy;
end;

procedure TgxMultiProxyMaster.Assign(Source: TPersistent);
begin
  if Source is TgxMultiProxyMaster then
  begin
    MasterObject := TgxMultiProxyMaster(Source).MasterObject;
    FDistanceMin := TgxMultiProxyMaster(Source).FDistanceMin;
    FDistanceMin2 := TgxMultiProxyMaster(Source).FDistanceMin2;
    FDistanceMax := TgxMultiProxyMaster(Source).FDistanceMax;
    FDistanceMax2 := TgxMultiProxyMaster(Source).FDistanceMax2;
    FVisible := TgxMultiProxyMaster(Source).FVisible;
    NotifyChange;
  end
  else
    inherited;
end;

function TgxMultiProxyMaster.OwnerObject: TgxMultiProxy;
begin
  Result := TgxMultiProxy(TgxMultiProxyMasters(Collection).GetOwner);
end;

procedure TgxMultiProxyMaster.NotifyChange;
begin
  TgxMultiProxyMasters(Collection).NotifyChange;
end;

function TgxMultiProxyMaster.GetDisplayName: String;
begin
  if MasterObject <> nil then
    Result := MasterObject.Name
  else
    Result := '???';
  Result := Result + Format(' [%.2f; %.2f[', [DistanceMin, DistanceMax]);
  if not Visible then
    Result := Result + ' (hidden)';
end;

procedure TgxMultiProxyMaster.SetMasterObject(const val: TgxBaseSceneObject);
begin
  if FMasterObject <> val then
  begin
    if Assigned(FMasterObject) then
      FMasterObject.RemoveFreeNotification(OwnerObject);
    FMasterObject := val;
    if Assigned(FMasterObject) then
      FMasterObject.FreeNotification(OwnerObject);
    NotifyChange;
  end;
end;

procedure TgxMultiProxyMaster.SetDistanceMin(const val: Single);
begin
  if FDistanceMin <> val then
  begin
    FDistanceMin := val;
    FDistanceMin2 := Sqr(val);
    NotifyChange;
  end;
end;

procedure TgxMultiProxyMaster.SetDistanceMax(const val: Single);
begin
  if FDistanceMax <> val then
  begin
    FDistanceMax := val;
    FDistanceMax2 := Sqr(val);
    NotifyChange;
  end;
end;

procedure TgxMultiProxyMaster.SetVisible(const val: Boolean);
begin
  if FVisible <> val then
  begin
    FVisible := val;
    NotifyChange;
  end;
end;

// ------------------
// ------------------ TgxMultiProxyMasters ------------------
// ------------------

constructor TgxMultiProxyMasters.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TgxMultiProxyMaster)
end;

procedure TgxMultiProxyMasters.SetItems(index: Integer; const val: TgxMultiProxyMaster);
begin
  inherited Items[index] := val;
end;

function TgxMultiProxyMasters.GetItems(index: Integer): TgxMultiProxyMaster;
begin
  Result := TgxMultiProxyMaster(inherited Items[index]);
end;

procedure TgxMultiProxyMasters.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

function TgxMultiProxyMasters.Add: TgxMultiProxyMaster;
begin
  Result := (inherited Add) as TgxMultiProxyMaster;
end;

function TgxMultiProxyMasters.Add(master: TgxBaseSceneObject; DistanceMin, DistanceMax: Single): TgxMultiProxyMaster;
begin
  BeginUpdate;
  Result := (inherited Add) as TgxMultiProxyMaster;
  Result.MasterObject := master;
  Result.DistanceMin := DistanceMin;
  Result.DistanceMax := DistanceMax;
  EndUpdate;
end;

procedure TgxMultiProxyMasters.Notification(AComponent: TComponent);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if FMasterObject = AComponent then
        FMasterObject := nil;
end;

procedure TgxMultiProxyMasters.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and (GetOwner is TgxUpdateAbleComponent) then
    TgxUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

procedure TgxMultiProxyMasters.EndUpdate;
begin
  inherited EndUpdate;
  // Workaround for a bug in VCL's EndUpdate
  if UpdateCount = 0 then
    NotifyChange;
end;

// ------------------
// ------------------ TgxMultiProxy ------------------
// ------------------

constructor TgxMultiProxy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FMasterObjects := TgxMultiProxyMasters.Create(Self);
end;

destructor TgxMultiProxy.Destroy;
begin
  inherited Destroy;
  FMasterObjects.Free;
end;

procedure TgxMultiProxy.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
    FMasterObjects.Notification(AComponent);
  inherited;
end;

procedure TgxMultiProxy.SetMasterObjects(const val: TgxMultiProxyMasters);
begin
  FMasterObjects.Assign(val);
  StructureChanged;
end;

procedure TgxMultiProxy.Assign(Source: TPersistent);
begin
  if Source is TgxMultiProxy then
  begin
    MasterObjects := TgxMultiProxy(Source).MasterObjects;
  end;
  inherited;
end;

procedure TgxMultiProxy.DoRender(var rci: TgxRenderContextInfo; renderSelf, renderChildren: Boolean);
var
  i: Integer;
  oldProxySubObject: Boolean;
  mpMaster: TgxMultiProxyMaster;
  master: TgxBaseSceneObject;
  d2: Single;
begin
  if FRendering then
    Exit;
  FRendering := True;
  try
    d2 := VectorDistance2(rci.cameraPosition, AbsolutePosition);
    for i := 0 to MasterObjects.Count - 1 do
    begin
      mpMaster := MasterObjects[i];
      if mpMaster.Visible then
      begin
        master := mpMaster.MasterObject;
        if (master <> nil) and (d2 >= mpMaster.FDistanceMin2) and (d2 < mpMaster.FDistanceMax2) then
        begin
          oldProxySubObject := rci.proxySubObject;
          rci.proxySubObject := True;
          glMultMatrixf(PGLFloat(master.Matrix));
          master.DoRender(rci, renderSelf, (master.Count > 0));
          rci.proxySubObject := oldProxySubObject;
        end;
      end;
    end;
    // now render self stuff (our children, our effects, etc.)
    if renderChildren and (Count > 0) then
      Self.renderChildren(0, Count - 1, rci);
    // if masterGotEffects then
    // FMasterObject.Effects.RenderPostEffects(Scene.CurrentBuffer, rci);
  finally
    FRendering := False;
  end;
  ClearStructureChanged;
end;

function TgxMultiProxy.PrimaryMaster: TgxBaseSceneObject;
begin
  if MasterObjects.Count > 0 then
    Result := MasterObjects[0].MasterObject
  else
    Result := nil;
end;

function TgxMultiProxy.AxisAlignedDimensionsUnscaled: TVector4f;
var
  master: TgxBaseSceneObject;
begin
  master := PrimaryMaster;
  if Assigned(master) then
  begin
    Result := master.AxisAlignedDimensionsUnscaled;
  end
  else
    Result := inherited AxisAlignedDimensionsUnscaled;
end;

function TgxMultiProxy.RayCastIntersect(const rayStart, rayVector: TVector4f; intersectPoint: PVector4f = nil;
  intersectNormal: PVector4f = nil): Boolean;
var
  localRayStart, localRayVector: TVector4f;
  master: TgxBaseSceneObject;
begin
  master := PrimaryMaster;
  if Assigned(master) then
  begin
    SetVector(localRayStart, AbsoluteToLocal(rayStart));
    SetVector(localRayStart, master.LocalToAbsolute(localRayStart));
    SetVector(localRayVector, AbsoluteToLocal(rayVector));
    SetVector(localRayVector, master.LocalToAbsolute(localRayVector));
    NormalizeVector(localRayVector);

    Result := master.RayCastIntersect(localRayStart, localRayVector, intersectPoint, intersectNormal);
    if Result then
    begin
      if Assigned(intersectPoint) then
      begin
        SetVector(intersectPoint^, master.AbsoluteToLocal(intersectPoint^));
        SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
      end;
      if Assigned(intersectNormal) then
      begin
        SetVector(intersectNormal^, master.AbsoluteToLocal(intersectNormal^));
        SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      end;
    end;
  end
  else
    Result := False;
end;

function TgxMultiProxy.GenerateSilhouette(const silhouetteParameters: TgxSilhouetteParameters): TgxSilhouette;
var
  master: TgxBaseSceneObject;
begin
  master := PrimaryMaster;
  if Assigned(master) then
    Result := master.GenerateSilhouette(silhouetteParameters)
  else
    Result := nil;
end;

// -------------------------------------------------------------
initialization
// -------------------------------------------------------------

RegisterClasses([TgxMultiProxy]);

end.
