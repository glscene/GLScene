//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Portal;

(*
  Portal Rendering support.
  The portal structures are subclasses of the Mesh structures, with a "sector"
  being assimilated to a "MeshObject" and sector polygons to facegroups.
*)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.VectorFileObjects,
  GXS.Scene,
  GXS.Material,
  GXS.RenderContextInfo;

type

  (* A mesh object list that handles portal rendering.
    The items are treated as being sectors. *)
  TgxPortalMeshObjectList = class(TgxMeshObjectList)
  public
    constructor CreateOwned(AOwner: TgxBaseMesh);
    destructor Destroy; override;
    procedure BuildList(var mrci: TgxRenderContextInfo); override;
  end;

  // A portal renderer sector.
  TgxSectorMeshObject = class(TgxMorphableMeshObject)
  private
    FRenderDone: Boolean;
  public
    constructor CreateOwned(AOwner: TgxMeshObjectList);
    destructor Destroy; override;
    procedure BuildList(var mrci: TgxRenderContextInfo); override;
    procedure Prepare; override;
    property RenderDone: Boolean read FRenderDone write FRenderDone;
  end;

  (* A portal polygon.
    This is the base class for portal polygons, the TFGPortalPolygon class
    implements the portal. *)
  TFGPolygon = class(TFGVertexNormalTexIndexList)
  public
    constructor CreateOwned(AOwner: TgxFaceGroups); override;
    destructor Destroy; override;
    procedure Prepare; override;
  end;

  (* A portal polygon.
    This is the base class for portal polygons, the TFGPortalPolygon class
    implements the portal. *)
  TFGPortalPolygon = class(TFGPolygon)
  private
    FDestinationSectorIndex: Integer;
    FCenter, FNormal: TAffineVector;
    FRadius: Single;
  public
    constructor CreateOwned(AOwner: TgxFaceGroups); override;
    destructor Destroy; override;
    procedure BuildList(var mrci: TgxRenderContextInfo); override;
    procedure Prepare; override;
    property DestinationSectorIndex: Integer read FDestinationSectorIndex write FDestinationSectorIndex;
  end;

  // Portal Renderer class.
  TgxPortal = class(TgxBaseMesh)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MaterialLibrary;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TgxPortalMeshObjectList ------------------
// ------------------

constructor TgxPortalMeshObjectList.CreateOwned(AOwner: TgxBaseMesh);
begin
  inherited CreateOwned(AOwner);
end;

destructor TgxPortalMeshObjectList.Destroy;
begin
  inherited;
end;

procedure TgxPortalMeshObjectList.BuildList(var mrci: TgxRenderContextInfo);
var
  i: Integer;
  startSector: TgxMeshObject;
begin
  for i := 0 to Count - 1 do
    with TgxSectorMeshObject(Items[i]) do
      if InheritsFrom(TgxSectorMeshObject) then
        RenderDone := False;
  startSector := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].PointInObject(PAffineVector(@mrci.cameraPosition)^) then
    begin
      startSector := Items[i];
      Break;
    end;
  end;
  if startSector <> nil then
    startSector.BuildList(mrci)
  else
    for i := 0 to Count - 1 do
      Items[i].BuildList(mrci);
end;

// ------------------
// ------------------ TgxSectorMeshObject ------------------
// ------------------

constructor TgxSectorMeshObject.CreateOwned(AOwner: TgxMeshObjectList);
begin
  inherited;
  Mode := momFaceGroups;
end;

destructor TgxSectorMeshObject.Destroy;
begin
  inherited;
end;

procedure TgxSectorMeshObject.BuildList(var mrci: TgxRenderContextInfo);
var
  i: Integer;
  libMat: TgxLibMaterial;
begin
  if not RenderDone then
  begin
    RenderDone := True;
    // single pass : portals/polygons were sorted earlier
    if Assigned(mrci.MaterialLibrary) then
    begin
      for i := 0 to FaceGroups.Count - 1 do
        with FaceGroups[i] do
        begin
          if Length(MaterialName) > 0 then
          begin
            libMat := TgxMaterialLibrary(mrci.MaterialLibrary).Materials.GetLibMaterialByName(MaterialName);
            if Assigned(libMat) then
            begin
              libMat.Apply(mrci);
              repeat
                BuildList(mrci);
              until not libMat.UnApply(mrci);
            end
            else
              BuildList(mrci);
          end
          else
            BuildList(mrci);
        end;
    end
    else
      for i := 0 to FaceGroups.Count - 1 do
        FaceGroups[i].BuildList(mrci);
  end;
end;

procedure TgxSectorMeshObject.Prepare;
var
  i: Integer;
begin
  for i := 0 to FaceGroups.Count - 1 do
    TFGPolygon(FaceGroups[i]).Prepare;
  FaceGroups.SortByMaterial; // this brings portals first
end;

// ------------------
// ------------------ TFGPolygon ------------------
// ------------------

constructor TFGPolygon.CreateOwned(AOwner: TgxFaceGroups);
begin
  inherited;
  Mode := fgmmTriangleFan;
end;

destructor TFGPolygon.Destroy;
begin
  inherited;
end;

procedure TFGPolygon.Prepare;
begin
  // nothing, ain't no portal !
end;

// ------------------
// ------------------ TFGPortalPolygon ------------------
// ------------------

constructor TFGPortalPolygon.CreateOwned(AOwner: TgxFaceGroups);
begin
  inherited;
end;

destructor TFGPortalPolygon.Destroy;
begin
  inherited;
end;

procedure TFGPortalPolygon.BuildList(var mrci: TgxRenderContextInfo);
var
  dir: TAffineVector;
begin
  if FDestinationSectorIndex >= 0 then
  begin
    VectorSubtract(FCenter, PAffineVector(@mrci.rcci.origin)^, dir);
    if (VectorDotProduct(FNormal, dir) <= 0) and (not IsVolumeClipped(FCenter, FRadius, mrci.rcci.frustum)) then
    begin
      Owner.Owner.Owner.Items[FDestinationSectorIndex].BuildList(mrci);
    end
  end;
end;

procedure TFGPortalPolygon.Prepare;
var
  min, max: TAffineVector;
begin
  GetExtents(min, max);
  FNormal := GetNormal;
  VectorAdd(min, max, FCenter);
  ScaleVector(FCenter, 0.5);
  FRadius := VectorDistance(min, max) * 0.5;
end;

// ------------------
// ------------------ TgxPortal ------------------
// ------------------

constructor TgxPortal.Create(AOwner: TComponent);
begin
  FMeshObjects := TgxPortalMeshObjectList.CreateOwned(Self);
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  UseMeshMaterials := True;
end;

destructor TgxPortal.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TgxPortal, TgxSectorMeshObject, TFGPolygon, TFGPortalPolygon]);

end.
