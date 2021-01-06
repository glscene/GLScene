//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.Portal;

(*
  Portal Rendering support for GLScene.
  The portal structures are subclasses of the Mesh structures, with a "sector"
  being assimilated to a "MeshObject" and sector polygons to facegroups.
*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.PersistentClasses,
  GLS.VectorTypes,
  GLS.VectorFileObjects,
  GLS.Scene,
  GLS.Material,
  GLS.VectorGeometry,
  GLS.RenderContextInfo;

type

  (* A mesh object list that handles portal rendering.
    The items are treated as being sectors. *)
  TGLPortalMeshObjectList = class(TGLMeshObjectList)
  public
    constructor CreateOwned(AOwner: TGLBaseMesh);
    destructor Destroy; override;
    procedure BuildList(var mrci: TGLRenderContextInfo); override;
  end;

  // A portal renderer sector.
  TGLSectorMeshObject = class(TGLMorphableMeshObject)
  private
    FRenderDone: Boolean;
  public
    constructor CreateOwned(AOwner: TGLMeshObjectList);
    destructor Destroy; override;
    procedure BuildList(var mrci: TGLRenderContextInfo); override;
    procedure Prepare; override;
    property RenderDone: Boolean read FRenderDone write FRenderDone;
  end;

  (* A portal polygon.
    This is the base class for portal polygons, the TFGPortalPolygon class
    implements the portal. *)
  TFGPolygon = class(TFGVertexNormalTexIndexList)
  public
    constructor CreateOwned(AOwner: TGLFaceGroups); override;
    destructor Destroy; override;
    procedure Prepare; override;
  end;

  (* A portal polygon. This is the base class for portal polygons,
    the TFGPortalPolygon class implements the portal. *)
  TFGPortalPolygon = class(TFGPolygon)
  private
    FDestinationSectorIndex: Integer;
    FCenter, FNormal: TAffineVector;
    FRadius: Single;
  public
    constructor CreateOwned(AOwner: TGLFaceGroups); override;
    destructor Destroy; override;
    procedure BuildList(var mrci: TGLRenderContextInfo); override;
    procedure Prepare; override;
    property DestinationSectorIndex: Integer read FDestinationSectorIndex
      write FDestinationSectorIndex;
  end;

  // Portal Renderer class.
  TGLPortal = class(TGLBaseMesh)
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
// ------------------ TGLPortalMeshObjectList ------------------
// ------------------

constructor TGLPortalMeshObjectList.CreateOwned(AOwner: TGLBaseMesh);
begin
  inherited CreateOwned(AOwner);
end;

destructor TGLPortalMeshObjectList.Destroy;
begin
  inherited;
end;

procedure TGLPortalMeshObjectList.BuildList(var mrci: TGLRenderContextInfo);
var
  i: Integer;
  startSector: TMeshObject;
begin
  for i := 0 to Count - 1 do
    with TGLSectorMeshObject(Items[i]) do
      if InheritsFrom(TGLSectorMeshObject) then
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
// ------------------ TGLSectorMeshObject ------------------
// ------------------

constructor TGLSectorMeshObject.CreateOwned(AOwner: TGLMeshObjectList);
begin
  inherited;
  Mode := momFaceGroups;
end;

destructor TGLSectorMeshObject.Destroy;
begin
  inherited;
end;

procedure TGLSectorMeshObject.BuildList(var mrci: TGLRenderContextInfo);
var
  i: Integer;
  libMat: TGLLibMaterial;
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
            libMat := TGLMaterialLibrary(mrci.MaterialLibrary)
              .Materials.GetLibMaterialByName(MaterialName);
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

procedure TGLSectorMeshObject.Prepare;
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

constructor TFGPolygon.CreateOwned(AOwner: TGLFaceGroups);
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

constructor TFGPortalPolygon.CreateOwned(AOwner: TGLFaceGroups);
begin
  inherited;
end;

destructor TFGPortalPolygon.Destroy;
begin
  inherited;
end;

procedure TFGPortalPolygon.BuildList(var mrci: TGLRenderContextInfo);
var
  dir: TAffineVector;
begin
  if FDestinationSectorIndex >= 0 then
  begin
    VectorSubtract(FCenter, PAffineVector(@mrci.rcci.origin)^, dir);
    if (VectorDotProduct(FNormal, dir) <= 0) and
      (not IsVolumeClipped(FCenter, FRadius, mrci.rcci.frustum)) then
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
// ------------------ TGLPortal ------------------
// ------------------

constructor TGLPortal.Create(AOwner: TComponent);
begin
  FMeshObjects := TGLPortalMeshObjectList.CreateOwned(Self);
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  UseMeshMaterials := True;
end;

destructor TGLPortal.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

// class registrations
RegisterClasses([TGLPortal, TGLSectorMeshObject, TFGPolygon, TFGPortalPolygon]);

end.
